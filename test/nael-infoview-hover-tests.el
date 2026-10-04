;;; nael-infoview-hover-tests.el --- Tests for nael-infoview-hover  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see nael/LICENSE.GPL3.

;;; Commentary:

;; Tests for information on subterms in the infoview that need no Lean
;; server: RPC calls are answered by stubs.

;;; Code:

(require 'ert)
(require 'nael-infoview)
(require 'nael-infoview-tests)

;; As in Doom Emacs, whose `K' runs these functions.
(defvar +lookup-documentation-functions nil)

(defvar nael-infoview-hover-test-calls nil
  "RPC calls made by the code under test, latest first.
Each is a plist of the method, the reference asked about, and the keys.")

(defun nael-infoview-hover-test-info (ref)
  "Return the hover information that the stub server has for REF."
  (pcase (plist-get ref :__rpcref)
    ("1" (list :exprExplicit (nael-render-test-code '("n" "11"))
               :type (nael-render-test-code '("Nat" "12"))
               :doc "The `n` of the test."))
    ("12" (list :exprExplicit (nael-render-test-code '("Nat" "13"))
                :type (nael-render-test-code "Type")
                :doc "The natural numbers."))
    (_ (list :exprExplicit (nael-render-test-code "0")
             :type (nael-render-test-code '("Nat" "12"))))))

(defvar nael-infoview-hover-test-error nil
  "If non-nil, the error code with which the stub server fails calls.")

(defvar nael-infoview-hover-test-deferred nil
  "If non-nil, the stub server keeps answers here instead of answering.
Each is a function that delivers it.")

(defmacro nael-infoview-hover-test-with-server (&rest body)
  "Run BODY with RPC calls answered at once by a stub server."
  (declare (indent 0) (debug t))
  `(let ((nael-infoview-hover-test-calls nil)
         (nael-infoview-hover-test-error nil)
         (nael-infoview-hover-test-deferred nil)
         (nael-infoview--pages nil))
     (cl-letf (((symbol-function 'nael-rpc-handle-live-p) #'always)
               ((symbol-function 'nael-rpc-info-to-interactive)
                (lambda (_handle info success &rest keys)
                  (push (append (list :method 'info :info info) keys)
                        nael-infoview-hover-test-calls)
                  (if nael-infoview-hover-test-error
                      (funcall (plist-get keys :failure)
                               (list :code nael-infoview-hover-test-error
                                     :message "fake error"))
                    (let* ((result (nael-infoview-hover-test-info info))
                           (answer
                            (lambda ()
                              (when-let* ((owner (plist-get keys :owner)))
                                (setf (nael-rpc-owner-refs owner)
                                      (nael-rpc-collect-refs result
                                                             :__rpcref)))
                              (funcall success result))))
                      (if nael-infoview-hover-test-deferred
                          (push answer nael-infoview-hover-test-deferred)
                        (funcall answer)))))))
       (unwind-protect (progn ,@body)
         (dolist (name (list nael-infoview-help-buffer-name "*Help*"))
           (when-let* ((help (get-buffer name)))
             (kill-buffer help)))
         (when-let* ((source (get-buffer "Source.lean")))
           (kill-buffer source))))))

(defun nael-infoview-hover-test-handle ()
  "Return an RPC handle for line 2, column 2 of a buffer Source.lean."
  (with-current-buffer (get-buffer-create "Source.lean")
    (erase-buffer)
    (insert "example : n = 0 := by\n  rfl\n")
    (nael-rpc--handle-create
     :buffer (current-buffer)
     :position '(:textDocument (:uri "file:///Source.lean")
                 :position (:line 1 :character 2)))))

(defmacro nael-infoview-hover-test-with-goal (&rest body)
  "Run BODY in the infoview showing the goal `n = 0', at its start."
  (declare (indent 0) (debug t))
  `(nael-infoview-test-with-options
     (unwind-protect
         (progn
           (nael-infoview--show
            (list :goals (vector (nael-render-test-goal)))
            nil (nael-rpc-make-owner) "x" (nael-infoview-hover-test-handle))
           (with-current-buffer (nael-infoview--buffer)
             (switch-to-buffer (current-buffer))
             (goto-char (point-min))
             ,@body))
       (when-let* ((buffer (nael-infoview--buffer)))
         (kill-buffer buffer)))))

(defun nael-infoview-hover-test-eldoc ()
  "Return what `nael-infoview-eldoc-function' reports at point.
The result is (DOC . ECHO), or nil if it reports nothing."
  (let (report)
    (nael-infoview-eldoc-function
     (lambda (doc &rest keys)
       (setq report (cons (substring-no-properties doc)
                          (substring-no-properties
                           (plist-get keys :echo))))))
    report))

;;;; ElDoc:

(ert-deftest nael-infoview-hover-test-eldoc ()
  "ElDoc shows the subterm at point and its type, then its documentation."
  (nael-infoview-hover-test-with-server
    (nael-infoview-hover-test-with-goal
      (should (memq #'nael-infoview-eldoc-function
                    eldoc-documentation-functions))
      (should (eq (and eldoc-mode t) global-eldoc-mode))
      (should (nael-rpc-handle-p nael-infoview--handle))
      (nael-infoview-test-goto "n = 0")
      (should (equal (nael-infoview-hover-test-eldoc)
                     '("n : Nat\n\nThe `n` of the test." . "n : Nat")))
      (let ((call (car nael-infoview-hover-test-calls)))
        (should (equal (plist-get call :info) '(:__rpcref "1")))
        (should (eq (plist-get call :slot) 'infoview-eldoc))
        ;; The answer is text; its references are released at once.
        (should (nael-rpc-owner-released (plist-get call :owner))))
      ;; Nothing on the line but the subterm.
      (nael-infoview-test-goto " = 0")
      (should-not (nael-infoview-hover-test-eldoc))
      (should-not (nael-infoview-eldoc-function #'ignore)))))

(ert-deftest nael-infoview-hover-test-eldoc-cache ()
  "ElDoc asks about a subterm once, until the goals are shown again."
  (nael-infoview-hover-test-with-server
    (nael-infoview-hover-test-with-goal
      (nael-infoview-test-goto "n = 0")
      (let ((report (nael-infoview-hover-test-eldoc)))
        (should (= (length nael-infoview-hover-test-calls) 1))
        (should (equal (nael-infoview-hover-test-eldoc) report))
        (should (= (length nael-infoview-hover-test-calls) 1))
        (nael-infoview--show (list :goals (vector (nael-render-test-goal)))
                             nil (nael-rpc-make-owner) "x"
                             nael-infoview--handle)
        ;; The old nodes are not kept.
        (should-not nael-infoview--info-cache)
        (nael-infoview-test-goto "n = 0")
        (should (equal (nael-infoview-hover-test-eldoc) report))
        (should (= (length nael-infoview-hover-test-calls) 2))))))

(ert-deftest nael-infoview-hover-test-eldoc-diff ()
  "ElDoc explains how the tactic at point changes the subterm."
  (nael-infoview-hover-test-with-server
    (nael-infoview-test-with-options
      (unwind-protect
          (progn
            (nael-infoview--show
             (list :goals
                   (vector (nael-render-test-goal
                            :type (nael-render-test-code
                                   '("n" "1" "willDelete") " = 0"))))
             nil (nael-rpc-make-owner) "x" (nael-infoview-hover-test-handle))
            (with-current-buffer (nael-infoview--buffer)
              (nael-infoview-test-goto "n = 0")
              (should (string-suffix-p
                       "\n\nThe tactic at point will delete this subterm."
                       (car (nael-infoview-hover-test-eldoc))))))
        (kill-buffer (nael-infoview--buffer))))))

(ert-deftest nael-infoview-hover-test-eldoc-no-handle ()
  "Without a live RPC handle, ElDoc reports nothing."
  (nael-infoview-hover-test-with-server
    (nael-infoview-hover-test-with-goal
      (nael-infoview-test-goto "n = 0")
      (cl-letf (((symbol-function 'nael-rpc-handle-live-p) #'ignore))
        (should-not (nael-infoview-eldoc-function #'ignore)))
      (setq nael-infoview--handle nil)
      (should-not (nael-infoview-eldoc-function #'ignore))
      (should-not nael-infoview-hover-test-calls))))

;;;; Tooltips:

(ert-deftest nael-infoview-hover-test-tooltip ()
  "With `tooltip-mode', the tooltip of a subterm documents it."
  (nael-infoview-hover-test-with-server
    (nael-infoview-hover-test-with-goal
      (nael-infoview-test-goto "n = 0")
      (let ((help-echo (get-char-property (point) 'help-echo))
            (pos (point))
            (under-mouse (nael-render-node-at (point)))
            shown)
        (cl-letf (((symbol-function 'nael-infoview--node-under-mouse)
                   (lambda () under-mouse))
                  (show-help-function (lambda (text) (push text shown))))
          ;; Not in the echo area, where ElDoc shows the subterm at point.
          (let ((tooltip-mode nil))
            (should-not (funcall help-echo nil nil pos))
            (should-not nael-infoview-hover-test-calls))
          (let ((tooltip-mode t))
            ;; Asked for once, while Lean has not answered.
            (setq nael-infoview-hover-test-deferred (list #'ignore))
            (should-not (funcall help-echo nil nil pos))
            (should-not (funcall help-echo nil nil pos))
            (should (= (length nael-infoview-hover-test-calls) 1))
            (should (eq (plist-get (car nael-infoview-hover-test-calls) :slot)
                        'infoview-tooltip))
            ;; Shown when Lean answers, as the mouse is still there.
            (funcall (car nael-infoview-hover-test-deferred))
            (setq nael-infoview-hover-test-deferred nil)
            (should (equal (mapcar #'substring-no-properties shown)
                           '("n : Nat\n\nThe `n` of the test.")))
            ;; Then known.
            (should (equal (substring-no-properties
                            (funcall help-echo nil nil pos))
                           "n : Nat\n\nThe `n` of the test."))
            ;; ElDoc knows it too.
            (should (equal (nael-infoview-hover-test-eldoc)
                           '("n : Nat\n\nThe `n` of the test." . "n : Nat")))
            (should (= (length nael-infoview-hover-test-calls) 1))
            ;; Not shown if the mouse has moved on.
            (setq shown nil under-mouse nil)
            (nael-infoview--show (list :goals (vector (nael-render-test-goal)))
                                 nil (nael-rpc-make-owner) "x"
                                 nael-infoview--handle)
            (nael-infoview-test-goto "n = 0")
            (should-not (funcall (get-char-property (point) 'help-echo)
                                 nil nil (point)))
            (should (= (length nael-infoview-hover-test-calls) 2))
            (should-not shown)))))))

;;;; Descriptions:

(defun nael-infoview-hover-test-help ()
  "Return the text of the help buffer."
  (with-current-buffer nael-infoview-help-buffer-name
    (buffer-substring-no-properties (point-min) (point-max))))

(defun nael-infoview-hover-test-page-owners ()
  "Return whether the owners of the kept descriptions are released."
  (mapcar (lambda (page)
            (nael-rpc-owner-released (nael-infoview--page-owner page)))
          nael-infoview--pages))

(ert-deftest nael-infoview-hover-test-describe ()
  "A subterm is described in the help buffer."
  (nael-infoview-hover-test-with-server
    (nael-infoview-hover-test-with-goal
      (should-error (nael-infoview-describe) :type 'user-error)
      (nael-infoview-test-goto "n = 0")
      (nael-infoview-describe)
      (should (equal (nael-infoview-hover-test-help)
                     "n : Nat

The `n` of the test.

From the goals at Source.lean:2:2.
"))
      (should (eq (plist-get (car nael-infoview-hover-test-calls) :slot)
                  'infoview-describe))
      (with-current-buffer nael-infoview-help-buffer-name
        ;; The location leads to the source.
        (search-forward "Source.lean")
        (push-button)
        (should (equal (buffer-name) "Source.lean"))
        (should (looking-at "rfl"))))))

(ert-deftest nael-infoview-hover-test-describe-keys ()
  "Subterms are described with RET, mouse-1 and `C-h .'."
  (nael-infoview-hover-test-with-server
    (nael-infoview-hover-test-with-goal
      (nael-infoview-test-goto "n = 0")
      (should (eq (key-binding (kbd "RET")) #'nael-infoview-describe))
      (should (eq (key-binding (kbd "C-h .")) #'nael-infoview-describe))
      (should (eq (key-binding [mouse-2]) #'nael-infoview-describe-at-mouse))
      ;; mouse-1 follows links, see `mouse-on-link-p'.
      (should (eq (key-binding [follow-link] nil t (point)) 'mouse-face))
      (should (get-char-property (point) 'mouse-face))
      (nael-infoview-test-goto " = 0")
      (should-not (eq (key-binding (kbd "RET")) #'nael-infoview-describe))
      (should-not (get-char-property (point) 'mouse-face)))))

(ert-deftest nael-infoview-hover-test-describe-nested ()
  "Subterms of a description are described in turn; history leads back."
  (nael-infoview-hover-test-with-server
    (nael-infoview-hover-test-with-goal
      (nael-infoview-test-goto "n = 0")
      (nael-infoview-describe)
      (with-current-buffer nael-infoview-help-buffer-name
        (goto-char (point-min))
        (search-forward "Nat")
        (backward-char)
        (should (eq (key-binding (kbd "RET")) #'nael-infoview-describe))
        (nael-infoview-describe)
        (should (equal (plist-get (car nael-infoview-hover-test-calls) :info)
                       '(:__rpcref "12")))
        (should (string-prefix-p "Nat : Type\n\nThe natural numbers.\n"
                                 (nael-infoview-hover-test-help)))
        (let ((calls (length nael-infoview-hover-test-calls)))
          (help-go-back)
          (should (string-prefix-p "n : Nat\n" (nael-infoview-hover-test-help)))
          (help-go-forward)
          (should (string-prefix-p "Nat : Type\n"
                                   (nael-infoview-hover-test-help)))
          ;; History shows what was received; it asks the server nothing.
          (should (= (length nael-infoview-hover-test-calls) calls)))
        ;; A page from history still describes its subterms.
        (help-go-back)
        (goto-char (point-min))
        (nael-infoview-describe)
        (should (equal (plist-get (car nael-infoview-hover-test-calls) :info)
                       '(:__rpcref "11")))
        (should (eq major-mode 'nael-infoview-help-mode))
        (should (eq (and eldoc-mode t) global-eldoc-mode))
        ;; Emacs's own help is elsewhere, and leaves this alone.
        (should-not (get-buffer "*Help*"))
        (describe-variable 'nael-infoview-delay)
        (should (string-prefix-p "0 : Nat\n" (nael-infoview-hover-test-help)))
        (should nael-infoview--handle)))))

(ert-deftest nael-infoview-hover-test-describe-release ()
  "Descriptions keep their references while history can lead back."
  (nael-infoview-hover-test-with-server
    (nael-infoview-hover-test-with-goal
      (nael-infoview-test-goto "n = 0")
      (nael-infoview-describe)
      (with-current-buffer nael-infoview-help-buffer-name
        (goto-char (point-min))
        (search-forward "Nat")
        (nael-infoview-describe 1))
      (should (equal (nael-infoview-hover-test-page-owners) '(nil nil)))
      (let ((first (car (last nael-infoview--pages)))
            (second (car nael-infoview--pages)))
        ;; Going back and describing anew drops the way forward.
        (with-current-buffer nael-infoview-help-buffer-name
          (help-go-back))
        ;; Help can still go forward to it.
        (should-not (nael-rpc-owner-released
                     (nael-infoview--page-owner second)))
        (nael-infoview-describe)
        (should (nael-rpc-owner-released (nael-infoview--page-owner second)))
        (should-not (memq second nael-infoview--pages))
        (should-not (nael-rpc-owner-released
                     (nael-infoview--page-owner first)))
        ;; Without the help buffer, nothing leads back.
        (kill-buffer nael-infoview-help-buffer-name)
        (should (nael-rpc-owner-released (nael-infoview--page-owner first)))
        (should-not nael-infoview--pages)
        (nael-infoview-describe)
        (should (equal (nael-infoview-hover-test-page-owners) '(nil)))))))

(ert-deftest nael-infoview-hover-test-return-from-help ()
  "Closing the help window, as Doom Emacs does, returns to the infoview."
  (nael-infoview-hover-test-with-server
    (nael-infoview-test-with-windows
      (nael-infoview--show (list :goals (vector (nael-render-test-goal)))
                           nil (nael-rpc-make-owner) "x"
                           (nael-infoview-hover-test-handle))
      (let ((home nael-infoview--window)
            (main (get-buffer-window "Main")))
        (nael-infoview-test-goto "n = 0")
        (nael-infoview-describe)
        (let ((help (get-buffer-window nael-infoview-help-buffer-name)))
          (should (window-live-p help))
          (should-not (memq help (list home main)))
          ;; Closed while selected: Emacs selects the main window.
          (select-window help)
          (delete-window help)
          (should (eq (selected-window) main))
          (cl-letf (((symbol-function 'old-selected-window)
                     (lambda () help)))
            (nael-infoview--return-from-help nil))
          (should (eq (selected-window) home))
          (should-not (memq #'nael-infoview--return-from-help
                            (default-value
                             'window-selection-change-functions))))
        ;; Closed from elsewhere: no reason to go back.
        (nael-infoview-describe)
        (let ((help (get-buffer-window nael-infoview-help-buffer-name)))
          (select-window main)
          (delete-window help)
          (cl-letf (((symbol-function 'old-selected-window)
                     (lambda () main)))
            (nael-infoview--return-from-help nil))
          (should (eq (selected-window) main)))))))

(ert-deftest nael-infoview-hover-test-describe-dead ()
  "A subterm that Lean no longer knows is reported, not raised."
  (nael-infoview-hover-test-with-server
    (nael-infoview-hover-test-with-goal
      (nael-infoview-test-goto "n = 0")
      (let (messages)
        (cl-letf (((symbol-function 'message)
                   (lambda (&rest args)
                     (push (apply #'format-message args) messages))))
          (setq nael-infoview-hover-test-error -32602)
          (nael-infoview-describe)
          (setq nael-infoview-hover-test-error -32000)
          (nael-infoview-describe))
        (should (string-prefix-p "Lean could not look up this subterm: fake"
                                 (car messages)))
        (should (string-prefix-p "Lean no longer knows this subterm"
                                 (cadr messages)))
        (should-not nael-infoview--pages))
      ;; The session has ended.
      (cl-letf (((symbol-function 'nael-rpc-handle-live-p) #'ignore))
        (should-error (nael-infoview-describe) :type 'user-error)))))

;;;; Definitions:

(defmacro nael-infoview-hover-test-with-definitions (links &rest body)
  "Run BODY with getGoToLocation answered with LINKS, or ERROR if a number.
The links are made with `nael-infoview-hover-test-link'.  The calls
made are recorded in `nael-infoview-hover-test-calls'."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'nael-rpc-call-sync)
              (lambda (_handle method params &rest keys)
                (push (append (list :method method :params params) keys)
                      nael-infoview-hover-test-calls)
                (let ((links ,links))
                  (if (numberp links)
                      (signal 'nael-rpc-error
                              (list method (list :code links
                                                 :message "fake error")))
                    links)))))
     ,@body))

(defun nael-infoview-hover-test-link (file line character)
  "Return a `LocationLink' to LINE and CHARACTER, in UTF-16, of FILE."
  (let ((position (list :line line :character character)))
    (list :targetUri (concat "file://" file)
          :targetRange (list :start '(:line 0 :character 0) :end position)
          :targetSelectionRange (list :start position :end position))))

(ert-deftest nael-infoview-hover-test-definitions ()
  "The definition of a subterm is found with xref."
  (let ((file (make-temp-file "nael-test" nil ".lean"
                              "-- 𝓝\ndef 𝓝𝓝 b : Nat := 1\n")))
    (unwind-protect
        (nael-infoview-hover-test-with-server
          (nael-infoview-hover-test-with-goal
            (nael-infoview-test-goto "n = 0")
            (should (eq (xref-find-backend) 'nael-infoview))
            (let ((identifier (xref-backend-identifier-at-point
                               'nael-infoview)))
              (should (equal identifier "n"))
              (nael-infoview-hover-test-with-definitions
                  ;; After "def 𝓝𝓝 ", which is 7 characters but 9
                  ;; UTF-16 code units.
                  (list (nael-infoview-hover-test-link file 1 9))
                (let* ((items (xref-backend-definitions 'nael-infoview
                                                        identifier))
                       (location (xref-item-location (car items))))
                  (should (= (length items) 1))
                  (should (equal (xref-item-summary (car items))
                                 "def 𝓝𝓝 b : Nat := 1"))
                  (should (equal (xref-file-location-file location) file))
                  (should (= (xref-file-location-line location) 2))
                  (should (= (xref-file-location-column location) 7))
                  (let ((call (car nael-infoview-hover-test-calls)))
                    (should (equal (plist-get call :method)
                                   "Lean.Widget.getGoToLocation"))
                    (should (equal (plist-get call :params)
                                   '(:kind "definition"
                                     :info (:__rpcref "1"))))
                    (should (eq (plist-get call :retry) nil))
                    (should (plist-member call :retry)))
                  ;; Doom Emacs passes the identifier without properties.
                  (should (equal (xref-backend-definitions 'nael-infoview "n")
                                 items))
                  ;; The same from a buffer visiting the file.
                  (let ((visiting (find-file-noselect file)))
                    (unwind-protect
                        (should (equal (xref-backend-definitions
                                        'nael-infoview identifier)
                                       items))
                      (kill-buffer visiting))))))
            ;; Not on a subterm, the backend is someone else's.
            (nael-infoview-test-goto " = 0")
            (should-not (eq (xref-find-backend) 'nael-infoview))))
      (delete-file file))))

(ert-deftest nael-infoview-hover-test-definitions-failure ()
  "A failure to find definitions is reported to the user."
  (nael-infoview-hover-test-with-server
    (nael-infoview-hover-test-with-goal
      (nael-infoview-test-goto "n = 0")
      (nael-infoview-hover-test-with-definitions -32602
        (should (equal (cadr (should-error
                              (xref-backend-definitions 'nael-infoview "n")
                              :type 'user-error))
                       "Lean no longer knows this subterm; \
describe it again from the infoview")))
      (nael-infoview-hover-test-with-definitions []
        (should-not (xref-backend-definitions 'nael-infoview "n"))))))

;;;; Doom Emacs:

(ert-deftest nael-infoview-hover-test-doom-lookup ()
  "Doom Emacs' `K' describes the subterm at point."
  (nael-infoview-hover-test-with-server
    (nael-infoview-hover-test-with-goal
      (should (memq #'nael-infoview--lookup-documentation
                    +lookup-documentation-functions))
      (should-not (default-value '+lookup-documentation-functions))
      (should-not (nael-infoview--lookup-documentation "goal"))
      (should-not (get-buffer nael-infoview-help-buffer-name))
      (nael-infoview-test-goto "n = 0")
      (should (eq (nael-infoview--lookup-documentation "n") 'deferred))
      (should (string-prefix-p "n : Nat\n" (nael-infoview-hover-test-help))))))

(ert-deftest nael-infoview-hover-test-no-doom ()
  "Without Doom Emacs, its hook is left undefined."
  (let ((value +lookup-documentation-functions))
    (makunbound '+lookup-documentation-functions)
    (unwind-protect
        (with-temp-buffer
          (nael-infoview-hover-setup)
          (should-not (boundp '+lookup-documentation-functions)))
      (setq +lookup-documentation-functions value))))

(provide 'nael-infoview-hover-tests)

;;; nael-infoview-hover-tests.el ends here
