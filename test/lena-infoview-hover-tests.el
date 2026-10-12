;;; lena-infoview-hover-tests.el --- Tests for lena-infoview-hover  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see lena/LICENSE.GPL3.

;;; Commentary:

;; Tests for information on subterms in the infoview that need no Lean
;; server: RPC calls are answered by stubs.

;;; Code:

(require 'ert)
(require 'lena-infoview)
(require 'lena-test-helpers)

;; As in Doom Emacs, whose `K' runs these functions.
(defvar +lookup-documentation-functions nil)

(defvar lena-infoview-hover-test-calls nil
  "RPC calls made by the code under test, latest first.
Each is a plist of the method, the reference asked about, and the keys.")

(defun lena-infoview-hover-test-info (ref)
  "Return the hover information that the stub server has for REF."
  (pcase (plist-get ref :__rpcref)
    ("1" (list :exprExplicit (lena-test-code '("n" "11"))
               :type (lena-test-code '("Nat" "12"))
               :doc "The `n` of the test."))
    ("12" (list :exprExplicit (lena-test-code '("Nat" "13"))
                :type (lena-test-code "Type")
                :doc "The natural numbers."))
    (_ (list :exprExplicit (lena-test-code "0")
             :type (lena-test-code '("Nat" "12"))))))

(defvar lena-infoview-hover-test-error nil
  "If non-nil, the error code with which the stub server fails calls.")

(defvar lena-infoview-hover-test-deferred nil
  "If non-nil, the stub server keeps answers here instead of answering.
Each is a function that delivers it.")

(defmacro lena-infoview-hover-test-with-server (&rest body)
  "Run BODY with RPC calls answered at once by a stub server."
  (declare (indent 0) (debug t))
  `(let ((lena-infoview-hover-test-calls nil)
         (lena-infoview-hover-test-error nil)
         (lena-infoview-hover-test-deferred nil)
         (lena-infoview--pages nil))
     (cl-letf (((symbol-function 'lena-rpc-handle-live-p) #'identity)
               ((symbol-function 'lena-rpc-info-to-interactive)
                (lambda (_handle info success &rest keys)
                  (push (append (list :method 'info :info info) keys)
                        lena-infoview-hover-test-calls)
                  (if lena-infoview-hover-test-error
                      (funcall (plist-get keys :failure)
                               (list :code lena-infoview-hover-test-error
                                     :message "fake error"))
                    (let* ((result (lena-infoview-hover-test-info info))
                           (answer
                            (lambda ()
                              (when-let* ((owner (plist-get keys :owner)))
                                (setf (lena-rpc-owner-refs owner)
                                      (lena-rpc-collect-refs result
                                                             :__rpcref)))
                              (funcall success result))))
                      (if lena-infoview-hover-test-deferred
                          (push answer lena-infoview-hover-test-deferred)
                        (funcall answer)))))))
       (unwind-protect (progn ,@body)
         (dolist (name (list lena-infoview-help-buffer-name "*Help*"))
           (when-let* ((help (get-buffer name)))
             (kill-buffer help)))
         (when-let* ((source (get-buffer "Source.lean")))
           (kill-buffer source))))))

(defun lena-infoview-hover-test-handle ()
  "Return an RPC handle for line 2, column 2 of a buffer Source.lean."
  (with-current-buffer (get-buffer-create "Source.lean")
    (erase-buffer)
    (insert "example : n = 0 := by\n  rfl\n")
    (lena-rpc--handle-create
     :buffer (current-buffer)
     :position '(:textDocument (:uri "file:///Source.lean")
                 :position (:line 1 :character 2)))))

(defmacro lena-infoview-hover-test-with-goal (&rest body)
  "Run BODY in the infoview showing the goal `n = 0', at its start."
  (declare (indent 0) (debug t))
  `(lena-test-with-options
     (unwind-protect
         (progn
           (lena-infoview--show
            (list :goals (vector (lena-test-goal)))
            nil (lena-rpc-make-owner) "x" (lena-infoview-hover-test-handle))
           (with-current-buffer (lena-infoview--buffer)
             (switch-to-buffer (current-buffer))
             (goto-char (point-min))
             ,@body))
       (when-let* ((buffer (lena-infoview--buffer)))
         (kill-buffer buffer)))))

(defun lena-infoview-hover-test-eldoc ()
  "Return what `lena-infoview-eldoc-function' reports at point.
The result is (DOC . ECHO), or nil if it reports nothing."
  (let (report)
    (lena-infoview-eldoc-function
     (lambda (doc &rest keys)
       (setq report (cons (substring-no-properties doc)
                          (substring-no-properties
                           (plist-get keys :echo))))))
    report))

;;;; ElDoc:

(ert-deftest lena-infoview-hover-test-eldoc ()
  "ElDoc shows the subterm at point and its type, then its documentation."
  (lena-infoview-hover-test-with-server
    (lena-infoview-hover-test-with-goal
      (should (memq #'lena-infoview-eldoc-function
                    eldoc-documentation-functions))
      (should (eq (and eldoc-mode t) global-eldoc-mode))
      (should (eql eldoc-idle-delay lena-eldoc-idle-delay))
      (should (lena-rpc-handle-p lena-infoview--handle))
      (lena-test-goto "n = 0")
      (should (equal (lena-infoview-hover-test-eldoc)
                     '("n : Nat\n\nThe `n` of the test." . "n : Nat")))
      (let ((call (car lena-infoview-hover-test-calls)))
        (should (equal (plist-get call :info) '(:__rpcref "1")))
        (should (eq (plist-get call :slot) 'infoview-eldoc))
        ;; The answer is text; its references are released at once.
        (should (lena-rpc-owner-released (plist-get call :owner))))
      ;; Nothing on the line but the subterm.
      (lena-test-goto " = 0")
      (should-not (lena-infoview-hover-test-eldoc))
      (should-not (lena-infoview-eldoc-function #'ignore)))))

(ert-deftest lena-infoview-hover-test-eldoc-cache ()
  "ElDoc asks about a subterm once, until the goals are shown again."
  (lena-infoview-hover-test-with-server
    (lena-infoview-hover-test-with-goal
      (lena-test-goto "n = 0")
      (let ((report (lena-infoview-hover-test-eldoc)))
        (should (= (length lena-infoview-hover-test-calls) 1))
        (should (equal (lena-infoview-hover-test-eldoc) report))
        (should (= (length lena-infoview-hover-test-calls) 1))
        (lena-infoview--show (list :goals (vector (lena-test-goal)))
                             nil (lena-rpc-make-owner) "x"
                             lena-infoview--handle)
        ;; The old nodes are not kept.
        (should-not lena-infoview--info-cache)
        (lena-test-goto "n = 0")
        (should (equal (lena-infoview-hover-test-eldoc) report))
        (should (= (length lena-infoview-hover-test-calls) 2))))))

(ert-deftest lena-infoview-hover-test-eldoc-diff ()
  "ElDoc explains how the tactic at point changes the subterm."
  (lena-infoview-hover-test-with-server
    (lena-test-with-options
      (unwind-protect
          (progn
            (lena-infoview--show
             (list :goals
                   (vector (lena-test-goal
                            :type (lena-test-code
                                   '("n" "1" "willDelete") " = 0"))))
             nil (lena-rpc-make-owner) "x" (lena-infoview-hover-test-handle))
            (with-current-buffer (lena-infoview--buffer)
              (lena-test-goto "n = 0")
              (should (string-suffix-p
                       "\n\nThe tactic at point will delete this subterm."
                       (car (lena-infoview-hover-test-eldoc))))))
        (kill-buffer (lena-infoview--buffer))))))

(ert-deftest lena-infoview-hover-test-eldoc-no-handle ()
  "Without a live RPC handle, ElDoc reports nothing."
  (lena-infoview-hover-test-with-server
    (lena-infoview-hover-test-with-goal
      (lena-test-goto "n = 0")
      (cl-letf (((symbol-function 'lena-rpc-handle-live-p) #'ignore))
        (should-not (lena-infoview-eldoc-function #'ignore)))
      (setq lena-infoview--handle nil)
      (should-not (lena-infoview-eldoc-function #'ignore))
      (should-not lena-infoview-hover-test-calls))))

;;;; Tooltips:

(ert-deftest lena-infoview-hover-test-tooltip ()
  "With `tooltip-mode', the tooltip of a subterm documents it."
  (lena-infoview-hover-test-with-server
    (lena-infoview-hover-test-with-goal
      (lena-test-goto "n = 0")
      (let ((help-echo (get-char-property (point) 'help-echo))
            (pos (point))
            (under-mouse (lena-render-node-at (point)))
            shown)
        (cl-letf (((symbol-function 'lena-infoview--node-under-mouse)
                   (lambda () under-mouse))
                  (show-help-function (lambda (text) (push text shown))))
          ;; Not in the echo area, where ElDoc shows the subterm at point.
          (let ((tooltip-mode nil))
            (should-not (funcall help-echo nil nil pos))
            (should-not lena-infoview-hover-test-calls))
          (let ((tooltip-mode t))
            ;; Asked for once, while Lean has not answered.
            (setq lena-infoview-hover-test-deferred (list #'ignore))
            (should-not (funcall help-echo nil nil pos))
            (should-not (funcall help-echo nil nil pos))
            (should (= (length lena-infoview-hover-test-calls) 1))
            (should (eq (plist-get (car lena-infoview-hover-test-calls) :slot)
                        'infoview-tooltip))
            ;; Shown when Lean answers, as the mouse is still there.
            (funcall (car lena-infoview-hover-test-deferred))
            (setq lena-infoview-hover-test-deferred nil)
            (should (equal (mapcar #'substring-no-properties shown)
                           '("n : Nat\n\nThe `n` of the test.")))
            ;; Then known.
            (should (equal (substring-no-properties
                            (funcall help-echo nil nil pos))
                           "n : Nat\n\nThe `n` of the test."))
            ;; ElDoc knows it too.
            (should (equal (lena-infoview-hover-test-eldoc)
                           '("n : Nat\n\nThe `n` of the test." . "n : Nat")))
            (should (= (length lena-infoview-hover-test-calls) 1))
            ;; Not shown if the mouse has moved on.
            (setq shown nil under-mouse nil)
            (lena-infoview--show (list :goals (vector (lena-test-goal)))
                                 nil (lena-rpc-make-owner) "x"
                                 lena-infoview--handle)
            (lena-test-goto "n = 0")
            (should-not (funcall (get-char-property (point) 'help-echo)
                                 nil nil (point)))
            (should (= (length lena-infoview-hover-test-calls) 2))
            (should-not shown)))))))

;;;; Descriptions:

(defun lena-infoview-hover-test-help ()
  "Return the text of the help buffer."
  (with-current-buffer lena-infoview-help-buffer-name
    (buffer-substring-no-properties (point-min) (point-max))))

(defun lena-infoview-hover-test-page-owners ()
  "Return whether the owners of the kept descriptions are released."
  (mapcar (lambda (page)
            (lena-rpc-owner-released (lena-infoview--page-owner page)))
          lena-infoview--pages))

(ert-deftest lena-infoview-hover-test-describe ()
  "A subterm is described in the help buffer."
  (lena-infoview-hover-test-with-server
    (lena-infoview-hover-test-with-goal
      (should-error (lena-infoview-describe) :type 'user-error)
      (lena-test-goto "n = 0")
      (lena-infoview-describe)
      (should (equal (lena-infoview-hover-test-help)
                     "n : Nat

The `n` of the test.

From the goals at Source.lean:2:2.
"))
      (should (eq (plist-get (car lena-infoview-hover-test-calls) :slot)
                  'infoview-describe))
      (with-current-buffer lena-infoview-help-buffer-name
        ;; The location leads to the source.
        (search-forward "Source.lean")
        (push-button)
        (should (equal (buffer-name) "Source.lean"))
        (should (looking-at "rfl"))))))

(ert-deftest lena-infoview-hover-test-describe-keys ()
  "Subterms are described with RET, mouse-1 and `C-h .'."
  (lena-infoview-hover-test-with-server
    (lena-infoview-hover-test-with-goal
      (lena-test-goto "n = 0")
      (should (eq (key-binding (kbd "RET")) #'lena-infoview-describe))
      (should (eq (key-binding (kbd "C-h .")) #'lena-infoview-describe))
      (should (eq (key-binding [mouse-2]) #'lena-infoview-describe-at-mouse))
      ;; mouse-1 follows links, see `mouse-on-link-p'.
      (should (eq (key-binding [follow-link] nil t (point)) 'mouse-face))
      (should (get-char-property (point) 'mouse-face))
      (lena-test-goto " = 0")
      (should-not (eq (key-binding (kbd "RET")) #'lena-infoview-describe))
      (should-not (get-char-property (point) 'mouse-face)))))

(ert-deftest lena-infoview-hover-test-describe-nested ()
  "Subterms of a description are described in turn; history leads back."
  (lena-infoview-hover-test-with-server
    (lena-infoview-hover-test-with-goal
      (lena-test-goto "n = 0")
      (lena-infoview-describe)
      (with-current-buffer lena-infoview-help-buffer-name
        (goto-char (point-min))
        (search-forward "Nat")
        (backward-char)
        (should (eq (key-binding (kbd "RET")) #'lena-infoview-describe))
        (lena-infoview-describe)
        (should (equal (plist-get (car lena-infoview-hover-test-calls) :info)
                       '(:__rpcref "12")))
        (should (string-prefix-p "Nat : Type\n\nThe natural numbers.\n"
                                 (lena-infoview-hover-test-help)))
        (let ((calls (length lena-infoview-hover-test-calls)))
          (help-go-back)
          (should (string-prefix-p "n : Nat\n" (lena-infoview-hover-test-help)))
          (help-go-forward)
          (should (string-prefix-p "Nat : Type\n"
                                   (lena-infoview-hover-test-help)))
          ;; History shows what was received; it asks the server nothing.
          (should (= (length lena-infoview-hover-test-calls) calls)))
        ;; A page from history still describes its subterms.
        (help-go-back)
        (goto-char (point-min))
        (lena-infoview-describe)
        (should (equal (plist-get (car lena-infoview-hover-test-calls) :info)
                       '(:__rpcref "11")))
        (should (eq major-mode 'lena-infoview-help-mode))
        (should (eq (and eldoc-mode t) global-eldoc-mode))
        ;; Emacs's own help is elsewhere, and leaves this alone.
        (should-not (get-buffer "*Help*"))
        (describe-variable 'lena-infoview-delay)
        (should (string-prefix-p "0 : Nat\n" (lena-infoview-hover-test-help)))
        (should lena-infoview--handle)))))

(ert-deftest lena-infoview-hover-test-describe-release ()
  "Descriptions keep their references while history can lead back."
  (lena-infoview-hover-test-with-server
    (lena-infoview-hover-test-with-goal
      (lena-test-goto "n = 0")
      (lena-infoview-describe)
      (with-current-buffer lena-infoview-help-buffer-name
        (goto-char (point-min))
        (search-forward "Nat")
        (lena-infoview-describe 1))
      (should (equal (lena-infoview-hover-test-page-owners) '(nil nil)))
      (let ((first (car (last lena-infoview--pages)))
            (second (car lena-infoview--pages)))
        ;; Going back and describing anew drops the way forward.
        (with-current-buffer lena-infoview-help-buffer-name
          (help-go-back))
        ;; Help can still go forward to it.
        (should-not (lena-rpc-owner-released
                     (lena-infoview--page-owner second)))
        (lena-infoview-describe)
        (should (lena-rpc-owner-released (lena-infoview--page-owner second)))
        (should-not (memq second lena-infoview--pages))
        (should-not (lena-rpc-owner-released
                     (lena-infoview--page-owner first)))
        ;; Without the help buffer, nothing leads back.
        (kill-buffer lena-infoview-help-buffer-name)
        (should (lena-rpc-owner-released (lena-infoview--page-owner first)))
        (should-not lena-infoview--pages)
        (lena-infoview-describe)
        (should (equal (lena-infoview-hover-test-page-owners) '(nil)))))))

(ert-deftest lena-infoview-hover-test-return-from-help ()
  "Closing the help window, as Doom Emacs does, returns to the infoview."
  (lena-infoview-hover-test-with-server
    (lena-test-with-windows
      (lena-infoview--show (list :goals (vector (lena-test-goal)))
                           nil (lena-rpc-make-owner) "x"
                           (lena-infoview-hover-test-handle))
      (let ((home lena-infoview--window)
            (main (get-buffer-window "Main")))
        (lena-test-goto "n = 0")
        (lena-infoview-describe)
        (let ((help (get-buffer-window lena-infoview-help-buffer-name)))
          (should (window-live-p help))
          (should-not (memq help (list home main)))
          ;; Closed while selected: Emacs selects the main window.
          (select-window help)
          (delete-window help)
          (should (eq (selected-window) main))
          (cl-letf (((symbol-function 'old-selected-window)
                     (lambda () help)))
            (lena-infoview--return-from-help nil))
          (should (eq (selected-window) home))
          (should-not (memq #'lena-infoview--return-from-help
                            (default-value
                             'window-selection-change-functions))))
        ;; Closed from elsewhere: no reason to go back.
        (lena-infoview-describe)
        (let ((help (get-buffer-window lena-infoview-help-buffer-name)))
          (select-window main)
          (delete-window help)
          (cl-letf (((symbol-function 'old-selected-window)
                     (lambda () main)))
            (lena-infoview--return-from-help nil))
          (should (eq (selected-window) main)))))))

(ert-deftest lena-infoview-hover-test-describe-dead ()
  "A subterm that Lean no longer knows is reported, not raised."
  (lena-infoview-hover-test-with-server
    (lena-infoview-hover-test-with-goal
      (lena-test-goto "n = 0")
      (let (messages)
        (cl-letf (((symbol-function 'message)
                   (lambda (&rest args)
                     (push (apply #'format-message args) messages))))
          (setq lena-infoview-hover-test-error -32602)
          (lena-infoview-describe)
          (setq lena-infoview-hover-test-error -32000)
          (lena-infoview-describe))
        (should (string-prefix-p "Lean could not look up this subterm: fake"
                                 (car messages)))
        (should (string-prefix-p "Lean no longer knows this subterm"
                                 (cadr messages)))
        (should-not lena-infoview--pages))
      ;; The session has ended.
      (cl-letf (((symbol-function 'lena-rpc-handle-live-p) #'ignore))
        (should-error (lena-infoview-describe) :type 'user-error)))))

;;;; Definitions:

(defmacro lena-infoview-hover-test-with-definitions (links &rest body)
  "Run BODY with getGoToLocation answered with LINKS, or ERROR if a number.
The links are made with `lena-infoview-hover-test-link'.  The calls
made are recorded in `lena-infoview-hover-test-calls'."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'lena-rpc-call-sync)
              (lambda (_handle method params &rest keys)
                (push (append (list :method method :params params) keys)
                      lena-infoview-hover-test-calls)
                (let ((links ,links))
                  (if (numberp links)
                      (signal 'lena-rpc-error
                              (list method (list :code links
                                                 :message "fake error")))
                    links)))))
     ,@body))

(defun lena-infoview-hover-test-link (file line character)
  "Return a `LocationLink' to LINE and CHARACTER, in UTF-16, of FILE."
  (let ((position (list :line line :character character)))
    (list :targetUri (concat "file://" file)
          :targetRange (list :start '(:line 0 :character 0) :end position)
          :targetSelectionRange (list :start position :end position))))

(ert-deftest lena-infoview-hover-test-definitions ()
  "The definition of a subterm is found with xref."
  (let ((file (make-temp-file "lena-test" nil ".lean"
                              "-- 𝓝\ndef 𝓝𝓝 b : Nat := 1\n")))
    (unwind-protect
        (lena-infoview-hover-test-with-server
          (lena-infoview-hover-test-with-goal
            (lena-test-goto "n = 0")
            (should (eq (xref-find-backend) 'lena-infoview))
            (let ((identifier (xref-backend-identifier-at-point
                               'lena-infoview)))
              (should (equal identifier "n"))
              (lena-infoview-hover-test-with-definitions
                  ;; After "def 𝓝𝓝 ", which is 7 characters but 9
                  ;; UTF-16 code units.
                  (list (lena-infoview-hover-test-link file 1 9))
                (let* ((items (xref-backend-definitions 'lena-infoview
                                                        identifier))
                       (location (xref-item-location (car items))))
                  (should (= (length items) 1))
                  (should (equal (xref-item-summary (car items))
                                 "def 𝓝𝓝 b : Nat := 1"))
                  (should (equal (xref-file-location-file location) file))
                  (should (= (xref-file-location-line location) 2))
                  (should (= (xref-file-location-column location) 7))
                  (let ((call (car lena-infoview-hover-test-calls)))
                    (should (equal (plist-get call :method)
                                   "Lean.Widget.getGoToLocation"))
                    (should (equal (plist-get call :params)
                                   '(:kind "definition"
                                     :info (:__rpcref "1"))))
                    (should (eq (plist-get call :retry) nil))
                    (should (plist-member call :retry)))
                  ;; Doom Emacs passes the identifier without properties.
                  (should (equal (xref-backend-definitions 'lena-infoview "n")
                                 items))
                  ;; The same from a buffer visiting the file.
                  (let ((visiting (find-file-noselect file)))
                    (unwind-protect
                        (should (equal (xref-backend-definitions
                                        'lena-infoview identifier)
                                       items))
                      (kill-buffer visiting))))))
            ;; Not on a subterm, the backend is someone else's.
            (lena-test-goto " = 0")
            (should-not (eq (xref-find-backend) 'lena-infoview))))
      (delete-file file))))

(ert-deftest lena-infoview-hover-test-definitions-failure ()
  "A failure to find definitions is reported to the user."
  (lena-infoview-hover-test-with-server
    (lena-infoview-hover-test-with-goal
      (lena-test-goto "n = 0")
      (lena-infoview-hover-test-with-definitions -32602
        (should (equal (cadr (should-error
                              (xref-backend-definitions 'lena-infoview "n")
                              :type 'user-error))
                       "Lean no longer knows this subterm; \
describe it again from the infoview")))
      (lena-infoview-hover-test-with-definitions []
        (should-not (xref-backend-definitions 'lena-infoview "n"))))))

;;;; Doom Emacs:

(ert-deftest lena-infoview-hover-test-doom-lookup ()
  "Doom Emacs' `K' describes the subterm at point."
  (lena-infoview-hover-test-with-server
    (lena-infoview-hover-test-with-goal
      (should (memq #'lena-infoview--lookup-documentation
                    +lookup-documentation-functions))
      (should-not (default-value '+lookup-documentation-functions))
      (should-not (lena-infoview--lookup-documentation "goal"))
      (should-not (get-buffer lena-infoview-help-buffer-name))
      (lena-test-goto "n = 0")
      (should (eq (lena-infoview--lookup-documentation "n") 'deferred))
      (should (string-prefix-p "n : Nat\n" (lena-infoview-hover-test-help))))))

(ert-deftest lena-infoview-hover-test-no-doom ()
  "Without Doom Emacs, its hook is left undefined."
  (let ((value +lookup-documentation-functions))
    (makunbound '+lookup-documentation-functions)
    (unwind-protect
        (with-temp-buffer
          (lena-infoview-hover-setup)
          (should-not (boundp '+lookup-documentation-functions)))
      (setq +lookup-documentation-functions value))))

(provide 'lena-infoview-hover-tests)

;;; lena-infoview-hover-tests.el ends here
