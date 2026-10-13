;;; lena-eglot-tests.el --- Tests for lena-eglot  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see lena/LICENSE.GPL3.

;;; Commentary:

;; Tests for the handling of diagnostics in `lena-eglot' that need no
;; Lean server.  Diagnostics are made by hand the way the server makes
;; them: the range of one that spans several lines ends at the start of
;; its second line, and its full range is sent as `:fullRange'.

;;; Code:

(require 'ert)
(require 'lena-eglot)
(require 'lena-test-helpers)

(ert-deftest lena-eglot-test-diagnostic-lines ()
  "Lines of a diagnostic, by its range or its full range."
  (let ((diagnostic (lena-test-diagnostic 1 '(2 . 4) '(5 . 3))))
    (should (equal (lena-eglot-diagnostic-lines diagnostic) '(2 . 2)))
    (should (equal (lena-eglot-diagnostic-lines diagnostic t) '(2 . 5))))
  ;; A range that ends at the start of a line ends on the line before.
  (should (equal (lena-eglot-diagnostic-lines
                  (lena-test-diagnostic 1 '(2 . 4) '(5 . 0))
                  t)
                 '(2 . 4)))
  ;; Without a full range, the range counts.
  (should (equal (lena-eglot-diagnostic-lines
                  (list :range (lena-test-range '(3 . 0) '(3 . 5)))
                  t)
                 '(3 . 3))))

(ert-deftest lena-eglot-test-goals-accomplished-p ()
  "Lines inside a complete proof, by the full range of its diagnostic."
  (with-temp-buffer
    (setq lena-eglot-diagnostics
          (vector (lena-test-diagnostic 3 '(2 . 0) '(6 . 0)
                                         :tags '(2) :silent t)
                  (lena-test-diagnostic 1 '(8 . 0) '(9 . 4)
                                         :tags '(1))))
    (should-not (lena-eglot-goals-accomplished-p 1))
    (should (lena-eglot-goals-accomplished-p 2))
    (should (lena-eglot-goals-accomplished-p 5))
    (should-not (lena-eglot-goals-accomplished-p 6))
    (should-not (lena-eglot-goals-accomplished-p 8))))

(defclass lena-eglot-test-server (lena-eglot-server) ()
  "Server whose own handling of diagnostics only records them.")

(defvar lena-eglot-test-handled nil
  "Diagnostics that reached the handler of `lena-eglot-test-server'.")

(cl-defmethod eglot-handle-notification
  ((_server lena-eglot-test-server)
   (_method (eql textDocument/publishDiagnostics))
   &key diagnostics &allow-other-keys)
  "Record DIAGNOSTICS instead of handing them to Flymake."
  (setq lena-eglot-test-handled diagnostics))

(cl-defmethod eglot-client-capabilities ((_server lena-eglot-test-server))
  "Return capabilities without asking Eglot, which needs a project."
  (list :workspace nil))

(ert-deftest lena-eglot-test-silent-diagnostics ()
  "Eglot does not see silent diagnostics; Lena keeps all."
  (let* ((file (make-temp-file "lena-eglot-test" nil ".lean"))
         (buffer (find-file-noselect file))
         (server (make-instance 'lena-eglot-test-server
                                :process (make-pipe-process
                                          :name "lena-eglot-test"
                                          :noquery t)))
         (silent (lena-test-diagnostic 3 '(0 . 0) '(1 . 4)
                                        :tags '(2) :silent t))
         (error (lena-test-diagnostic 1 '(3 . 0) '(3 . 4)))
         (hooked nil)
         (lena-eglot-test-handled nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (add-hook 'lena-eglot-diagnostics-functions
                      (lambda (diagnostics) (setq hooked diagnostics))
                      nil t))
          (eglot-handle-notification server 'textDocument/publishDiagnostics
                                     :uri (eglot-path-to-uri file)
                                     :diagnostics (vector silent error))
          (should (equal lena-eglot-test-handled (vector error)))
          (should (equal hooked (vector silent error)))
          (should (equal (buffer-local-value 'lena-eglot-diagnostics buffer)
                         (vector silent error))))
      (delete-process (jsonrpc--process server))
      (kill-buffer buffer)
      (delete-file file))))

(ert-deftest lena-eglot-test-capabilities ()
  "The client asks for silent diagnostics and the RPC wire format."
  (let ((server (make-instance 'lena-eglot-test-server
                               :process (make-pipe-process
                                         :name "lena-eglot-test"
                                         :noquery t))))
    (unwind-protect
        (let ((lean (plist-get (eglot-client-capabilities server) :lean)))
          (should (eq (plist-get lean :silentDiagnosticSupport) t))
          (should (equal (plist-get lean :rpcWireFormat) "v1")))
      (delete-process (jsonrpc--process server)))))

;;;; Semantic tokens:

(defvar eglot-semantic-tokens-mode)
(defvar eglot--semtok-state)

(ert-deftest lena-eglot-test-semantic-tokens-refresh ()
  "Lean's requests to refresh semantic tokens make Eglot ask again."
  (let ((server (make-instance 'lena-eglot-test-server
                               :process (make-pipe-process
                                         :name "lena-eglot-test"
                                         :noquery t)))
        (flushed nil))
    (with-temp-buffer
      (let ((shown (current-buffer)))
        (with-temp-buffer
          (let ((other (current-buffer)))
            (with-current-buffer shown
              (setq-local eglot-semantic-tokens-mode t
                          eglot--semtok-state '(:data [] :docver 0)))
            (with-current-buffer other
              (setq-local eglot--semtok-state '(:data [] :docver 0)))
            (unwind-protect
                (cl-letf (((symbol-function 'eglot-managed-p)
                           (lambda () (memq (current-buffer) (list shown other))))
                          ((symbol-function 'eglot-current-server)
                           (lambda () server))
                          ((symbol-function 'font-lock-flush)
                           (lambda (&rest _) (push (current-buffer) flushed))))
                  (should-not (eglot-handle-request
                               server 'workspace/semanticTokens/refresh))
                  (should-not (buffer-local-value 'eglot--semtok-state shown))
                  (should (equal flushed (list shown)))
                  ;; Without semantic tokens shown, nothing changes.
                  (should (buffer-local-value 'eglot--semtok-state other)))
              (delete-process (jsonrpc--process server)))))))))

(ert-deftest lena-eglot-test-inlay-hints-refresh ()
  "Lean's requests to refresh inlay hints make Eglot ask for the shown ones."
  (let ((server (make-instance 'lena-eglot-test-server
                               :process (make-pipe-process
                                         :name "lena-eglot-test"
                                         :noquery t)))
        (asked nil))
    (with-temp-buffer
      (let ((shown (current-buffer)))
        (insert (make-string 100 ?x))
        (with-temp-buffer
          (let ((hidden (current-buffer))
                (window (selected-window))
                (previous (window-buffer (selected-window))))
            (with-current-buffer shown (setq-local eglot-inlay-hints-mode t))
            (with-current-buffer hidden (setq-local eglot-inlay-hints-mode t))
            (unwind-protect
                (cl-letf (((symbol-function 'eglot-managed-p)
                           (lambda () (memq (current-buffer) (list shown hidden))))
                          ((symbol-function 'eglot-current-server)
                           (lambda () server))
                          ((symbol-function 'lena-eglot-compat-update-hints)
                           (lambda (from to)
                             (push (list (current-buffer) from to) asked))))
                  (set-window-buffer window shown)
                  (should-not (eglot-handle-request
                               server 'workspace/inlayHint/refresh))
                  ;; Only the shown buffer is asked, for its window.
                  (should (equal (mapcar #'car asked) (list shown)))
                  (should (equal (cdar asked)
                                 (list (window-start window)
                                       (window-end window t)))))
              (set-window-buffer window previous)
              (delete-process (jsonrpc--process server)))))))))

(ert-deftest lena-eglot-test-semantic-tokens-restart ()
  "Restarting the file makes Eglot ask for its semantic tokens again."
  (with-temp-buffer
    (setq-local eglot-semantic-tokens-mode t
                eglot--semtok-state '(:data [] :docver 0))
    (let (sent)
      (cl-letf (((symbol-function 'eglot-managed-p) #'always)
                ((symbol-function 'lena-eglot-compat-did-close)
                 (lambda () (push 'close sent)))
                ((symbol-function 'lena-eglot-compat-did-open)
                 (lambda () (push 'open sent)))
                ((symbol-function 'font-lock-flush) #'ignore))
        (lena-eglot-restart-file))
      (should (equal sent '(open close)))
      (should-not eglot--semtok-state))))

;;;; Building dependencies:

(defvar lena-eglot-test-sent nil
  "Messages that `lena-eglot-test-server' was asked to send, newest first.")

(cl-defmethod jsonrpc-connection-send ((_server lena-eglot-test-server)
                                       &rest args)
  "Record ARGS instead of sending them."
  (push args lena-eglot-test-sent))

(ert-deftest lena-eglot-test-dependency-build-mode ()
  "Opening a file tells the server whether to build its imports."
  (let ((server (make-instance 'lena-eglot-test-server
                               :process (make-pipe-process
                                         :name "lena-eglot-test"
                                         :noquery t)))
        (lena-eglot-test-sent nil))
    (unwind-protect
        (cl-flet ((open ()
                    (jsonrpc-connection-send
                     server :method :textDocument/didOpen
                     :params '(:textDocument (:uri "file:///A.lean")))
                    (plist-get (plist-get (car lena-eglot-test-sent) :params)
                               :dependencyBuildMode)))
          (let ((lena-eglot-build-dependencies nil))
            (should (equal (open) "never"))
            (let ((lena-eglot--build-dependencies-once t))
              (should (equal (open) "once"))))
          (let ((lena-eglot-build-dependencies t))
            (should (equal (open) "always")))
          (jsonrpc-connection-send server :method :textDocument/didClose
                                   :params '(:textDocument (:uri "u")))
          (should (equal (plist-get (car lena-eglot-test-sent) :params)
                         '(:textDocument (:uri "u")))))
      (delete-process (jsonrpc--process server)))))

;;;; Widgets in messages:

(defmacro lena-eglot-test-with-toolchain (toolchain &rest body)
  "Run BODY in a directory whose `lean-toolchain' says TOOLCHAIN.
TOOLCHAIN nil means no such file."
  (declare (indent 1) (debug t))
  `(let ((default-directory (file-name-as-directory
                             (make-temp-file "lena-eglot-test" t))))
     (unwind-protect
         (progn
           (when ,toolchain
             (with-temp-file "lean-toolchain" (insert ,toolchain "\n")))
           ,@body)
       (delete-directory default-directory t))))

(ert-deftest lena-eglot-test-lean-version ()
  "The Lean version of a project, from its `lean-toolchain'."
  (dolist (case '(("leanprover/lean4:v4.34.1" (4 34))
                  ("leanprover/lean4:v4.35.0-rc3" (4 35))
                  ("leanprover/lean4:v4.9.0" (4 9))
                  ("v4.30.0" (4 30))
                  ("leanprover/lean4:nightly-2026-09-01" nightly)
                  ("leanprover/lean4-pr-releases:pr-release-1234" nil)
                  (nil nil)))
    (lena-eglot-test-with-toolchain (car case)
      (should (equal (lena-eglot-lean-version default-directory)
                     (cadr case)))
      ;; Also from a directory below.
      (make-directory "sub")
      (should (equal (lena-eglot-lean-version "sub/") (cadr case))))))

(defvar lena-eglot-test-base-options nil
  "Initialization options of the test server before Lena adds its own.")

(cl-defmethod eglot-initialization-options ((_server lena-eglot-test-server))
  "Return `lena-eglot-test-base-options'."
  lena-eglot-test-base-options)

(ert-deftest lena-eglot-test-has-widgets ()
  "The server is asked for widgets as `lena-eglot-widgets' says."
  (let ((server (make-instance 'lena-eglot-test-server
                               :process (make-pipe-process
                                         :name "lena-eglot-test"
                                         :noquery t))))
    (unwind-protect
        (cl-flet ((options (toolchain setting base)
                    (lena-eglot-test-with-toolchain toolchain
                      (let ((lena-eglot-widgets setting)
                            (lena-eglot-test-base-options base))
                        (eglot-initialization-options server)))))
          (should (equal (options "leanprover/lean4:v4.30.0" 'auto eglot--{})
                         '(:hasWidgets t)))
          (should (equal (options "leanprover/lean4:nightly-2026-09-01" 'auto
                                  nil)
                         '(:hasWidgets t)))
          (should (eq (options "leanprover/lean4:v4.29.1" 'auto eglot--{})
                      eglot--{}))
          (should (eq (options nil 'auto eglot--{}) eglot--{}))
          (should (equal (options "leanprover/lean4:v4.28.0" t '(:other 1))
                         '(:other 1 :hasWidgets t)))
          (should (equal (options "leanprover/lean4:v4.34.1" nil '(:other 1))
                         '(:other 1)))
          (let ((table (make-hash-table :test #'equal)))
            (puthash "other" 1 table)
            (let ((result (options "leanprover/lean4:v4.34.1" 'auto table)))
              (should (eq (gethash "hasWidgets" result) t))
              (should (= (gethash "other" result) 1))
              (should-not (gethash "hasWidgets" table)))))
      (delete-process (jsonrpc--process server)))))

(provide 'lena-eglot-tests)

;;; lena-eglot-tests.el ends here
