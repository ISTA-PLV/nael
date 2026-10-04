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

(defmacro nael-infoview-hover-test-with-server (&rest body)
  "Run BODY with RPC calls answered at once by a stub server."
  (declare (indent 0) (debug t))
  `(let ((nael-infoview-hover-test-calls nil))
     (cl-letf (((symbol-function 'nael-rpc-handle-live-p) #'always)
               ((symbol-function 'nael-rpc-info-to-interactive)
                (lambda (_handle info success &rest keys)
                  (push (append (list :method 'info :info info) keys)
                        nael-infoview-hover-test-calls)
                  (let ((result (nael-infoview-hover-test-info info)))
                    (when-let* ((owner (plist-get keys :owner)))
                      (setf (nael-rpc-owner-refs owner)
                            (nael-rpc-collect-refs result :__rpcref)))
                    (funcall success result)))))
       ,@body)))

(defmacro nael-infoview-hover-test-with-goal (&rest body)
  "Run BODY in the infoview showing the goal `n = 0', at its start."
  (declare (indent 0) (debug t))
  `(nael-infoview-test-with-options
     (unwind-protect
         (progn
           (nael-infoview--show
            (list :goals (vector (nael-render-test-goal)))
            nil (nael-rpc-make-owner) "x" 'handle)
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
      (should (eq nael-infoview--handle 'handle))
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
                             nil (nael-rpc-make-owner) "x" 'handle)
        ;; The old nodes are not kept.
        (should-not nael-infoview--eldoc-cache)
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
             nil (nael-rpc-make-owner) "x" 'handle)
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

(provide 'nael-infoview-hover-tests)

;;; nael-infoview-hover-tests.el ends here
