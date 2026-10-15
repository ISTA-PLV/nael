;;; lena-rpc-live.el --- Tests for lena-rpc against Lean  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see lena/LICENSE.GPL3.

;;; Commentary:

;; Tests for `lena-rpc' against a real Lean server, run by Eglot on the
;; Lake project in `test/lean/'.  They need `lake' in `exec-path' and
;; are skipped otherwise.  The test of keep-alives waits 35 seconds and
;; only runs if the environment variable LENA_TEST_SLOW is set.
;;
;; Run with: emacs -Q --batch -l build.el test-live

;;; Code:

(require 'ert)
(require 'lena-render)
(require 'lena-rpc)

(defconst lena-rpc-live-root
  (file-name-as-directory
   (expand-file-name "lean" (file-name-directory
                             (or load-file-name buffer-file-name)))))

(defconst lena-rpc-live-text
  "import Tiny.B

theorem t (n : Nat) (h : n = b) : n + 0 = 1 := by
  rw [h]
  rfl
"
  "Contents of `Tiny/A.lean', restored after each test.")

(defvar lena-rpc-live-buffer nil)

(defun lena-rpc-live-wait (predicate &optional seconds)
  "Process input until PREDICATE holds or SECONDS (default 60) pass."
  (let ((deadline (+ (float-time) (or seconds 60))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(defun lena-rpc-live-processed-p ()
  "Return non-nil if the server has finished processing the buffer."
  (and (vectorp lena-eglot-file-progress)
       (seq-empty-p lena-eglot-file-progress)))

(defun lena-rpc-live-buffer ()
  "Return the buffer of `Tiny/A.lean', managed by Eglot and processed."
  (unless (executable-find "lake")
    (ert-skip "No `lake' in `exec-path'"))
  (unless (buffer-live-p lena-rpc-live-buffer)
    (let ((default-directory lena-rpc-live-root))
      (call-process "lake" nil nil nil "build"))
    (setq project-find-functions
          (list (lambda (_) (cons 'transient lena-rpc-live-root))))
    (setq lena-rpc-live-buffer
          (find-file-noselect (concat lena-rpc-live-root "Tiny/A.lean")))
    (with-current-buffer lena-rpc-live-buffer
      (lena-mode)
      (let ((default-directory lena-rpc-live-root))
        (apply #'eglot (eglot--guess-contact)))
      (should (lena-rpc-live-wait #'eglot-managed-p 30))))
  (with-current-buffer lena-rpc-live-buffer
    (unless (equal (buffer-string) lena-rpc-live-text)
      (erase-buffer)
      (insert lena-rpc-live-text)
      (set-buffer-modified-p nil)
      (lena-eglot-flush-changes)
      (setq lena-eglot-file-progress nil))
    (should (lena-rpc-live-wait #'lena-rpc-live-processed-p 60)))
  lena-rpc-live-buffer)

(defun lena-rpc-live-goto (line column)
  "Go to 1-based LINE and 0-based COLUMN."
  (goto-char (point-min))
  (forward-line (1- line))
  (move-to-column column))

(defun lena-rpc-live-sync (call)
  "Call CALL with success and failure functions; wait; return the outcome.
The outcome is (ok . RESULT), (error . ERROR) or nil after 30 seconds."
  (let (outcome)
    (funcall call
             (lambda (result) (setq outcome (cons 'ok result)))
             (lambda (error) (setq outcome (cons 'error error))))
    (lena-rpc-live-wait (lambda () outcome) 30)
    outcome))

(defun lena-rpc-live-goals (handle &optional owner)
  "Return the outcome of a goal request through HANDLE for OWNER."
  (lena-rpc-live-sync
   (lambda (ok fail)
     (lena-rpc-get-interactive-goals handle ok :failure fail :owner owner))))

(defun lena-rpc-live-target (outcome)
  "Return the target of the first goal in OUTCOME as a string."
  (when-let* ((goals (plist-get (cdr outcome) :goals))
              ((> (length goals) 0)))
    (lena-render--plain-text (plist-get (aref goals 0) :type))))

(defun lena-rpc-live-first-info (tagged)
  "Return the first subterm reference in TAGGED, a `CodeWithInfos'."
  (cond ((plist-member tagged :tag)
         (plist-get (aref (plist-get tagged :tag) 0) :info))
        ((plist-member tagged :append)
         (seq-some #'lena-rpc-live-first-info (plist-get tagged :append)))))

(ert-deftest lena-rpc-live-goals ()
  "Interactive goals arrive with their references."
  (with-current-buffer (lena-rpc-live-buffer)
    (lena-rpc-live-goto 4 2)
    (let* ((owner (lena-rpc-make-owner))
           (outcome (lena-rpc-live-goals (lena-rpc-open) owner))
           (goal (aref (plist-get (cdr outcome) :goals) 0)))
      (should (eq (car outcome) 'ok))
      (should (equal (lena-rpc-live-target outcome) "n + 0 = 1"))
      (should (equal (seq-mapcat (lambda (hyp) (plist-get hyp :names))
                                 (plist-get goal :hyps) 'list)
                     '("n" "h")))
      (should (> (length (lena-rpc-owner-refs owner)) 3))
      (lena-rpc-owner-release owner))))

(ert-deftest lena-rpc-live-release ()
  "A reference works until its owner is released."
  (with-current-buffer (lena-rpc-live-buffer)
    (lena-rpc-live-goto 4 2)
    (let* ((handle (lena-rpc-open))
           (owner (lena-rpc-make-owner))
           (goal (aref (plist-get (cdr (lena-rpc-live-goals handle owner))
                                  :goals)
                       0))
           (info (lena-rpc-live-first-info (plist-get goal :type)))
           (hover (lambda ()
                    (lena-rpc-live-sync
                     (lambda (ok fail)
                       (lena-rpc-info-to-interactive handle info ok
                                                     :failure fail))))))
      (should (eq (car (funcall hover)) 'ok))
      (lena-rpc-owner-release owner)
      (lena-rpc--flush-garbage (lena-rpc-handle-session handle))
      (let ((outcome (funcall hover)))
        (should (eq (car outcome) 'error))
        (should (equal (lena-rpc-error-code (cdr outcome)) -32602))))))

(ert-deftest lena-rpc-live-reconnect ()
  "A session the server does not know is replaced transparently."
  (with-current-buffer (lena-rpc-live-buffer)
    (lena-rpc-live-goto 4 2)
    (let ((handle (lena-rpc-open)))
      (should (eq (car (lena-rpc-live-goals handle)) 'ok))
      (setf (lena-rpc-session-id (lena-rpc-handle-session handle)) "123")
      (let ((outcome (lena-rpc-live-goals handle)))
        (should (eq (car outcome) 'ok))
        (should (equal (lena-rpc-live-target outcome) "n + 0 = 1"))
        (should-not (equal (lena-rpc-session-id
                            (lena-rpc-handle-session handle))
                           "123"))))))

(ert-deftest lena-rpc-live-slot ()
  "Of two calls in one slot, only the second is delivered."
  (with-current-buffer (lena-rpc-live-buffer)
    (lena-rpc-live-goto 4 2)
    (let ((handle (lena-rpc-open))
          delivered)
      (lena-rpc-get-interactive-goals
       handle (lambda (_) (push 'first delivered)) :slot 'goals)
      (lena-rpc-get-interactive-goals
       handle (lambda (_) (push 'second delivered)) :slot 'goals)
      (should (lena-rpc-live-wait (lambda () delivered) 30))
      ;; Give a late answer to the first call time to arrive.
      (lena-rpc-live-wait #'ignore 1)
      (should (equal delivered '(second))))))

(ert-deftest lena-rpc-live-edit-before-call ()
  "A call right after an edit is answered for the edited text."
  (with-current-buffer (lena-rpc-live-buffer)
    (lena-rpc-live-goto 2 0)
    (insert "\n\n\n")
    ;; The line of `rw' moved from 4 to 7; without the edit, line 7 is
    ;; past the end of the proof.
    (lena-rpc-live-goto 7 2)
    (let ((outcome (lena-rpc-live-goals (lena-rpc-open))))
      (should (eq (car outcome) 'ok))
      (should (equal (lena-rpc-live-target outcome) "n + 0 = 1")))))

(ert-deftest lena-rpc-live-import-change ()
  "A call survives the restart of the file worker after an import edit."
  (with-current-buffer (lena-rpc-live-buffer)
    (lena-rpc-live-goto 4 2)
    (let* ((handle (lena-rpc-open))
           (id (progn (should (eq (car (lena-rpc-live-goals handle)) 'ok))
                      (lena-rpc-session-id (lena-rpc-handle-session handle)))))
      (goto-char (point-min))
      (insert "import Init\n")
      (lena-rpc-live-goto 5 2)
      (let* ((handle (lena-rpc-open))
             (outcome (lena-rpc-live-goals handle)))
        (should (eq (car outcome) 'ok))
        (should (equal (lena-rpc-live-target outcome) "n + 0 = 1"))
        (should-not (equal (lena-rpc-session-id
                            (lena-rpc-handle-session handle))
                           id))))))

(ert-deftest lena-rpc-live-keepalive ()
  "A session stays alive beyond the server's 30 second expiry."
  (skip-unless (getenv "LENA_TEST_SLOW"))
  (with-current-buffer (lena-rpc-live-buffer)
    (lena-rpc-live-goto 4 2)
    (let* ((handle (lena-rpc-open))
           (id (progn (should (eq (car (lena-rpc-live-goals handle)) 'ok))
                      (lena-rpc-session-id (lena-rpc-handle-session handle)))))
      (lena-rpc-live-wait #'ignore 35)
      (should (eq (car (lena-rpc-live-goals handle)) 'ok))
      (should (equal (lena-rpc-session-id (lena-rpc-handle-session handle))
                     id)))))

(provide 'lena-rpc-live)

;;; lena-rpc-live.el ends here
