;;; nael-rpc-live.el --- Tests for nael-rpc against Lean  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see nael/LICENSE.GPL3.

;;; Commentary:

;; Tests for `nael-rpc' against a real Lean server, run by Eglot on the
;; Lake project in `test/lean/'.  They need `lake' in `exec-path' and
;; are skipped otherwise.  The test of keep-alives waits 35 seconds and
;; only runs if the environment variable NAEL_TEST_SLOW is set.
;;
;; Run with: emacs -Q --batch -l build.el test-live

;;; Code:

(require 'ert)
(require 'nael-render)
(require 'nael-rpc)

(defconst nael-rpc-live-root
  (file-name-as-directory
   (expand-file-name "lean" (file-name-directory
                             (or load-file-name buffer-file-name)))))

(defconst nael-rpc-live-text
  "import Tiny.B

theorem t (n : Nat) (h : n = b) : n + 0 = 1 := by
  rw [h]
  rfl
"
  "Contents of `Tiny/A.lean', restored after each test.")

(defvar nael-rpc-live-buffer nil)

(defun nael-rpc-live-wait (predicate &optional seconds)
  "Process input until PREDICATE holds or SECONDS (default 60) pass."
  (let ((deadline (+ (float-time) (or seconds 60))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(defun nael-rpc-live-processed-p ()
  "Return non-nil if the server has finished processing the buffer."
  (and (vectorp nael-eglot-file-progress)
       (seq-empty-p nael-eglot-file-progress)))

(defun nael-rpc-live-buffer ()
  "Return the buffer of `Tiny/A.lean', managed by Eglot and processed."
  (unless (executable-find "lake")
    (ert-skip "No `lake' in `exec-path'"))
  (unless (buffer-live-p nael-rpc-live-buffer)
    (let ((default-directory nael-rpc-live-root))
      (call-process "lake" nil nil nil "build"))
    (setq project-find-functions
          (list (lambda (_) (cons 'transient nael-rpc-live-root))))
    (setq nael-rpc-live-buffer
          (find-file-noselect (concat nael-rpc-live-root "Tiny/A.lean")))
    (with-current-buffer nael-rpc-live-buffer
      (nael-mode)
      (let ((default-directory nael-rpc-live-root))
        (apply #'eglot (eglot--guess-contact)))
      (should (nael-rpc-live-wait #'eglot-managed-p 30))))
  (with-current-buffer nael-rpc-live-buffer
    (unless (equal (buffer-string) nael-rpc-live-text)
      (erase-buffer)
      (insert nael-rpc-live-text)
      (set-buffer-modified-p nil)
      (nael-eglot-flush-changes)
      (setq nael-eglot-file-progress nil))
    (should (nael-rpc-live-wait #'nael-rpc-live-processed-p 60)))
  nael-rpc-live-buffer)

(defun nael-rpc-live-goto (line column)
  "Go to 1-based LINE and 0-based COLUMN."
  (goto-char (point-min))
  (forward-line (1- line))
  (move-to-column column))

(defun nael-rpc-live-sync (call)
  "Call CALL with success and failure functions; wait; return the outcome.
The outcome is (ok . RESULT), (error . ERROR) or nil after 30 seconds."
  (let (outcome)
    (funcall call
             (lambda (result) (setq outcome (cons 'ok result)))
             (lambda (error) (setq outcome (cons 'error error))))
    (nael-rpc-live-wait (lambda () outcome) 30)
    outcome))

(defun nael-rpc-live-goals (handle &optional owner)
  "Return the outcome of a goal request through HANDLE for OWNER."
  (nael-rpc-live-sync
   (lambda (ok fail)
     (nael-rpc-get-interactive-goals handle ok :failure fail :owner owner))))

(defun nael-rpc-live-target (outcome)
  "Return the target of the first goal in OUTCOME as a string."
  (when-let* ((goals (plist-get (cdr outcome) :goals))
              ((> (length goals) 0)))
    (nael-render--plain-text (plist-get (aref goals 0) :type))))

(defun nael-rpc-live-first-info (tagged)
  "Return the first subterm reference in TAGGED, a `CodeWithInfos'."
  (cond ((plist-member tagged :tag)
         (plist-get (aref (plist-get tagged :tag) 0) :info))
        ((plist-member tagged :append)
         (seq-some #'nael-rpc-live-first-info (plist-get tagged :append)))))

(ert-deftest nael-rpc-live-goals ()
  "Interactive goals arrive with their references."
  (with-current-buffer (nael-rpc-live-buffer)
    (nael-rpc-live-goto 4 2)
    (let* ((owner (nael-rpc-make-owner))
           (outcome (nael-rpc-live-goals (nael-rpc-open) owner))
           (goal (aref (plist-get (cdr outcome) :goals) 0)))
      (should (eq (car outcome) 'ok))
      (should (equal (nael-rpc-live-target outcome) "n + 0 = 1"))
      (should (equal (seq-mapcat (lambda (hyp) (plist-get hyp :names))
                                 (plist-get goal :hyps) 'list)
                     '("n" "h")))
      (should (> (length (nael-rpc-owner-refs owner)) 3))
      (nael-rpc-owner-release owner))))

(ert-deftest nael-rpc-live-release ()
  "A reference works until its owner is released."
  (with-current-buffer (nael-rpc-live-buffer)
    (nael-rpc-live-goto 4 2)
    (let* ((handle (nael-rpc-open))
           (owner (nael-rpc-make-owner))
           (goal (aref (plist-get (cdr (nael-rpc-live-goals handle owner))
                                  :goals)
                       0))
           (info (nael-rpc-live-first-info (plist-get goal :type)))
           (hover (lambda ()
                    (nael-rpc-live-sync
                     (lambda (ok fail)
                       (nael-rpc-info-to-interactive handle info ok
                                                     :failure fail))))))
      (should (eq (car (funcall hover)) 'ok))
      (nael-rpc-owner-release owner)
      (nael-rpc--flush-garbage (nael-rpc-handle-session handle))
      (let ((outcome (funcall hover)))
        (should (eq (car outcome) 'error))
        (should (equal (nael-rpc-error-code (cdr outcome)) -32602))))))

(ert-deftest nael-rpc-live-queued ()
  "Calls made before the handshake has settled are answered."
  (with-current-buffer (nael-rpc-live-buffer)
    (nael-rpc-close-all)
    (nael-rpc-live-goto 4 2)
    (let ((handle (nael-rpc-open))
          (answers 0))
      (should-not (nael-rpc-session-settled (nael-rpc-handle-session handle)))
      (dotimes (_ 2)
        (nael-rpc-get-interactive-goals handle (lambda (_) (cl-incf answers))))
      (should (nael-rpc-live-wait (lambda () (= answers 2)) 30)))))

(ert-deftest nael-rpc-live-reconnect ()
  "A session the server does not know is replaced transparently."
  (with-current-buffer (nael-rpc-live-buffer)
    (nael-rpc-live-goto 4 2)
    (let ((handle (nael-rpc-open)))
      (should (eq (car (nael-rpc-live-goals handle)) 'ok))
      (setf (nael-rpc-session-id (nael-rpc-handle-session handle)) "123")
      (let ((outcome (nael-rpc-live-goals handle)))
        (should (eq (car outcome) 'ok))
        (should (equal (nael-rpc-live-target outcome) "n + 0 = 1"))
        (should-not (equal (nael-rpc-session-id
                            (nael-rpc-handle-session handle))
                           "123"))))))

(ert-deftest nael-rpc-live-slot ()
  "Of two calls in one slot, only the second is delivered."
  (with-current-buffer (nael-rpc-live-buffer)
    (nael-rpc-live-goto 4 2)
    (let ((handle (nael-rpc-open))
          delivered)
      (nael-rpc-get-interactive-goals
       handle (lambda (_) (push 'first delivered)) :slot 'goals)
      (nael-rpc-get-interactive-goals
       handle (lambda (_) (push 'second delivered)) :slot 'goals)
      (should (nael-rpc-live-wait (lambda () delivered) 30))
      ;; Give a late answer to the first call time to arrive.
      (nael-rpc-live-wait #'ignore 1)
      (should (equal delivered '(second))))))

(ert-deftest nael-rpc-live-edit-before-call ()
  "A call right after an edit is answered for the edited text."
  (with-current-buffer (nael-rpc-live-buffer)
    (nael-rpc-live-goto 2 0)
    (insert "\n\n\n")
    ;; The line of `rw' moved from 4 to 7; without the edit, line 7 is
    ;; past the end of the proof.
    (nael-rpc-live-goto 7 2)
    (let ((outcome (nael-rpc-live-goals (nael-rpc-open))))
      (should (eq (car outcome) 'ok))
      (should (equal (nael-rpc-live-target outcome) "n + 0 = 1")))))

(ert-deftest nael-rpc-live-import-change ()
  "A call survives the restart of the file worker after an import edit."
  (with-current-buffer (nael-rpc-live-buffer)
    (nael-rpc-live-goto 4 2)
    (let* ((handle (nael-rpc-open))
           (id (progn (should (eq (car (nael-rpc-live-goals handle)) 'ok))
                      (nael-rpc-session-id (nael-rpc-handle-session handle)))))
      (goto-char (point-min))
      (insert "import Init\n")
      (nael-rpc-live-goto 5 2)
      (let* ((handle (nael-rpc-open))
             (outcome (nael-rpc-live-goals handle)))
        (should (eq (car outcome) 'ok))
        (should (equal (nael-rpc-live-target outcome) "n + 0 = 1"))
        (should-not (equal (nael-rpc-session-id
                            (nael-rpc-handle-session handle))
                           id))))))

(ert-deftest nael-rpc-live-keepalive ()
  "A session stays alive beyond the server's 30 second expiry."
  (skip-unless (getenv "NAEL_TEST_SLOW"))
  (with-current-buffer (nael-rpc-live-buffer)
    (nael-rpc-live-goto 4 2)
    (let* ((handle (nael-rpc-open))
           (id (progn (should (eq (car (nael-rpc-live-goals handle)) 'ok))
                      (nael-rpc-session-id (nael-rpc-handle-session handle)))))
      (nael-rpc-live-wait #'ignore 35)
      (should (eq (car (nael-rpc-live-goals handle)) 'ok))
      (should (equal (nael-rpc-session-id (nael-rpc-handle-session handle))
                     id)))))

(provide 'nael-rpc-live)

;;; nael-rpc-live.el ends here
