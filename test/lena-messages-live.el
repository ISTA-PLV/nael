;;; nael-messages-live.el --- Tests for messages in the infoview against Lean  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see nael/LICENSE.GPL3.

;;; Commentary:

;; Tests for the messages in `nael-infoview' against a real Lean server
;; on the Lake project in `test/lean/', on `Tiny/Messages.lean': a
;; `#check', a proof with unsolved goals, a trace, `exact?', two of
;; Lean's hints, and an error with an explanation.  The
;; toolchain of the project is recent, so the server sends widgets.
;;
;; Run with: emacs -Q --batch -l build.el test-live

;;; Code:

(require 'ert)
(require 'nael-infoview-live)
(require 'nael-margin-live)
(require 'nael-test-helpers)

(defun nael-messages-live-open ()
  "Return `Tiny/Messages.lean', processed and current, with the infoview."
  (let ((buffer (nael-margin-live-open "Tiny/Messages.lean")))
    (set-buffer buffer)
    (switch-to-buffer buffer)
    (unless (nael-infoview-visible-p)
      (nael-infoview))
    buffer))

(defun nael-messages-live-move (line column string)
  "Move to LINE and COLUMN and wait until the infoview shows STRING."
  (nael-infoview-live-move line column)
  (should (nael-infoview-live-wait-for string))
  (nael-infoview-live-settle))

(defun nael-messages-live-visible-p (string)
  "Return non-nil if the infoview shows STRING unfolded."
  (with-current-buffer (nael-infoview--buffer)
    (nael-test-visible-p string)))

(ert-deftest nael-messages-live-widgets ()
  "The server of the test project is asked for widgets."
  (should (nael-eglot-widgets-p nael-rpc-live-root))
  (with-current-buffer (nael-messages-live-open)
    (should (nael-rpc-live-wait
             (lambda ()
               (seq-some (lambda (diagnostic)
                           (equal (flymake-diagnostic-text diagnostic)
                                  "Lean 4: (trace)"))
                         (flymake-diagnostics)))
             10))))

(ert-deftest nael-messages-live-at-point ()
  "The messages of the line of point, folded if they start elsewhere."
  (with-current-buffer (nael-messages-live-open)
    (nael-messages-live-move 1 0 "Messages (1)")
    (should (nael-messages-live-visible-p
             "Nat.add_comm (n m : Nat) : n + m = m + n"))
    ;; Inside the proof with unsolved goals: its error, folded.
    (nael-messages-live-move 5 4 "3:55 error")
    (should (string-prefix-p "Tactic state: 1 goal\ncase left"
                             (nael-infoview-live-text)))
    (should-not (nael-messages-live-visible-p "unsolved goals"))
    ;; Where it starts, unfolded, with its goal.
    (nael-messages-live-move 3 56 "unsolved goals")
    (should (nael-messages-live-visible-p "unsolved goals"))
    (should (nael-messages-live-visible-p "case right"))
    ;; Without messages, no section.
    (nael-messages-live-move 2 0 "No goals")
    (should-not (string-search "Messages" (nael-infoview-live-text)))))

(ert-deftest nael-messages-live-subterms ()
  "Subterms in messages can be described like those in goals."
  (with-current-buffer (nael-messages-live-open)
    (nael-messages-live-move 1 0 "Messages (1)")
    (with-current-buffer (nael-infoview--buffer)
      (nael-test-goto "n + m = m + n")
      (let ((node (nael-render-node-at (point)))
            text)
        (should node)
        (should (nael-infoview-eldoc-function
                 (lambda (doc &rest _) (setq text doc))))
        (should (nael-rpc-live-wait (lambda () text) 10))
        ;; The innermost subterm at the start of the equation.
        (should (string-search "n : Nat" text))))))

(ert-deftest nael-messages-live-trace ()
  "A trace's children are fetched on request, and their references kept."
  (with-current-buffer (nael-messages-live-open)
    (nael-messages-live-move 8 0 "[Meta.synthInstance]")
    (with-current-buffer (nael-infoview--buffer)
      (let* ((owner nael-infoview--owner)
             (refs (length (nael-rpc-owner-refs owner))))
        (nael-test-goto "[Meta.synthInstance]")
        (should (get-text-property (point) 'nael-trace-lazy))
        (nael-infoview-expand-trace)
        (should (nael-rpc-live-wait
                 (lambda () (nael-messages-live-visible-p "instInhabitedNat"))
                 10))
        ;; The children's references belong to the shown rendering.
        (should (> (length (nael-rpc-owner-refs owner)) refs))
        ;; Moving away releases them with the rest.
        (with-current-buffer (car nael-infoview--target)
          (nael-messages-live-move 1 0 "Messages (1)"))
        (should (nael-rpc-owner-released owner))))))

(ert-deftest nael-messages-live-apply ()
  "\"Try this\" replaces the tactic in the source."
  (with-current-buffer (nael-messages-live-open)
    (nael-messages-live-move 11 2 "[apply]")
    (with-current-buffer (nael-infoview--buffer)
      (nael-test-goto "[apply]")
      (nael-infoview-apply-suggestion))
    (should (equal (buffer-name) "Messages.lean"))
    (should (equal (buffer-substring (line-beginning-position)
                                     (line-end-position))
                   "  exact Nat.add_comm a b"))))

(ert-deftest nael-messages-live-hints ()
  "Lean's hints with diffs can be applied; error codes link to explanations."
  (with-current-buffer (nael-messages-live-open)
    (nael-messages-live-move 18 29 "[lean.invalidField]")
    (with-current-buffer (nael-infoview--buffer)
      (nael-test-goto "[lean.invalidField]")
      (should (string-search "lean.invalidField"
                             (get-text-property (point) 'nael-explanation))))
    (nael-messages-live-move 14 0 "end Probe")
    (with-current-buffer (nael-infoview--buffer)
      (nael-test-goto "end Prob")
      (should (get-text-property (point) 'nael-suggestion))
      (nael-infoview-apply-suggestion))
    (should (equal (buffer-name) "Messages.lean"))
    (should (equal (buffer-substring (line-beginning-position)
                                     (line-end-position))
                   "end Probe"))))

(provide 'nael-messages-live)

;;; nael-messages-live.el ends here
