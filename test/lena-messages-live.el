;;; lena-messages-live.el --- Tests for messages in the infoview against Lean  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see lena/LICENSE.GPL3.

;;; Commentary:

;; Tests for the messages in `lena-infoview' against a real Lean server
;; on the Lake project in `test/lean/', on `Tiny/Messages.lean': a
;; `#check', a proof with unsolved goals, a trace, `exact?', two of
;; Lean's hints, and an error with an explanation.  The
;; toolchain of the project is recent, so the server sends widgets.
;;
;; Run with: emacs -Q --batch -l build.el test-live

;;; Code:

(require 'ert)
(require 'lena-infoview-live)
(require 'lena-margin-live)
(require 'lena-test-helpers)

(defun lena-messages-live-open ()
  "Return `Tiny/Messages.lean', processed and current, with the infoview."
  (let ((buffer (lena-margin-live-open "Tiny/Messages.lean")))
    (set-buffer buffer)
    (switch-to-buffer buffer)
    (unless (lena-infoview-visible-p)
      (lena-infoview))
    buffer))

(defun lena-messages-live-move (line column string)
  "Move to LINE and COLUMN and wait until the infoview shows STRING."
  (lena-infoview-live-move line column)
  (should (lena-infoview-live-wait-for string))
  (lena-infoview-live-settle))

(defun lena-messages-live-visible-p (string)
  "Return non-nil if the infoview shows STRING unfolded."
  (with-current-buffer (lena-infoview--buffer)
    (lena-test-visible-p string)))

(ert-deftest lena-messages-live-widgets ()
  "The server of the test project is asked for widgets."
  (should (lena-eglot-widgets-p lena-rpc-live-root))
  (with-current-buffer (lena-messages-live-open)
    (should (lena-rpc-live-wait
             (lambda ()
               (seq-some (lambda (diagnostic)
                           (equal (flymake-diagnostic-text diagnostic)
                                  "Lean 4: (trace)"))
                         (flymake-diagnostics)))
             10))))

(ert-deftest lena-messages-live-at-point ()
  "The messages of the line of point, folded if they start elsewhere."
  (with-current-buffer (lena-messages-live-open)
    (lena-messages-live-move 1 0 "Messages (1)")
    (should (lena-messages-live-visible-p
             "Nat.add_comm (n m : Nat) : n + m = m + n"))
    ;; Inside the proof with unsolved goals: its error, folded.
    (lena-messages-live-move 5 4 "3:55 error")
    (should (string-prefix-p "Tactic state: 1 goal\ncase left"
                             (lena-infoview-live-text)))
    (should-not (lena-messages-live-visible-p "unsolved goals"))
    ;; Where it starts, unfolded, with its goal.
    (lena-messages-live-move 3 56 "unsolved goals")
    (should (lena-messages-live-visible-p "unsolved goals"))
    (should (lena-messages-live-visible-p "case right"))
    ;; Without messages, no section.
    (lena-messages-live-move 2 0 "No goals")
    (should-not (string-search "Messages" (lena-infoview-live-text)))))

(ert-deftest lena-messages-live-subterms ()
  "Subterms in messages can be described like those in goals."
  (with-current-buffer (lena-messages-live-open)
    (lena-messages-live-move 1 0 "Messages (1)")
    (with-current-buffer (lena-infoview--buffer)
      (lena-test-goto "n + m = m + n")
      (let ((node (lena-render-node-at (point)))
            text)
        (should node)
        (should (lena-infoview-eldoc-function
                 (lambda (doc &rest _) (setq text doc))))
        (should (lena-rpc-live-wait (lambda () text) 10))
        ;; The innermost subterm at the start of the equation.
        (should (string-search "n : Nat" text))))))

(ert-deftest lena-messages-live-trace ()
  "A trace's children are fetched on request, and their references kept."
  (with-current-buffer (lena-messages-live-open)
    (lena-messages-live-move 8 0 "[Meta.synthInstance]")
    (with-current-buffer (lena-infoview--buffer)
      (let* ((owner lena-infoview--owner)
             (refs (length (lena-rpc-owner-refs owner))))
        (lena-test-goto "[Meta.synthInstance]")
        (should (get-text-property (point) 'lena-trace-lazy))
        (lena-infoview-expand-trace)
        (should (lena-rpc-live-wait
                 (lambda () (lena-messages-live-visible-p "instInhabitedNat"))
                 10))
        ;; The children's references belong to the shown rendering.
        (should (> (length (lena-rpc-owner-refs owner)) refs))
        ;; Moving away releases them with the rest.
        (with-current-buffer (car lena-infoview--target)
          (lena-messages-live-move 1 0 "Messages (1)"))
        (should (lena-rpc-owner-released owner))))))

(ert-deftest lena-messages-live-apply ()
  "\"Try this\" replaces the tactic in the source."
  (with-current-buffer (lena-messages-live-open)
    (lena-messages-live-move 11 2 "[apply]")
    (with-current-buffer (lena-infoview--buffer)
      (lena-test-goto "[apply]")
      (lena-infoview-apply-suggestion))
    (should (equal (buffer-name) "Messages.lean"))
    (should (equal (buffer-substring (line-beginning-position)
                                     (line-end-position))
                   "  exact Nat.add_comm a b"))))

(ert-deftest lena-messages-live-hints ()
  "Lean's hints with diffs can be applied; error codes link to explanations."
  (with-current-buffer (lena-messages-live-open)
    (lena-messages-live-move 18 29 "[lean.invalidField]")
    (with-current-buffer (lena-infoview--buffer)
      (lena-test-goto "[lean.invalidField]")
      (should (string-search "lean.invalidField"
                             (get-text-property (point) 'lena-explanation))))
    (lena-messages-live-move 14 0 "end Probe")
    (with-current-buffer (lena-infoview--buffer)
      (lena-test-goto "end Prob")
      (should (get-text-property (point) 'lena-suggestion))
      (lena-infoview-apply-suggestion))
    (should (equal (buffer-name) "Messages.lean"))
    (should (equal (buffer-substring (line-beginning-position)
                                     (line-end-position))
                   "end Probe"))))

(provide 'lena-messages-live)

;;; lena-messages-live.el ends here
