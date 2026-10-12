;;; lena-infoview-live.el --- Tests for lena-infoview against Lean  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see lena/LICENSE.GPL3.

;;; Commentary:

;; Tests for `lena-infoview' against a real Lean server on the Lake
;; project in `test/lean/'.  They reuse the setup of `lena-rpc-live'.
;; There is no command loop in batch mode, so the tests move point and
;; then run `lena-infoview--post-command' themselves.
;;
;; Run with: emacs -Q --batch -l build.el test-live

;;; Code:

(require 'ert)
(require 'lena-infoview)
(require 'lena-rpc-live)

(defun lena-infoview-live-text ()
  "Return the text of the infoview without properties."
  (with-current-buffer (lena-infoview--buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun lena-infoview-live-status ()
  "Return the status of the infoview."
  (buffer-local-value 'lena-infoview--status (lena-infoview--buffer)))

(defun lena-infoview-live-wait-for (string)
  "Wait until the infoview is up to date and contains STRING."
  (lena-rpc-live-wait
   (lambda ()
     (and (null (lena-infoview-live-status))
          (string-search string (lena-infoview-live-text))))
   30))

(defun lena-infoview-live-move (line column)
  "Move point to LINE and COLUMN as a command would."
  (lena-rpc-live-goto line column)
  (lena-infoview--post-command))

(defun lena-infoview-live-settle ()
  "Wait until the current throttling period of the infoview has ended."
  (should (lena-rpc-live-wait (lambda () (null lena-infoview--timer)) 1)))

(defun lena-infoview-live-open ()
  "Return the test buffer with the infoview displayed for it.
The buffer is current, and point is before `rw'."
  (let ((buffer (lena-rpc-live-buffer)))
    (set-buffer buffer)
    (switch-to-buffer buffer)
    (unless (lena-infoview-visible-p)
      (lena-infoview))
    (lena-infoview-live-move 4 2)
    (should (lena-infoview-live-wait-for "⊢ n + 0 = 1"))
    (lena-infoview-live-settle)
    buffer))

(ert-deftest lena-infoview-live-follows-point ()
  "The infoview shows the goals at point and follows it."
  (with-current-buffer (lena-infoview-live-open)
    ;; The proof is complete, which a line above the goals says.
    (should (string-match-p
             "\\`Goals accomplished.*\nTactic state: 1 goal\nn : Nat\nh : n = b\n"
             (lena-infoview-live-text)))
    (lena-infoview-live-move 5 2)
    (should (lena-infoview-live-wait-for "⊢ b + 0 = 1"))
    ;; Outside the proof: no goals, no expected type.
    (lena-infoview-live-move 1 0)
    (should (lena-infoview-live-wait-for "No goals or expected type"))))

(ert-deftest lena-infoview-live-releases-previous ()
  "References of the previous content are released once it is replaced."
  (with-current-buffer (lena-infoview-live-open)
    (let ((owner (buffer-local-value 'lena-infoview--owner
                                     (lena-infoview--buffer))))
      (should (lena-rpc-owner-refs owner))
      (should-not (lena-rpc-owner-released owner))
      (lena-infoview-live-move 5 2)
      (should (lena-infoview-live-wait-for "⊢ b + 0 = 1"))
      (should (lena-rpc-owner-released owner))
      (should-not (lena-rpc-owner-released
                   (buffer-local-value 'lena-infoview--owner
                                       (lena-infoview--buffer)))))))

(ert-deftest lena-infoview-live-processing ()
  "While the line of point is being processed, nothing is requested."
  (with-current-buffer (lena-infoview-live-open)
    (let ((text (lena-infoview-live-text))
          (request (alist-get 'infoview-goals lena-rpc--slots)))
      (unwind-protect
          (progn
            (setq lena-eglot-file-progress
                  [(:range (:start (:line 2 :character 0)
                            :end (:line 5 :character 0))
                           :kind 1)])
            (lena-infoview-live-move 5 2)
            (should (eq (lena-infoview-live-status) 'processing))
            (should (eq (alist-get 'infoview-goals lena-rpc--slots) request))
            (should (equal (lena-infoview-live-text) text))
            (should (buffer-local-value 'lena-infoview--dim-cookie
                                        (lena-infoview--buffer))))
        ;; The server reports that processing ended.
        (setq lena-eglot-file-progress [])
        (run-hook-with-args 'lena-eglot-file-progress-functions []))
      (should (lena-infoview-live-wait-for "⊢ b + 0 = 1"))
      (should-not (buffer-local-value 'lena-infoview--dim-cookie
                                      (lena-infoview--buffer))))))

(ert-deftest lena-infoview-live-no-progress-yet ()
  "Without any progress report, goals are requested rather than waited for."
  (with-current-buffer (lena-infoview-live-open)
    (let ((generation lena-infoview--generation))
      (unwind-protect
          (progn
            (setq lena-eglot-file-progress nil)
            (lena-infoview-live-move 5 2)
            (should (= lena-infoview--generation (1+ generation)))
            (should (lena-infoview-live-wait-for "⊢ b + 0 = 1")))
        (setq lena-eglot-file-progress [])))))

(ert-deftest lena-infoview-live-refresh-while-processing ()
  "Refreshing requests goals even while the server reports processing."
  (with-current-buffer (lena-infoview-live-open)
    (unwind-protect
        (progn
          (setq lena-eglot-file-progress
                [(:range (:start (:line 2 :character 0)
                          :end (:line 5 :character 0))
                         :kind 1)])
          (lena-infoview-live-move 5 2)
          (should (eq (lena-infoview-live-status) 'processing))
          (lena-infoview-refresh)
          (should (lena-infoview-live-wait-for "⊢ b + 0 = 1")))
      (setq lena-eglot-file-progress []))))

(ert-deftest lena-infoview-live-local-hooks ()
  "The infoview hooks into Lena buffers only, and leaves them when killed."
  (let ((buffer (lena-infoview-live-open)))
    (should (memq #'lena-infoview--post-command
                  (buffer-local-value 'post-command-hook buffer)))
    (should-not (memq #'lena-infoview--post-command
                      (default-value 'post-command-hook)))
    (with-temp-buffer
      (should-not (memq #'lena-infoview--post-command post-command-hook)))
    (kill-buffer (lena-infoview--buffer))
    (should-not (memq #'lena-infoview--post-command
                      (buffer-local-value 'post-command-hook buffer)))
    (should-not (memq #'lena-infoview--track eglot-managed-mode-hook))))

(ert-deftest lena-infoview-live-expected-type-hidden ()
  "A hidden expected type is not even requested."
  (with-current-buffer (lena-infoview-live-open)
    ;; Inside `[h]' there is an expected type.
    (lena-infoview-live-move 4 6)
    (should (lena-infoview-live-wait-for "Expected type"))
    (lena-infoview-live-settle)
    (let ((lena-infoview-expected-type 'hidden)
          (before (alist-get 'infoview-term-goal lena-rpc--slots)))
      (lena-infoview-live-move 4 7)
      (should (lena-rpc-live-wait
               (lambda ()
                 (and (null (lena-infoview-live-status))
                      (not (string-search "Expected type"
                                          (lena-infoview-live-text)))))
               30))
      (should (string-prefix-p "Goals accomplished"
                               (lena-infoview-live-text)))
      (should (eq (alist-get 'infoview-term-goal lena-rpc--slots) before)))))

(defun lena-infoview-live-large-goal (n)
  "Return a theorem whose goal has N variables, N hypotheses and a sum."
  (concat "theorem large "
          (mapconcat (lambda (i) (format "(x%d : Nat)" i)) (number-sequence 0 (1- n)) " ")
          " "
          (mapconcat (lambda (i) (format "(h%d : x%d = %d)" i i i))
                     (number-sequence 0 (1- n)) " ")
          " : "
          (mapconcat (lambda (i) (format "x%d" i)) (number-sequence 0 (1- n)) " + ")
          (format " = %d := by\n  sorry\n" (/ (* n (1- n)) 2))))

(ert-deftest lena-infoview-live-large-goal ()
  "A goal with hundreds of hypotheses renders in well under a second.
The time is only checked if the environment variable LENA_TEST_TIMING
is set, since it depends on the machine."
  (with-current-buffer (lena-infoview-live-open)
    (goto-char (point-max))
    (insert "\n" (lena-infoview-live-large-goal 300))
    (lena-eglot-flush-changes)
    (setq lena-eglot-file-progress nil)
    (should (lena-rpc-live-wait #'lena-rpc-live-processed-p 120))
    (goto-char (point-max))
    (forward-line -1)
    (back-to-indentation)
    (lena-infoview--post-command)
    (should (lena-infoview-live-wait-for "h299 : x299 = 299"))
    (with-current-buffer (lena-infoview--buffer)
      (let* ((results lena-infoview--results)
             (render (car (benchmark-run 5
                            (apply #'lena-render-goals
                                   (append results
                                           (list (lena-infoview--options)))))))
             (show (car (benchmark-run 5 (lena-infoview--render))))
             (nodes (length (overlays-in (point-min) (point-max)))))
        (message "Large goal: %d characters, %d overlays; render %.1f ms, \
render into buffer %.1f ms"
                 (buffer-size) nodes (* 200 render) (* 200 show))
        (should (> nodes 900))
        (when (getenv "LENA_TEST_TIMING")
          (should (< (/ show 5) 0.5)))))))

(ert-deftest lena-infoview-live-throttle ()
  "Updates in quick succession are coalesced into a trailing one."
  (with-current-buffer (lena-infoview-live-open)
    (let ((generation lena-infoview--generation))
      (lena-infoview-live-move 5 2)
      (lena-infoview-live-move 4 3)
      (lena-infoview-live-move 5 2)
      ;; The first update ran at once; the others wait for the timer.
      (should (= lena-infoview--generation (1+ generation)))
      (should lena-infoview--again)
      (should (lena-rpc-live-wait
               (lambda () (= lena-infoview--generation (+ 2 generation)))
               1))
      (should (lena-infoview-live-wait-for "⊢ b + 0 = 1")))))

(ert-deftest lena-infoview-live-edit ()
  "After an edit, the infoview shows the goals for the edited text."
  (with-current-buffer (lena-infoview-live-open)
    (lena-rpc-live-goto 4 0)
    (insert "  have hb : b = 1 := rfl\n")
    (lena-infoview-live-move 5 2)
    (should (lena-infoview-live-wait-for "hb : b = 1"))))

(ert-deftest lena-infoview-live-window-point ()
  "The infoview window keeps its point at the same goal line."
  (with-current-buffer (lena-infoview-live-open)
    (let ((window (lena-infoview-visible-p)))
      (with-selected-window window
        (goto-char (point-min))
        (search-forward "h : ")
        (set-window-point window (point)))
      (lena-infoview-live-move 4 3)
      (lena-infoview-live-settle)
      (should (lena-infoview-live-wait-for "⊢ n + 0 = 1"))
      (should (equal (with-selected-window window
                       (buffer-substring-no-properties
                        (line-beginning-position) (point)))
                     "h : ")))))

(ert-deftest lena-infoview-live-eldoc ()
  "While the infoview is visible, ElDoc requests no goals."
  (with-current-buffer (lena-infoview-live-open)
    (should (lena-infoview-shows-goals-p))
    (should-not (lena-eglot-eldoc-goal #'ignore))
    (should-not (lena-eglot-eldoc-term-goal #'ignore))
    (lena-infoview)
    (should-not (lena-infoview-visible-p))
    (should (lena-eglot-eldoc-goal #'ignore))))

(provide 'lena-infoview-live)

;;; lena-infoview-live.el ends here
