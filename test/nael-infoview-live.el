;;; nael-infoview-live.el --- Tests for nael-infoview against Lean  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see nael/LICENSE.GPL3.

;;; Commentary:

;; Tests for `nael-infoview' against a real Lean server on the Lake
;; project in `test/lean/'.  They reuse the setup of `nael-rpc-live'.
;; There is no command loop in batch mode, so the tests move point and
;; then run `nael-infoview--post-command' themselves.
;;
;; Run with: emacs -Q --batch -l build.el test-live

;;; Code:

(require 'ert)
(require 'nael-infoview)
(require 'nael-rpc-live)

(defun nael-infoview-live-text ()
  "Return the text of the infoview without properties."
  (with-current-buffer (nael-infoview--buffer)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun nael-infoview-live-status ()
  "Return the status of the infoview."
  (buffer-local-value 'nael-infoview--status (nael-infoview--buffer)))

(defun nael-infoview-live-wait-for (string)
  "Wait until the infoview is up to date and contains STRING."
  (nael-rpc-live-wait
   (lambda ()
     (and (null (nael-infoview-live-status))
          (string-search string (nael-infoview-live-text))))
   30))

(defun nael-infoview-live-move (line column)
  "Move point to LINE and COLUMN as a command would."
  (nael-rpc-live-goto line column)
  (nael-infoview--post-command))

(defun nael-infoview-live-settle ()
  "Wait until the current throttling period of the infoview has ended."
  (should (nael-rpc-live-wait (lambda () (null nael-infoview--timer)) 1)))

(defun nael-infoview-live-open ()
  "Return the test buffer with the infoview displayed for it.
The buffer is current, and point is before `rw'."
  (let ((buffer (nael-rpc-live-buffer)))
    (set-buffer buffer)
    (switch-to-buffer buffer)
    (unless (nael-infoview-visible-p)
      (nael-infoview))
    (nael-infoview-live-move 4 2)
    (should (nael-infoview-live-wait-for "⊢ n + 0 = 1"))
    (nael-infoview-live-settle)
    buffer))

(ert-deftest nael-infoview-live-follows-point ()
  "The infoview shows the goals at point and follows it."
  (with-current-buffer (nael-infoview-live-open)
    ;; The proof is complete, which a line above the goals says.
    (should (string-match-p
             "\\`Goals accomplished.*\nTactic state: 1 goal\nn : Nat\nh : n = b\n"
             (nael-infoview-live-text)))
    (nael-infoview-live-move 5 2)
    (should (nael-infoview-live-wait-for "⊢ b + 0 = 1"))
    ;; Outside the proof: no goals, no expected type.
    (nael-infoview-live-move 1 0)
    (should (nael-infoview-live-wait-for "No goals or expected type"))))

(ert-deftest nael-infoview-live-releases-previous ()
  "References of the previous content are released once it is replaced."
  (with-current-buffer (nael-infoview-live-open)
    (let ((owner (buffer-local-value 'nael-infoview--owner
                                     (nael-infoview--buffer))))
      (should (nael-rpc-owner-refs owner))
      (should-not (nael-rpc-owner-released owner))
      (nael-infoview-live-move 5 2)
      (should (nael-infoview-live-wait-for "⊢ b + 0 = 1"))
      (should (nael-rpc-owner-released owner))
      (should-not (nael-rpc-owner-released
                   (buffer-local-value 'nael-infoview--owner
                                       (nael-infoview--buffer)))))))

(ert-deftest nael-infoview-live-processing ()
  "While the line of point is being processed, nothing is requested."
  (with-current-buffer (nael-infoview-live-open)
    (let ((text (nael-infoview-live-text))
          (request (alist-get 'infoview-goals nael-rpc--slots)))
      (unwind-protect
          (progn
            (setq nael-eglot-file-progress
                  [(:range (:start (:line 2 :character 0)
                            :end (:line 5 :character 0))
                           :kind 1)])
            (nael-infoview-live-move 5 2)
            (should (eq (nael-infoview-live-status) 'processing))
            (should (eq (alist-get 'infoview-goals nael-rpc--slots) request))
            (should (equal (nael-infoview-live-text) text))
            (should (buffer-local-value 'nael-infoview--dim-cookie
                                        (nael-infoview--buffer))))
        ;; The server reports that processing ended.
        (setq nael-eglot-file-progress [])
        (run-hook-with-args 'nael-eglot-file-progress-functions []))
      (should (nael-infoview-live-wait-for "⊢ b + 0 = 1"))
      (should-not (buffer-local-value 'nael-infoview--dim-cookie
                                      (nael-infoview--buffer))))))

(ert-deftest nael-infoview-live-no-progress-yet ()
  "Without any progress report, goals are requested rather than waited for."
  (with-current-buffer (nael-infoview-live-open)
    (let ((generation nael-infoview--generation))
      (unwind-protect
          (progn
            (setq nael-eglot-file-progress nil)
            (nael-infoview-live-move 5 2)
            (should (= nael-infoview--generation (1+ generation)))
            (should (nael-infoview-live-wait-for "⊢ b + 0 = 1")))
        (setq nael-eglot-file-progress [])))))

(ert-deftest nael-infoview-live-refresh-while-processing ()
  "Refreshing requests goals even while the server reports processing."
  (with-current-buffer (nael-infoview-live-open)
    (unwind-protect
        (progn
          (setq nael-eglot-file-progress
                [(:range (:start (:line 2 :character 0)
                          :end (:line 5 :character 0))
                         :kind 1)])
          (nael-infoview-live-move 5 2)
          (should (eq (nael-infoview-live-status) 'processing))
          (nael-infoview-refresh)
          (should (nael-infoview-live-wait-for "⊢ b + 0 = 1")))
      (setq nael-eglot-file-progress []))))

(ert-deftest nael-infoview-live-local-hooks ()
  "The infoview hooks into Nael buffers only, and leaves them when killed."
  (let ((buffer (nael-infoview-live-open)))
    (should (memq #'nael-infoview--post-command
                  (buffer-local-value 'post-command-hook buffer)))
    (should-not (memq #'nael-infoview--post-command
                      (default-value 'post-command-hook)))
    (with-temp-buffer
      (should-not (memq #'nael-infoview--post-command post-command-hook)))
    (kill-buffer (nael-infoview--buffer))
    (should-not (memq #'nael-infoview--post-command
                      (buffer-local-value 'post-command-hook buffer)))
    (should-not (memq #'nael-infoview--track eglot-managed-mode-hook))))

(ert-deftest nael-infoview-live-expected-type-hidden ()
  "A hidden expected type is not even requested."
  (with-current-buffer (nael-infoview-live-open)
    ;; Inside `[h]' there is an expected type.
    (nael-infoview-live-move 4 6)
    (should (nael-infoview-live-wait-for "Expected type"))
    (nael-infoview-live-settle)
    (let ((nael-infoview-expected-type 'hidden)
          (before (alist-get 'infoview-term-goal nael-rpc--slots)))
      (nael-infoview-live-move 4 7)
      (should (nael-rpc-live-wait
               (lambda ()
                 (and (null (nael-infoview-live-status))
                      (not (string-search "Expected type"
                                          (nael-infoview-live-text)))))
               30))
      (should (string-prefix-p "Goals accomplished"
                               (nael-infoview-live-text)))
      (should (eq (alist-get 'infoview-term-goal nael-rpc--slots) before)))))

(defun nael-infoview-live-large-goal (n)
  "Return a theorem whose goal has N variables, N hypotheses and a sum."
  (concat "theorem large "
          (mapconcat (lambda (i) (format "(x%d : Nat)" i)) (number-sequence 0 (1- n)) " ")
          " "
          (mapconcat (lambda (i) (format "(h%d : x%d = %d)" i i i))
                     (number-sequence 0 (1- n)) " ")
          " : "
          (mapconcat (lambda (i) (format "x%d" i)) (number-sequence 0 (1- n)) " + ")
          (format " = %d := by\n  sorry\n" (/ (* n (1- n)) 2))))

(ert-deftest nael-infoview-live-large-goal ()
  "A goal with hundreds of hypotheses renders in well under a second."
  (with-current-buffer (nael-infoview-live-open)
    (goto-char (point-max))
    (insert "\n" (nael-infoview-live-large-goal 300))
    (nael-eglot-flush-changes)
    (setq nael-eglot-file-progress nil)
    (should (nael-rpc-live-wait #'nael-rpc-live-processed-p 120))
    (goto-char (point-max))
    (forward-line -1)
    (back-to-indentation)
    (nael-infoview--post-command)
    (should (nael-infoview-live-wait-for "h299 : x299 = 299"))
    (with-current-buffer (nael-infoview--buffer)
      (let* ((results nael-infoview--results)
             (render (car (benchmark-run 5
                            (apply #'nael-render-goals
                                   (append results
                                           (list (nael-infoview--options)))))))
             (show (car (benchmark-run 5 (nael-infoview--render))))
             (nodes (length (overlays-in (point-min) (point-max)))))
        (message "Large goal: %d characters, %d overlays; render %.1f ms, \
render into buffer %.1f ms"
                 (buffer-size) nodes (* 200 render) (* 200 show))
        (should (> nodes 900))
        (should (< (/ show 5) 0.5))))))

(ert-deftest nael-infoview-live-throttle ()
  "Updates in quick succession are coalesced into a trailing one."
  (with-current-buffer (nael-infoview-live-open)
    (let ((generation nael-infoview--generation))
      (nael-infoview-live-move 5 2)
      (nael-infoview-live-move 4 3)
      (nael-infoview-live-move 5 2)
      ;; The first update ran at once; the others wait for the timer.
      (should (= nael-infoview--generation (1+ generation)))
      (should nael-infoview--again)
      (should (nael-rpc-live-wait
               (lambda () (= nael-infoview--generation (+ 2 generation)))
               1))
      (should (nael-infoview-live-wait-for "⊢ b + 0 = 1")))))

(ert-deftest nael-infoview-live-edit ()
  "After an edit, the infoview shows the goals for the edited text."
  (with-current-buffer (nael-infoview-live-open)
    (nael-rpc-live-goto 4 0)
    (insert "  have hb : b = 1 := rfl\n")
    (nael-infoview-live-move 5 2)
    (should (nael-infoview-live-wait-for "hb : b = 1"))))

(ert-deftest nael-infoview-live-window-point ()
  "The infoview window keeps its point at the same goal line."
  (with-current-buffer (nael-infoview-live-open)
    (let ((window (nael-infoview-visible-p)))
      (with-selected-window window
        (goto-char (point-min))
        (search-forward "h : ")
        (set-window-point window (point)))
      (nael-infoview-live-move 4 3)
      (nael-infoview-live-settle)
      (should (nael-infoview-live-wait-for "⊢ n + 0 = 1"))
      (should (equal (with-selected-window window
                       (buffer-substring-no-properties
                        (line-beginning-position) (point)))
                     "h : ")))))

(ert-deftest nael-infoview-live-eldoc ()
  "While the infoview is visible, ElDoc requests no goals."
  (with-current-buffer (nael-infoview-live-open)
    (should (nael-infoview-shows-goals-p))
    (should-not (nael-eglot-eldoc-goal #'ignore))
    (should-not (nael-eglot-eldoc-term-goal #'ignore))
    (nael-infoview)
    (should-not (nael-infoview-visible-p))
    (should (nael-eglot-eldoc-goal #'ignore))))

(provide 'nael-infoview-live)

;;; nael-infoview-live.el ends here
