;;; lena-margin-live.el --- Tests for lena-margin against Lean  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see lena/LICENSE.GPL3.

;;; Commentary:

;; Tests for `lena-margin-mode' against a real Lean server on the Lake
;; project in `test/lean/'.  They reuse the setup of `lena-rpc-live'
;; and open `Tiny/Signs.lean', which has a complete proof, one with
;; unsolved goals, one that uses `sorry' and two errors, and
;; `Tiny/Slow.lean', which keeps the server busy for a few seconds.
;;
;; Run with: emacs -Q --batch -l build.el test-live

;;; Code:

(require 'ert)
(require 'lena-margin)
(require 'lena-infoview-live)
(require 'lena-rpc-live)
(require 'lena-test-helpers)

(defvar lena-margin-live-texts nil
  "The original contents of the test files, by file name.")

(defun lena-margin-live-open (file)
  "Return the buffer of FILE in the test project, managed and processed.
Its contents are as they were when it was first opened in the session."
  (lena-rpc-live-buffer)
  (let ((buffer (find-file-noselect (concat lena-rpc-live-root file))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'lena-mode)
        (lena-mode)
        (should (lena-rpc-live-wait #'eglot-managed-p 30)))
      (let ((text (or (cdr (assoc file lena-margin-live-texts))
                      (cdar (push (cons file (buffer-string))
                                  lena-margin-live-texts)))))
        (unless (equal (buffer-string) text)
          (erase-buffer)
          (insert text)
          (set-buffer-modified-p nil)
          (lena-eglot-flush-changes)
          (setq lena-eglot-file-progress nil)))
      (should (lena-rpc-live-wait #'lena-rpc-live-processed-p 60))
      (lena-margin-live-settle))
    buffer))

(defun lena-margin-live-settle ()
  "Wait until the signs are drawn after the latest report."
  (should (lena-rpc-live-wait (lambda () (null lena-margin--timer)) 2)))

(defun lena-margin-live-signs ()
  "Draw the signs as they would be with all characters displayable.
Return them as (LINE STRING), LINE 0-based."
  (lena-test-with-margin-defaults
    (lena-margin-draw)
    (lena-test-margin-drawn)))

(defconst lena-margin-live-expected
  '((0 "✓")                             ; done
    (3 "┌") (4 "│") (5 "✗")             ; open_goal, unsolved
    (7 "⚠")                             ; admitted, sorry
    (11 "┌") (12 "│") (13 "└")          ; example, failed instance
    (15 "‼"))                           ; #check
  "The signs of `Tiny/Signs.lean'.")

(ert-deftest lena-margin-live-signs ()
  "The server's diagnostics give the expected signs."
  (with-current-buffer (lena-margin-live-open "Tiny/Signs.lean")
    (should lena-margin-mode)
    ;; One column, or two where `‼' falls back to `!!', as in batch mode.
    (should (eql left-margin-width (lena-margin--needed-width)))
    (should (equal (lena-margin-live-signs) lena-margin-live-expected))))

(ert-deftest lena-margin-live-flymake ()
  "Flymake gets the diagnostics without the silent one, and no indicators."
  (with-current-buffer (lena-margin-live-open "Tiny/Signs.lean")
    (should (seq-some (lambda (diagnostic)
                        (lena-eglot-diagnostic-tag-p diagnostic
                                                     'goals-accomplished))
                      lena-eglot-diagnostics))
    (should (lena-rpc-live-wait
             (lambda () (= (length (flymake-diagnostics)) 4)) 10))
    (should-not (seq-some (lambda (diagnostic)
                            (string-search "Goals accomplished"
                                           (flymake-diagnostic-text
                                            diagnostic)))
                          (flymake-diagnostics)))
    (let ((overlays (seq-filter (lambda (overlay)
                                  (overlay-get overlay 'flymake-overlay))
                                (overlays-in (point-min) (point-max)))))
      (should (= (length overlays) 4))
      (dolist (overlay overlays)
        (when-let* ((string (overlay-get overlay 'before-string)))
          (should (lena-test-empty-display-p
                   (get-text-property 0 'display string))))))))

(ert-deftest lena-margin-live-edit ()
  "After an edit, the signs follow the server's new diagnostics."
  (with-current-buffer (lena-margin-live-open "Tiny/Signs.lean")
    (goto-char (point-min))
    (insert "-- A comment\n")
    (lena-eglot-flush-changes)
    (should (lena-rpc-live-wait
             (lambda ()
               (and (lena-rpc-live-processed-p)
                    (equal (car (lena-eglot-diagnostic-lines
                                 (seq-find
                                  (lambda (diagnostic)
                                    (lena-eglot-diagnostic-tag-p
                                     diagnostic 'goals-accomplished))
                                  lena-eglot-diagnostics)))
                           1)))
             30))
    (lena-margin-live-settle)
    (should (equal (lena-margin-live-signs)
                   (mapcar (pcase-lambda (`(,line ,string))
                             (list (1+ line) string))
                           lena-margin-live-expected)))
    ;; Completing the open proof turns its bar into a complete proof.
    (goto-char (point-min))
    (search-forward "· omega")
    (insert "\n  · trivial")
    (lena-eglot-flush-changes)
    (should (lena-rpc-live-wait
             (lambda ()
               (and (lena-rpc-live-processed-p)
                    (= (seq-count (lambda (diagnostic)
                                    (lena-eglot-diagnostic-tag-p
                                     diagnostic 'goals-accomplished))
                                  lena-eglot-diagnostics)
                       2)))
             30))
    (lena-margin-live-settle)
    (should (equal (seq-take (lena-margin-live-signs) 3)
                   '((1 "✓") (4 "✓") (9 "⚠"))))))

(ert-deftest lena-margin-live-mode-change ()
  "Setting up the major mode again leaves one set of signs."
  (with-current-buffer (lena-margin-live-open "Tiny/Signs.lean")
    (lena-mode)
    (should (lena-rpc-live-wait #'eglot-managed-p 30))
    (setq lena-eglot-file-progress nil)
    (should (lena-rpc-live-wait
             (lambda () (and (lena-rpc-live-processed-p)
                             (seq-some (lambda (diagnostic)
                                         (lena-eglot-diagnostic-tag-p
                                          diagnostic 'goals-accomplished))
                                       lena-eglot-diagnostics)))
             60))
    (lena-margin-live-settle)
    (should lena-margin-mode)
    (should (equal (lena-margin-live-signs) lena-margin-live-expected))
    (should (= (seq-count (lambda (overlay) (overlay-get overlay 'lena-margin))
                          (overlays-in (point-min) (point-max)))
               (length lena-margin-live-expected)))))

(ert-deftest lena-margin-live-processing ()
  "Lines being processed get a bar until the server is done with them."
  (with-current-buffer (lena-margin-live-open "Tiny/Slow.lean")
    ;; Make the server process the file again.
    (goto-char (point-min))
    (insert "-- Again\n")
    (lena-eglot-flush-changes)
    (setq lena-eglot-file-progress nil)
    (should (lena-rpc-live-wait
             (lambda ()
               (and (not lena-margin--timer)
                    (assoc 5 (lena-margin-live-signs))
                    (equal (cadr (assoc 5 (lena-margin-live-signs))) "│")))
             10))
    (should (lena-rpc-live-wait #'lena-rpc-live-processed-p 30))
    (lena-margin-live-settle)
    (should (equal (lena-margin-live-signs) '((1 "✓") (5 "✓"))))))

(provide 'lena-margin-live)

;;; lena-margin-live.el ends here
