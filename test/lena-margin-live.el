;;; nael-margin-live.el --- Tests for nael-margin against Lean  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see nael/LICENSE.GPL3.

;;; Commentary:

;; Tests for `nael-margin-mode' against a real Lean server on the Lake
;; project in `test/lean/'.  They reuse the setup of `nael-rpc-live'
;; and open `Tiny/Signs.lean', which has a complete proof, one with
;; unsolved goals, one that uses `sorry' and two errors, and
;; `Tiny/Slow.lean', which keeps the server busy for a few seconds.
;;
;; Run with: emacs -Q --batch -l build.el test-live

;;; Code:

(require 'ert)
(require 'nael-margin)
(require 'nael-infoview-live)
(require 'nael-rpc-live)
(require 'nael-test-helpers)

(defvar nael-margin-live-texts nil
  "The original contents of the test files, by file name.")

(defun nael-margin-live-open (file)
  "Return the buffer of FILE in the test project, managed and processed.
Its contents are as they were when it was first opened in the session."
  (nael-rpc-live-buffer)
  (let ((buffer (find-file-noselect (concat nael-rpc-live-root file))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'nael-mode)
        (nael-mode)
        (should (nael-rpc-live-wait #'eglot-managed-p 30)))
      (let ((text (or (cdr (assoc file nael-margin-live-texts))
                      (cdar (push (cons file (buffer-string))
                                  nael-margin-live-texts)))))
        (unless (equal (buffer-string) text)
          (erase-buffer)
          (insert text)
          (set-buffer-modified-p nil)
          (nael-eglot-flush-changes)
          (setq nael-eglot-file-progress nil)))
      (should (nael-rpc-live-wait #'nael-rpc-live-processed-p 60))
      (nael-margin-live-settle))
    buffer))

(defun nael-margin-live-settle ()
  "Wait until the signs are drawn after the latest report."
  (should (nael-rpc-live-wait (lambda () (null nael-margin--timer)) 2)))

(defun nael-margin-live-signs ()
  "Draw the signs as they would be with all characters displayable.
Return them as (LINE STRING), LINE 0-based."
  (nael-test-with-margin-defaults
    (nael-margin-draw)
    (nael-test-margin-drawn)))

(defconst nael-margin-live-expected
  '((0 "✓")                             ; done
    (3 "┌") (4 "│") (5 "✗")             ; open_goal, unsolved
    (7 "⚠")                             ; admitted, sorry
    (11 "┌") (12 "│") (13 "└")          ; example, failed instance
    (15 "‼"))                           ; #check
  "The signs of `Tiny/Signs.lean'.")

(ert-deftest nael-margin-live-signs ()
  "The server's diagnostics give the expected signs."
  (with-current-buffer (nael-margin-live-open "Tiny/Signs.lean")
    (should nael-margin-mode)
    ;; One column, or two where `‼' falls back to `!!', as in batch mode.
    (should (eql left-margin-width (nael-margin--needed-width)))
    (should (equal (nael-margin-live-signs) nael-margin-live-expected))))

(ert-deftest nael-margin-live-flymake ()
  "Flymake gets the diagnostics without the silent one, and no indicators."
  (with-current-buffer (nael-margin-live-open "Tiny/Signs.lean")
    (should (seq-some (lambda (diagnostic)
                        (nael-eglot-diagnostic-tag-p diagnostic
                                                     'goals-accomplished))
                      nael-eglot-diagnostics))
    (should (nael-rpc-live-wait
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
          (should (nael-test-empty-display-p
                   (get-text-property 0 'display string))))))))

(ert-deftest nael-margin-live-edit ()
  "After an edit, the signs follow the server's new diagnostics."
  (with-current-buffer (nael-margin-live-open "Tiny/Signs.lean")
    (goto-char (point-min))
    (insert "-- A comment\n")
    (nael-eglot-flush-changes)
    (should (nael-rpc-live-wait
             (lambda ()
               (and (nael-rpc-live-processed-p)
                    (equal (car (nael-eglot-diagnostic-lines
                                 (seq-find
                                  (lambda (diagnostic)
                                    (nael-eglot-diagnostic-tag-p
                                     diagnostic 'goals-accomplished))
                                  nael-eglot-diagnostics)))
                           1)))
             30))
    (nael-margin-live-settle)
    (should (equal (nael-margin-live-signs)
                   (mapcar (pcase-lambda (`(,line ,string))
                             (list (1+ line) string))
                           nael-margin-live-expected)))
    ;; Completing the open proof turns its bar into a complete proof.
    (goto-char (point-min))
    (search-forward "· omega")
    (insert "\n  · trivial")
    (nael-eglot-flush-changes)
    (should (nael-rpc-live-wait
             (lambda ()
               (and (nael-rpc-live-processed-p)
                    (= (seq-count (lambda (diagnostic)
                                    (nael-eglot-diagnostic-tag-p
                                     diagnostic 'goals-accomplished))
                                  nael-eglot-diagnostics)
                       2)))
             30))
    (nael-margin-live-settle)
    (should (equal (seq-take (nael-margin-live-signs) 3)
                   '((1 "✓") (4 "✓") (9 "⚠"))))))

(ert-deftest nael-margin-live-mode-change ()
  "Setting up the major mode again leaves one set of signs."
  (with-current-buffer (nael-margin-live-open "Tiny/Signs.lean")
    (nael-mode)
    (should (nael-rpc-live-wait #'eglot-managed-p 30))
    (setq nael-eglot-file-progress nil)
    (should (nael-rpc-live-wait
             (lambda () (and (nael-rpc-live-processed-p)
                             (seq-some (lambda (diagnostic)
                                         (nael-eglot-diagnostic-tag-p
                                          diagnostic 'goals-accomplished))
                                       nael-eglot-diagnostics)))
             60))
    (nael-margin-live-settle)
    (should nael-margin-mode)
    (should (equal (nael-margin-live-signs) nael-margin-live-expected))
    (should (= (seq-count (lambda (overlay) (overlay-get overlay 'nael-margin))
                          (overlays-in (point-min) (point-max)))
               (length nael-margin-live-expected)))))

(ert-deftest nael-margin-live-processing ()
  "Lines being processed get a bar until the server is done with them."
  (with-current-buffer (nael-margin-live-open "Tiny/Slow.lean")
    ;; Make the server process the file again.
    (goto-char (point-min))
    (insert "-- Again\n")
    (nael-eglot-flush-changes)
    (setq nael-eglot-file-progress nil)
    (should (nael-rpc-live-wait
             (lambda ()
               (and (not nael-margin--timer)
                    (assoc 5 (nael-margin-live-signs))
                    (equal (cadr (assoc 5 (nael-margin-live-signs))) "│")))
             10))
    (should (nael-rpc-live-wait #'nael-rpc-live-processed-p 30))
    (nael-margin-live-settle)
    (should (equal (nael-margin-live-signs) '((1 "✓") (5 "✓"))))))

(provide 'nael-margin-live)

;;; nael-margin-live.el ends here
