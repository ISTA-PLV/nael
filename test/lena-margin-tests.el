;;; lena-margin-tests.el --- Tests for lena-margin  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see lena/LICENSE.GPL3.

;;; Commentary:

;; Tests for `lena-margin' that need no Lean server, on diagnostics
;; made by hand; see `lena-test-diagnostic'.

;;; Code:

(require 'ert)
(require 'lena-margin)
(require 'lena-test-helpers)

(defun lena-margin-test-wait (predicate &optional seconds)
  "Run timers until PREDICATE holds or SECONDS (default 2) pass."
  (let ((deadline (+ (float-time) (or seconds 2))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (funcall predicate)))

(defun lena-margin-test-signs (diagnostics &optional processing line-count)
  "Return the signs for DIAGNOSTICS and PROCESSING as (LINE STRING FACE).
The buffer has LINE-COUNT lines, 20 by default."
  (mapcar (pcase-lambda (`(,line ,_rank ,string ,face ,_help))
            (list line string face))
          (lena-margin-signs (vconcat diagnostics) (vconcat processing)
                             (or line-count 20))))

;;;; Signs:

(ert-deftest lena-margin-test-single-line ()
  "Diagnostics on one line get the sign of their severity."
  (lena-test-with-margin-defaults
    (should (equal (lena-margin-test-signs
                    (list (lena-test-diagnostic 1 '(1 . 2) '(1 . 5))
                          (lena-test-diagnostic 2 '(3 . 2) '(3 . 5))
                          (lena-test-diagnostic 3 '(5 . 2) '(5 . 5))
                          (lena-test-diagnostic 4 '(7 . 2) '(7 . 5))))
                   '((1 "‼" lena-margin-error)
                     (3 "!" lena-margin-warning)
                     (5 "!" lena-margin-info)
                     (7 "!" lena-margin-info))))))

(ert-deftest lena-margin-test-bar ()
  "A diagnostic over several lines gets a bar over its full range."
  (lena-test-with-margin-defaults
    (should (equal (lena-margin-test-signs
                    (list (lena-test-diagnostic 1 '(2 . 4) '(5 . 3))))
                   '((2 "┌" lena-margin-error)
                     (3 "│" lena-margin-error)
                     (4 "│" lena-margin-error)
                     (5 "└" lena-margin-error))))
    ;; Ending at the start of a line, it ends on the line before.
    (should (equal (lena-margin-test-signs
                    (list (lena-test-diagnostic 2 '(2 . 4) '(4 . 0))))
                   '((2 "┌" lena-margin-warning)
                     (3 "└" lena-margin-warning))))))

(ert-deftest lena-margin-test-bar-criterion ()
  "Bars only where the server shortened the range, as in lean.nvim."
  (lena-test-with-margin-defaults
    ;; Over two lines, the shortened range ends on the same line as the
    ;; full range, so there is no bar.
    (should (equal (lena-margin-test-signs
                    (list (lena-test-diagnostic 1 '(2 . 4) '(3 . 3))))
                   '((2 "‼" lena-margin-error))))
    ;; A diagnostic about the whole buffer gets a sign on its first line.
    (should (equal (lena-margin-test-signs
                    (list (lena-test-diagnostic 1 '(0 . 0) '(19 . 0)))
                    nil 20)
                   '((0 "‼" lena-margin-error))))
    ;; No full range: a sign.
    (should (equal (lena-margin-test-signs
                    (list (list :range (lena-test-range '(2 . 0)
                                                         '(3 . 0))
                                :severity 1 :message "Message")))
                   '((2 "‼" lena-margin-error))))))

(ert-deftest lena-margin-test-precedence ()
  "Each line shows one sign: errors, warnings, information, processing."
  (lena-test-with-margin-defaults
    (should (equal (lena-margin-test-signs
                    (list (lena-test-diagnostic 2 '(2 . 0) '(2 . 3))
                          (lena-test-diagnostic 1 '(1 . 0) '(4 . 3))
                          (lena-test-diagnostic 3 '(4 . 0) '(4 . 3))
                          (lena-test-diagnostic 3 '(6 . 0) '(6 . 3))
                          (lena-test-diagnostic 3 '(8 . 0) '(9 . 0)
                                                 :tags '(2) :silent t))
                    (list (list :range (lena-test-range '(5 . 0)
                                                         '(8 . 0))
                                :kind 1)))
                   '((1 "┌" lena-margin-error)
                     (2 "│" lena-margin-error)
                     (3 "│" lena-margin-error)
                     (4 "└" lena-margin-error)
                     (5 "│" lena-margin-processing)
                     (6 "!" lena-margin-info)
                     (7 "│" lena-margin-processing)
                     (8 "│" lena-margin-processing))))
    ;; Among signs of the same severity, a first or single line wins
    ;; over the lines of a bar.
    (should (equal (lena-margin-test-signs
                    (list (lena-test-diagnostic 1 '(1 . 0) '(4 . 3))
                          (lena-test-diagnostic 1 '(2 . 0) '(2 . 3))
                          (lena-test-diagnostic 1 '(4 . 0) '(6 . 3))))
                   '((1 "┌" lena-margin-error)
                     (2 "‼" lena-margin-error)
                     (3 "│" lena-margin-error)
                     (4 "┌" lena-margin-error)
                     (5 "│" lena-margin-error)
                     (6 "└" lena-margin-error))))))

(ert-deftest lena-margin-test-proofs ()
  "Signs for complete, unsolved and admitted proofs."
  (lena-test-with-margin-defaults
    (should (equal (lena-margin-test-signs
                    (list (lena-test-diagnostic 3 '(0 . 0) '(1 . 6)
                                                 :tags '(2) :silent t)
                          (lena-test-diagnostic 1 '(3 . 56) '(5 . 9)
                                                 :tags '(1))
                          (lena-test-diagnostic
                           2 '(7 . 8) '(7 . 16)
                           :message "declaration uses `sorry`")
                          (lena-test-diagnostic 1 '(9 . 2) '(9 . 8)
                                                 :tags '(1))
                          (lena-test-diagnostic
                           2 '(11 . 8) '(11 . 16)
                           :message "declaration uses 'sorry'")))
                   '((0 "✓" lena-margin-goals-accomplished)
                     (3 "┌" lena-margin-error)
                     (4 "│" lena-margin-error)
                     (5 "✗" lena-margin-unsolved-goals)
                     (7 "⚠" lena-margin-admitted)
                     (9 "✗" lena-margin-unsolved-goals)
                     (11 "⚠" lena-margin-admitted))))
    ;; Other silent diagnostics and other warnings get no such sign.
    (should (equal (lena-margin-test-signs
                    (list (lena-test-diagnostic 3 '(0 . 0) '(0 . 6)
                                                 :silent t)
                          (lena-test-diagnostic
                           2 '(2 . 0) '(2 . 6)
                           :message "unused variable `sorry`")))
                   '((2 "!" lena-margin-warning))))))

(ert-deftest lena-margin-test-processing ()
  "Lines being processed get a bar; regions with fatal errors do not."
  (lena-test-with-margin-defaults
    (should (equal (lena-margin-test-signs
                    nil
                    (list (list :range (lena-test-range '(2 . 0)
                                                         '(3 . 0))
                                :kind 1)
                          (list :range (lena-test-range '(5 . 0)
                                                         '(6 . 0))
                                :kind 2)
                          (list :range (lena-test-range '(8 . 0)
                                                         '(30 . 0))))
                    10)
                   '((2 "│" lena-margin-processing)
                     (3 "│" lena-margin-processing)
                     (8 "│" lena-margin-processing)
                     (9 "│" lena-margin-processing))))))

(ert-deftest lena-margin-test-options ()
  "The signs follow the options, also as buffer-local values."
  (lena-test-with-margin-defaults
    (with-temp-buffer
      (setq-local lena-margin-goals-accomplished "🐣"
                  lena-margin-unsolved-goals "🥚"
                  lena-margin-admitted "🍗"
                  lena-margin-bar '("╭" "┆" "╰"))
      (let ((diagnostics
             (list (lena-test-diagnostic 3 '(0 . 0) '(1 . 6)
                                          :tags '(2) :silent t)
                   (lena-test-diagnostic 1 '(3 . 0) '(5 . 9)
                                          :tags '(1))
                   (lena-test-diagnostic
                    2 '(7 . 8) '(7 . 16)
                    :message "declaration uses `sorry`"))))
        (should (equal (lena-margin-test-signs diagnostics)
                       '((0 "🐣" lena-margin-goals-accomplished)
                         (3 "╭" lena-margin-error)
                         (4 "┆" lena-margin-error)
                         (5 "🥚" lena-margin-unsolved-goals)
                         (7 "🍗" lena-margin-admitted))))
        ;; Empty strings: no sign for complete proofs, and the signs of
        ;; the diagnostics for the others.
        (setq-local lena-margin-goals-accomplished ""
                    lena-margin-unsolved-goals ""
                    lena-margin-admitted "")
        (should (equal (lena-margin-test-signs diagnostics)
                       '((3 "╭" lena-margin-error)
                         (4 "┆" lena-margin-error)
                         (5 "╰" lena-margin-error)
                         (7 "!" lena-margin-warning))))))
    ;; Flymake's strings for the severities.
    (let ((flymake-margin-indicators-string '((error "E" compilation-error)
                                              (warning "W")
                                              (note "I"))))
      (should (equal (lena-margin-test-signs
                      (list (lena-test-diagnostic 1 '(1 . 0) '(1 . 3))
                            (lena-test-diagnostic 2 '(2 . 0) '(2 . 3))
                            (lena-test-diagnostic 3 '(3 . 0) '(3 . 3))))
                     '((1 "E" lena-margin-error)
                       (2 "W" lena-margin-warning)
                       (3 "I" lena-margin-info)))))))

(ert-deftest lena-margin-test-emoji-height ()
  "Emoji in signs, and only they, are drawn at `lena-margin-emoji-height'."
  (cl-flet ((height (string &optional (pos 0))
              (let ((face (get-text-property pos 'face string)))
                (if (keywordp (car-safe face))
                    (plist-get face :height)
                  (seq-some (lambda (face) (plist-get face :height)) face))))
            (sign (string)
              (cadr (get-text-property
                     0 'display
                     (lena-margin--string string 'lena-margin-admitted "Help")))))
    (let ((lena-margin-emoji-height 0.7))
      (dolist (chick '("🐣" "🥚" "🍗"))
        (should (equal (height (sign chick)) 0.7))
        ;; The rest of the sign stays as it was.
        (should (eq (get-text-property 0 'keymap (sign chick))
                    lena-margin-sign-map)))
      (should-not (height (sign "✓")))
      (let ((title (lena-margin-scale-emoji "Goals accomplished 🐣")))
        (should-not (height title))
        (should (equal (height title (1- (length title))) 0.7))))
    (let ((lena-margin-emoji-height 1.0))
      (should-not (height (sign "🐣"))))))

(ert-deftest lena-margin-test-fallbacks ()
  "Defaults that cannot be displayed fall back; users' values stay."
  (cl-letf (((symbol-function 'char-displayable-p)
             (lambda (char) (< char #x2000))))
    (with-temp-buffer
      (should (equal (lena-margin-sign 'lena-margin-goals-accomplished) "*"))
      (should (equal (lena-margin-sign 'lena-margin-unsolved-goals) "x"))
      (should (equal (lena-margin-sign 'lena-margin-admitted) ""))
      (should (equal (lena-margin--severity-sign 'error) "!!"))
      (setq-local lena-margin-goals-accomplished "🐣")
      (should (equal (lena-margin-sign 'lena-margin-goals-accomplished)
                     "🐣")))))

;;;; Drawing and the margin:

(defmacro lena-margin-test-with-buffer (diagnostics &rest body)
  "Run BODY in a buffer of 20 lines with DIAGNOSTICS, shown in a window."
  (declare (indent 1) (debug t))
  `(lena-test-with-margin-defaults
     (with-temp-buffer
       (dotimes (i 20) (insert (format "line %d\n" i)))
       (setq lena-eglot-diagnostics (vconcat ,diagnostics)
             lena-eglot-file-progress [])
       (set-window-buffer nil (current-buffer))
       ,@body)))

(ert-deftest lena-margin-test-draw ()
  "Signs are drawn at the start of their lines and cut to two columns."
  (lena-margin-test-with-buffer
      (list (lena-test-diagnostic 1 '(2 . 4) '(4 . 3))
            (lena-test-diagnostic 2 '(7 . 8) '(7 . 16)
                                   :message "declaration uses `sorry`"))
    (setq-local lena-margin-admitted "abc")
    (lena-margin-draw)
    (should (equal (lena-test-margin-drawn)
                   '((2 "┌") (3 "│") (4 "└") (7 "ab"))))
    (should (seq-every-p (lambda (overlay)
                           (= (overlay-start overlay)
                              (save-excursion
                                (goto-char (overlay-start overlay))
                                (line-beginning-position))))
                         lena-margin--overlays))
    ;; Drawing again replaces the signs.
    (setq lena-eglot-diagnostics [])
    (lena-margin-draw)
    (should-not (lena-test-margin-drawn))
    (should-not (seq-some (lambda (overlay) (overlay-get overlay 'lena-margin))
                          (overlays-in (point-min) (point-max))))))

(ert-deftest lena-margin-test-help ()
  "A sign tells the message of its diagnostic and lists all on click."
  (lena-margin-test-with-buffer
      (list (lena-test-diagnostic 1 '(2 . 4) '(2 . 9)
                                   :message "unknown identifier"))
    (lena-margin-draw)
    (let ((string (cadr (get-text-property
                         0 'display
                         (overlay-get (car lena-margin--overlays)
                                      'before-string)))))
      (should (equal (get-text-property 0 'help-echo string)
                     "unknown identifier"))
      (should (eq (lookup-key (get-text-property 0 'keymap string)
                              [left-margin mouse-1])
                  #'flymake-show-buffer-diagnostics)))))

(ert-deftest lena-margin-test-width ()
  "The mode widens the left margin to its widest sign and gives back as much."
  (lena-margin-test-with-buffer nil
    (should (eql left-margin-width 0))
    (lena-margin-mode)
    ;; The default signs take one column.
    (should (eql left-margin-width 1))
    (should (eql (car (window-margins)) 1))
    (lena-margin-mode -1)
    (should (eql left-margin-width 0))
    (should-not (car (window-margins)))
    (setq-local lena-margin-admitted "!?")
    ;; Wider margins stay as they are.
    (setq left-margin-width 4)
    (lena-margin-mode)
    (should (eql left-margin-width 4))
    (lena-margin-mode -1)
    (should (eql left-margin-width 4))
    ;; A narrower one is widened, and narrowed back to its width.
    (setq left-margin-width 1)
    (lena-margin-mode)
    (should (eql left-margin-width 2))
    ;; Someone else narrows it in between: drawing widens it again.
    (setq left-margin-width 1)
    (lena-margin-draw)
    (should (eql left-margin-width 2))
    (lena-margin-mode -1)
    (should (eql left-margin-width 1))
    (should (eql (car (window-margins)) 1))
    ;; Someone else changes it while the mode is on: it stays.
    (lena-margin-mode)
    (setq left-margin-width 3)
    (lena-margin-mode -1)
    (should (eql left-margin-width 3))
    (setq left-margin-width 0)
    (set-window-margins nil nil)))

(ert-deftest lena-margin-test-wide-signs ()
  "Signs wider in pixels than their columns widen the margin further."
  (lena-margin-test-with-buffer nil
    (cl-letf (((symbol-function 'display-graphic-p) #'always)
              ((symbol-function 'frame-char-width) (lambda (&rest _) 10))
              ((symbol-function 'string-pixel-width)
               (lambda (string &rest _)
                 ;; An emoji font whose glyphs are 2.3 columns wide.
                 (if (equal string "🍗") 23 (* 10 (string-width string))))))
      (lena-margin-mode)
      (should (eql left-margin-width 1))
      ;; All signs count, also one that no line shows.
      (setq-local lena-margin-admitted "🍗")
      (lena-margin-draw)
      (should (eql left-margin-width 3))
      (should (eql (car (window-margins)) 3))
      ;; The width before the mode is restored, not the first width.
      (lena-margin-mode -1)
      (should (eql left-margin-width 0))
      (set-window-margins nil nil))))

(ert-deftest lena-margin-test-indicators ()
  "The mode hides the indicators of Flymake and Flycheck while it is on."
  (lena-margin-test-with-buffer nil
    (lena-margin-mode)
    (with-suppressed-warnings ((obsolete flymake-diagnostic-types-alist))
      (dolist (type '(eglot-error eglot-warning eglot-note))
        (should (equal (alist-get 'flymake-margin-string
                                  (alist-get type
                                             flymake-diagnostic-types-alist))
                       '("" default)))
        (should (eq (alist-get 'flymake-bitmap
                               (alist-get type
                                          flymake-diagnostic-types-alist))
                    'lena-margin--empty-bitmap))))
    (should (local-variable-p 'flymake-autoresize-margins))
    (should-not flymake-autoresize-margins)
    (should (local-variable-p 'flycheck-indication-mode))
    (should-not flycheck-indication-mode)
    (lena-margin-mode -1)
    (should-not (local-variable-p 'flymake-diagnostic-types-alist))
    (should-not (local-variable-p 'flymake-autoresize-margins))
    (should-not (local-variable-p 'flycheck-indication-mode))))

(ert-deftest lena-margin-test-indicators-restored ()
  "The mode gives back the buffer's own settings, even when enabled twice."
  (lena-margin-test-with-buffer nil
    (with-suppressed-warnings ((obsolete flymake-diagnostic-types-alist))
      (setq-local flymake-diagnostic-types-alist '((mine)))
      (setq-local flymake-autoresize-margins 'mine)
      (lena-margin-mode)
      (lena-margin-mode)
      (should (equal (car (last flymake-diagnostic-types-alist)) '(mine)))
      (should-not flymake-autoresize-margins)
      (lena-margin-mode -1)
      (should (equal flymake-diagnostic-types-alist '((mine))))
      (should (eq flymake-autoresize-margins 'mine))
      (should-not (local-variable-p 'flycheck-indication-mode)))))

(defun lena-margin-test-flymake-backend (report &rest _)
  "Report an error of Eglot's type through REPORT."
  (funcall report (list (flymake-make-diagnostic (current-buffer) 1 5
                                                 'eglot-error "Boom"))))

(ert-deftest lena-margin-test-flymake ()
  "Flymake underlines, but shows no indicator, and none in the text."
  (lena-margin-test-with-buffer nil
    (put 'eglot-error 'flymake-category 'flymake-error)
    (lena-margin-mode)
    (setq-local flymake-diagnostic-functions
                (list #'lena-margin-test-flymake-backend))
    (unwind-protect
        (progn
          (flymake-mode)
          (flymake-start nil t)
          (should (lena-margin-test-wait #'flymake-diagnostics 5))
          (let* ((overlay (seq-find (lambda (overlay)
                                      (overlay-get overlay 'flymake-overlay))
                                    (overlays-in (point-min) (point-max))))
                 (string (overlay-get overlay 'before-string))
                 (display (and string (get-text-property 0 'display string))))
            (should (overlay-get overlay 'face))
            ;; Flymake's placeholder text, if any, is always replaced
            ;; by a display that shows nothing.
            (when string
              (should (lena-test-empty-display-p display)))))
      (flymake-mode -1))))

(ert-deftest lena-margin-test-schedule ()
  "Reports in a burst draw the signs once, after a delay."
  (lena-margin-test-with-buffer
      (list (lena-test-diagnostic 1 '(2 . 4) '(2 . 9)))
    (let ((draws 0)
          (lena-margin-delay 0.05))
      (advice-add 'lena-margin-draw :before
                  (lambda () (cl-incf draws))
                  '((name . lena-margin-test)))
      (unwind-protect
          (progn
            (lena-margin-mode)
            (setq draws 0)
            (run-hook-with-args 'lena-eglot-diagnostics-functions [])
            (run-hook-with-args 'lena-eglot-file-progress-functions [])
            (run-hook-with-args 'lena-eglot-diagnostics-functions [])
            (should (zerop draws))
            (should (lena-margin-test-wait (lambda () (= draws 1))))
            (lena-margin-test-wait #'ignore 0.15)
            (should (= draws 1))
            (should (equal (lena-test-margin-drawn) '((2 "‼")))))
        (advice-remove 'lena-margin-draw 'lena-margin-test)
        (lena-margin-mode -1)))))

(ert-deftest lena-margin-test-update ()
  "The mode is on in managed buffers when option `lena-margin' is non-nil."
  (lena-margin-test-with-buffer nil
    (let ((managed t)
          (lena-margin t))
      (cl-letf (((symbol-function 'eglot-managed-p) (lambda () managed)))
        (lena-margin-update)
        (should lena-margin-mode)
        (setq managed nil)
        (lena-margin-update)
        (should-not lena-margin-mode)
        (should (eql left-margin-width 0))
        (setq managed t lena-margin nil)
        (lena-margin-update)
        (should-not lena-margin-mode)))))

(provide 'lena-margin-tests)

;;; lena-margin-tests.el ends here
