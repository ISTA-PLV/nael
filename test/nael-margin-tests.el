;;; nael-margin-tests.el --- Tests for nael-margin  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see nael/LICENSE.GPL3.

;;; Commentary:

;; Tests for `nael-margin' that need no Lean server, on diagnostics
;; made by hand; see `nael-eglot-test-diagnostic'.

;;; Code:

(require 'ert)
(require 'nael-margin)
(require 'nael-eglot-tests)

(defmacro nael-margin-test-with-defaults (&rest body)
  "Run BODY with the default signs, all of which can be displayed."
  (declare (indent 0) (debug t))
  `(let ((nael-margin-goals-accomplished "🎉")
         (nael-margin-unsolved-goals "🔨")
         (nael-margin-admitted "🚧")
         (nael-margin-bar '("┌" "│" "└"))
         (nael-margin-processing "│")
         (flymake-margin-indicators-string
          '((error "‼" compilation-error)
            (warning "!" compilation-warning)
            (note "!" compilation-info))))
     (cl-letf (((symbol-function 'char-displayable-p) #'always))
       ,@body)))

(defun nael-margin-test-wait (predicate &optional seconds)
  "Run timers until PREDICATE holds or SECONDS (default 2) pass."
  (let ((deadline (+ (float-time) (or seconds 2))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.01))
    (funcall predicate)))

(defun nael-margin-test-signs (diagnostics &optional processing line-count)
  "Return the signs for DIAGNOSTICS and PROCESSING as (LINE STRING FACE).
The buffer has LINE-COUNT lines, 20 by default."
  (mapcar (pcase-lambda (`(,line ,_rank ,string ,face ,_help))
            (list line string face))
          (nael-margin-signs (vconcat diagnostics) (vconcat processing)
                             (or line-count 20))))


;;;; Signs:

(ert-deftest nael-margin-test-single-line ()
  "Diagnostics on one line get the sign of their severity."
  (nael-margin-test-with-defaults
    (should (equal (nael-margin-test-signs
                    (list (nael-eglot-test-diagnostic 1 '(1 . 2) '(1 . 5))
                          (nael-eglot-test-diagnostic 2 '(3 . 2) '(3 . 5))
                          (nael-eglot-test-diagnostic 3 '(5 . 2) '(5 . 5))
                          (nael-eglot-test-diagnostic 4 '(7 . 2) '(7 . 5))))
                   '((1 "‼" nael-margin-error)
                     (3 "!" nael-margin-warning)
                     (5 "!" nael-margin-info)
                     (7 "!" nael-margin-info))))))

(ert-deftest nael-margin-test-bar ()
  "A diagnostic over several lines gets a bar over its full range."
  (nael-margin-test-with-defaults
    (should (equal (nael-margin-test-signs
                    (list (nael-eglot-test-diagnostic 1 '(2 . 4) '(5 . 3))))
                   '((2 "┌" nael-margin-error)
                     (3 "│" nael-margin-error)
                     (4 "│" nael-margin-error)
                     (5 "└" nael-margin-error))))
    ;; Ending at the start of a line, it ends on the line before.
    (should (equal (nael-margin-test-signs
                    (list (nael-eglot-test-diagnostic 2 '(2 . 4) '(4 . 0))))
                   '((2 "┌" nael-margin-warning)
                     (3 "└" nael-margin-warning))))))

(ert-deftest nael-margin-test-bar-criterion ()
  "Bars only where the server shortened the range, as in lean.nvim."
  (nael-margin-test-with-defaults
    ;; Over two lines, the shortened range ends on the same line as the
    ;; full range, so there is no bar.
    (should (equal (nael-margin-test-signs
                    (list (nael-eglot-test-diagnostic 1 '(2 . 4) '(3 . 3))))
                   '((2 "‼" nael-margin-error))))
    ;; A diagnostic about the whole buffer gets a sign on its first line.
    (should (equal (nael-margin-test-signs
                    (list (nael-eglot-test-diagnostic 1 '(0 . 0) '(19 . 0)))
                    nil 20)
                   '((0 "‼" nael-margin-error))))
    ;; No full range: a sign.
    (should (equal (nael-margin-test-signs
                    (list (list :range (nael-eglot-test-range '(2 . 0)
                                                               '(3 . 0))
                                :severity 1 :message "Message")))
                   '((2 "‼" nael-margin-error))))))

(ert-deftest nael-margin-test-precedence ()
  "Each line shows one sign: errors, warnings, information, processing."
  (nael-margin-test-with-defaults
    (should (equal (nael-margin-test-signs
                    (list (nael-eglot-test-diagnostic 2 '(2 . 0) '(2 . 3))
                          (nael-eglot-test-diagnostic 1 '(1 . 0) '(4 . 3))
                          (nael-eglot-test-diagnostic 3 '(4 . 0) '(4 . 3))
                          (nael-eglot-test-diagnostic 3 '(6 . 0) '(6 . 3))
                          (nael-eglot-test-diagnostic 3 '(8 . 0) '(9 . 0)
                                                       :tags '(2) :silent t))
                    (list (list :range (nael-eglot-test-range '(5 . 0)
                                                               '(8 . 0))
                                :kind 1)))
                   '((1 "┌" nael-margin-error)
                     (2 "│" nael-margin-error)
                     (3 "│" nael-margin-error)
                     (4 "└" nael-margin-error)
                     (5 "│" nael-margin-processing)
                     (6 "!" nael-margin-info)
                     (7 "│" nael-margin-processing)
                     (8 "│" nael-margin-processing))))
    ;; Among signs of the same severity, a first or single line wins
    ;; over the lines of a bar.
    (should (equal (nael-margin-test-signs
                    (list (nael-eglot-test-diagnostic 1 '(1 . 0) '(4 . 3))
                          (nael-eglot-test-diagnostic 1 '(2 . 0) '(2 . 3))
                          (nael-eglot-test-diagnostic 1 '(4 . 0) '(6 . 3))))
                   '((1 "┌" nael-margin-error)
                     (2 "‼" nael-margin-error)
                     (3 "│" nael-margin-error)
                     (4 "┌" nael-margin-error)
                     (5 "│" nael-margin-error)
                     (6 "└" nael-margin-error))))))

(ert-deftest nael-margin-test-proofs ()
  "Signs for complete, unsolved and admitted proofs."
  (nael-margin-test-with-defaults
    (should (equal (nael-margin-test-signs
                    (list (nael-eglot-test-diagnostic 3 '(0 . 0) '(1 . 6)
                                                       :tags '(2) :silent t)
                          (nael-eglot-test-diagnostic 1 '(3 . 56) '(5 . 9)
                                                       :tags '(1))
                          (nael-eglot-test-diagnostic
                           2 '(7 . 8) '(7 . 16)
                           :message "declaration uses `sorry`")
                          (nael-eglot-test-diagnostic 1 '(9 . 2) '(9 . 8)
                                                       :tags '(1))
                          (nael-eglot-test-diagnostic
                           2 '(11 . 8) '(11 . 16)
                           :message "declaration uses 'sorry'")))
                   '((0 "🎉" nael-margin-goals-accomplished)
                     (3 "┌" nael-margin-error)
                     (4 "│" nael-margin-error)
                     (5 "🔨" nael-margin-unsolved-goals)
                     (7 "🚧" nael-margin-admitted)
                     (9 "🔨" nael-margin-unsolved-goals)
                     (11 "🚧" nael-margin-admitted))))
    ;; Other silent diagnostics and other warnings get no such sign.
    (should (equal (nael-margin-test-signs
                    (list (nael-eglot-test-diagnostic 3 '(0 . 0) '(0 . 6)
                                                       :silent t)
                          (nael-eglot-test-diagnostic
                           2 '(2 . 0) '(2 . 6)
                           :message "unused variable `sorry`")))
                   '((2 "!" nael-margin-warning))))))

(ert-deftest nael-margin-test-processing ()
  "Lines being processed get a bar; regions with fatal errors do not."
  (nael-margin-test-with-defaults
    (should (equal (nael-margin-test-signs
                    nil
                    (list (list :range (nael-eglot-test-range '(2 . 0)
                                                               '(3 . 0))
                                :kind 1)
                          (list :range (nael-eglot-test-range '(5 . 0)
                                                               '(6 . 0))
                                :kind 2)
                          (list :range (nael-eglot-test-range '(8 . 0)
                                                               '(30 . 0))))
                    10)
                   '((2 "│" nael-margin-processing)
                     (3 "│" nael-margin-processing)
                     (8 "│" nael-margin-processing)
                     (9 "│" nael-margin-processing))))))

(ert-deftest nael-margin-test-options ()
  "The signs follow the options, also as buffer-local values."
  (nael-margin-test-with-defaults
    (with-temp-buffer
      (setq-local nael-margin-goals-accomplished "🐣"
                  nael-margin-unsolved-goals "🥚"
                  nael-margin-admitted "🍗"
                  nael-margin-bar '("╭" "┆" "╰"))
      (let ((diagnostics
             (list (nael-eglot-test-diagnostic 3 '(0 . 0) '(1 . 6)
                                                :tags '(2) :silent t)
                   (nael-eglot-test-diagnostic 1 '(3 . 0) '(5 . 9)
                                                :tags '(1))
                   (nael-eglot-test-diagnostic
                    2 '(7 . 8) '(7 . 16)
                    :message "declaration uses `sorry`"))))
        (should (equal (nael-margin-test-signs diagnostics)
                       '((0 "🐣" nael-margin-goals-accomplished)
                         (3 "╭" nael-margin-error)
                         (4 "┆" nael-margin-error)
                         (5 "🥚" nael-margin-unsolved-goals)
                         (7 "🍗" nael-margin-admitted))))
        ;; Empty strings: no sign for complete proofs, and the signs of
        ;; the diagnostics for the others.
        (setq-local nael-margin-goals-accomplished ""
                    nael-margin-unsolved-goals ""
                    nael-margin-admitted "")
        (should (equal (nael-margin-test-signs diagnostics)
                       '((3 "╭" nael-margin-error)
                         (4 "┆" nael-margin-error)
                         (5 "╰" nael-margin-error)
                         (7 "!" nael-margin-warning))))))
    ;; Flymake's strings for the severities.
    (let ((flymake-margin-indicators-string '((error "E" compilation-error)
                                              (warning "W")
                                              (note "I"))))
      (should (equal (nael-margin-test-signs
                      (list (nael-eglot-test-diagnostic 1 '(1 . 0) '(1 . 3))
                            (nael-eglot-test-diagnostic 2 '(2 . 0) '(2 . 3))
                            (nael-eglot-test-diagnostic 3 '(3 . 0) '(3 . 3))))
                     '((1 "E" nael-margin-error)
                       (2 "W" nael-margin-warning)
                       (3 "I" nael-margin-info)))))))

(ert-deftest nael-margin-test-fallbacks ()
  "Defaults that cannot be displayed fall back; users' values stay."
  (cl-letf (((symbol-function 'char-displayable-p)
             (lambda (char) (< char #x2000))))
    (with-temp-buffer
      (should (equal (nael-margin-sign 'nael-margin-goals-accomplished) "*"))
      (should (equal (nael-margin-sign 'nael-margin-unsolved-goals) "x"))
      (should (equal (nael-margin-sign 'nael-margin-admitted) ""))
      (should (equal (nael-margin--severity-sign 'error) "!!"))
      (setq-local nael-margin-goals-accomplished "🐣")
      (should (equal (nael-margin-sign 'nael-margin-goals-accomplished)
                     "🐣"))))
  (cl-letf (((symbol-function 'char-displayable-p)
             (lambda (char) (not (memq char '(?🎉 ?🔨))))))
    (with-temp-buffer
      (should (equal (nael-margin-sign 'nael-margin-goals-accomplished)
                     "✓"))
      (should (equal (nael-margin-sign 'nael-margin-unsolved-goals) "✗")))))

;;;; Drawing and the margin:

(defmacro nael-margin-test-with-buffer (diagnostics &rest body)
  "Run BODY in a buffer of 20 lines with DIAGNOSTICS, shown in a window."
  (declare (indent 1) (debug t))
  `(nael-margin-test-with-defaults
     (with-temp-buffer
       (dotimes (i 20) (insert (format "line %d\n" i)))
       (setq nael-eglot-diagnostics (vconcat ,diagnostics)
             nael-eglot-file-progress [])
       (set-window-buffer nil (current-buffer))
       ,@body)))

(defun nael-margin-test-drawn ()
  "Return the drawn signs as (LINE STRING) in order, LINE 0-based."
  (sort (mapcar (lambda (overlay)
                  (let ((spec (get-text-property
                               0 'display (overlay-get overlay 'before-string))))
                    (should (equal (car spec) '(margin left-margin)))
                    (list (1- (line-number-at-pos (overlay-start overlay)))
                          (substring-no-properties (cadr spec)))))
                nael-margin--overlays)
        #'car-less-than-car))

(ert-deftest nael-margin-test-draw ()
  "Signs are drawn at the start of their lines and cut to two columns."
  (nael-margin-test-with-buffer
      (list (nael-eglot-test-diagnostic 1 '(2 . 4) '(4 . 3))
            (nael-eglot-test-diagnostic 2 '(7 . 8) '(7 . 16)
                                         :message "declaration uses `sorry`"))
    (setq-local nael-margin-admitted "abc")
    (nael-margin-draw)
    (should (equal (nael-margin-test-drawn)
                   '((2 "┌") (3 "│") (4 "└") (7 "ab"))))
    (should (seq-every-p (lambda (overlay)
                           (= (overlay-start overlay)
                              (save-excursion
                                (goto-char (overlay-start overlay))
                                (line-beginning-position))))
                         nael-margin--overlays))
    ;; Drawing again replaces the signs.
    (setq nael-eglot-diagnostics [])
    (nael-margin-draw)
    (should-not (nael-margin-test-drawn))
    (should-not (seq-some (lambda (overlay) (overlay-get overlay 'nael-margin))
                          (overlays-in (point-min) (point-max))))))

(ert-deftest nael-margin-test-help ()
  "A sign tells the message of its diagnostic and lists all on click."
  (nael-margin-test-with-buffer
      (list (nael-eglot-test-diagnostic 1 '(2 . 4) '(2 . 9)
                                         :message "unknown identifier"))
    (nael-margin-draw)
    (let ((string (cadr (get-text-property
                         0 'display
                         (overlay-get (car nael-margin--overlays)
                                      'before-string)))))
      (should (equal (get-text-property 0 'help-echo string)
                     "unknown identifier"))
      (should (eq (lookup-key (get-text-property 0 'keymap string)
                              [left-margin mouse-1])
                  #'flymake-show-buffer-diagnostics)))))

(ert-deftest nael-margin-test-width ()
  "The mode widens the left margin to two columns and gives back as much."
  (nael-margin-test-with-buffer nil
    (should (eql left-margin-width 0))
    (nael-margin-mode)
    (should (eql left-margin-width 2))
    (should (eql (car (window-margins)) 2))
    (nael-margin-mode -1)
    (should (eql left-margin-width 0))
    (should-not (car (window-margins)))
    ;; Wider margins stay as they are.
    (setq left-margin-width 4)
    (nael-margin-mode)
    (should (eql left-margin-width 4))
    (nael-margin-mode -1)
    (should (eql left-margin-width 4))
    ;; A narrower one is widened, and narrowed back to its width.
    (setq left-margin-width 1)
    (nael-margin-mode)
    (should (eql left-margin-width 2))
    ;; Someone else narrows it in between: drawing widens it again.
    (setq left-margin-width 1)
    (nael-margin-draw)
    (should (eql left-margin-width 2))
    (nael-margin-mode -1)
    (should (eql left-margin-width 1))
    (should (eql (car (window-margins)) 1))
    ;; Someone else changes it while the mode is on: it stays.
    (nael-margin-mode)
    (setq left-margin-width 3)
    (nael-margin-mode -1)
    (should (eql left-margin-width 3))
    (setq left-margin-width 0)
    (set-window-margins nil nil)))

(ert-deftest nael-margin-test-wide-signs ()
  "Signs wider in pixels than two columns widen the margin further."
  (nael-margin-test-with-buffer nil
    (let ((nael-margin--width-cache nil))
      (cl-letf (((symbol-function 'display-graphic-p) #'always)
                ((symbol-function 'frame-char-width) (lambda (&rest _) 10))
                ((symbol-function 'string-pixel-width)
                 (lambda (string &rest _)
                   ;; An emoji font whose glyphs are 2.3 columns wide.
                   (if (equal string "🍗") 23 (* 10 (string-width string))))))
        (nael-margin-mode)
        (should (eql left-margin-width 2))
        ;; All signs count, also one that no line shows.
        (setq-local nael-margin-admitted "🍗")
        (nael-margin-draw)
        (should (eql left-margin-width 3))
        (should (eql (car (window-margins)) 3))
        ;; The width before the mode is restored, not the first width.
        (nael-margin-mode -1)
        (should (eql left-margin-width 0))
        (set-window-margins nil nil)))))

(ert-deftest nael-margin-test-indicators ()
  "The mode hides the indicators of Flymake and Flycheck while it is on."
  (nael-margin-test-with-buffer nil
    (nael-margin-mode)
    (with-suppressed-warnings ((obsolete flymake-diagnostic-types-alist))
      (dolist (type '(eglot-error eglot-warning eglot-note))
        (should (equal (alist-get 'flymake-margin-string
                                  (alist-get type
                                             flymake-diagnostic-types-alist))
                       '("" default)))
        (should (eq (alist-get 'flymake-bitmap
                               (alist-get type
                                          flymake-diagnostic-types-alist))
                    'nael-margin--empty-bitmap))))
    (should (local-variable-p 'flymake-autoresize-margins))
    (should-not flymake-autoresize-margins)
    (should (local-variable-p 'flycheck-indication-mode))
    (should-not flycheck-indication-mode)
    (nael-margin-mode -1)
    (should-not (local-variable-p 'flymake-diagnostic-types-alist))
    (should-not (local-variable-p 'flymake-autoresize-margins))
    (should-not (local-variable-p 'flycheck-indication-mode))))

(defun nael-margin-test-empty-display-p (display)
  "Return non-nil if DISPLAY shows nothing, in the margin or the fringe.
Flymake 1.4 chooses between both with `when' specifications."
  (if (eq (car-safe (car-safe display)) 'when)
      (seq-every-p (lambda (spec)
                     (and (eq (car-safe spec) 'when)
                          (nael-margin-test-empty-display-p (cddr spec))))
                   display)
    (member display '(((margin left-margin) "")
                      (left-fringe nael-margin-empty)
                      (right-fringe nael-margin-empty)))))

(defun nael-margin-test-flymake-backend (report &rest _)
  "Report an error of Eglot's type through REPORT."
  (funcall report (list (flymake-make-diagnostic (current-buffer) 1 5
                                                 'eglot-error "Boom"))))

(ert-deftest nael-margin-test-flymake ()
  "Flymake underlines, but shows no indicator, and none in the text."
  (nael-margin-test-with-buffer nil
    (put 'eglot-error 'flymake-category 'flymake-error)
    (nael-margin-mode)
    (setq-local flymake-diagnostic-functions
                (list #'nael-margin-test-flymake-backend))
    (unwind-protect
        (progn
          (flymake-mode)
          (flymake-start nil t)
          (should (nael-margin-test-wait #'flymake-diagnostics 5))
          (let* ((overlay (seq-find (lambda (overlay)
                                      (overlay-get overlay 'flymake-overlay))
                                    (overlays-in (point-min) (point-max))))
                 (string (overlay-get overlay 'before-string))
                 (display (and string (get-text-property 0 'display string))))
            (should (overlay-get overlay 'face))
            ;; Flymake's placeholder text, if any, is always replaced
            ;; by a display that shows nothing.
            (when string
              (should (nael-margin-test-empty-display-p display)))))
      (flymake-mode -1))))

(ert-deftest nael-margin-test-schedule ()
  "Reports in a burst draw the signs once, after a delay."
  (nael-margin-test-with-buffer
      (list (nael-eglot-test-diagnostic 1 '(2 . 4) '(2 . 9)))
    (let ((draws 0)
          (nael-margin-delay 0.05))
      (advice-add 'nael-margin-draw :before
                  (lambda () (cl-incf draws))
                  '((name . nael-margin-test)))
      (unwind-protect
          (progn
            (nael-margin-mode)
            (setq draws 0)
            (run-hook-with-args 'nael-eglot-diagnostics-functions [])
            (run-hook-with-args 'nael-eglot-file-progress-functions [])
            (run-hook-with-args 'nael-eglot-diagnostics-functions [])
            (should (zerop draws))
            (should (nael-margin-test-wait (lambda () (= draws 1))))
            (nael-margin-test-wait #'ignore 0.15)
            (should (= draws 1))
            (should (equal (nael-margin-test-drawn) '((2 "‼")))))
        (advice-remove 'nael-margin-draw 'nael-margin-test)
        (nael-margin-mode -1)))))

(ert-deftest nael-margin-test-update ()
  "The mode is on in managed buffers when option `nael-margin' is non-nil."
  (nael-margin-test-with-buffer nil
    (let ((managed t)
          (nael-margin t))
      (cl-letf (((symbol-function 'eglot-managed-p) (lambda () managed)))
        (nael-margin-update)
        (should nael-margin-mode)
        (setq managed nil)
        (nael-margin-update)
        (should-not nael-margin-mode)
        (should (eql left-margin-width 0))
        (setq managed t nael-margin nil)
        (nael-margin-update)
        (should-not nael-margin-mode)))))

(provide 'nael-margin-tests)

;;; nael-margin-tests.el ends here
