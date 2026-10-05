;;; nael-margin.el --- Signs in the margin of Nael buffers  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see LICENSE.GPL3.

;;; Commentary:

;; `nael-margin-mode' shows what the Lean server reports about a Nael
;; buffer as signs in its left margin:
;;
;; - a sign on the line of each diagnostic, and a bar over the lines
;;   of a diagnostic that spans several;
;; - signs for proofs that are complete, that have unsolved goals, or
;;   that use `sorry';
;; - a bar on the lines that the server is still processing.
;;
;; Flymake keeps showing the diagnostics themselves: their underlines,
;; their messages in the echo area, and its list of them.  Only its
;; indicators in the fringe or margin are hidden in Nael buffers, since
;; it can only mark the first line of a diagnostic.  The server
;; shortens the range of a diagnostic to its first line and sends its
;; full range separately, which Eglot does not pass on to Flymake.
;;
;; The signs and when they are shown follow lean.nvim
;; <https://github.com/Julian/lean.nvim>; no code of it is used.  The
;; sign for proofs that use `sorry' is our own.

;;; Code:

(require 'cl-lib)
(require 'flymake)
(require 'seq)

(require 'nael-eglot)

(defgroup nael-margin nil
  "Signs in the left margin of Nael buffers."
  :group 'nael
  :link '(emacs-library-link :tag "Source Lisp File" "nael-margin.el")
  :prefix "nael-margin-")

(defcustom nael-margin t
  "Whether to show signs in the left margin of Nael buffers.
If non-nil, `nael-margin-mode' is turned on in Nael buffers that Eglot
manages, and the indicators of Flymake and Flycheck are hidden there.
Set this before Nael buffers are opened."
  :type 'boolean
  :group 'nael-margin)

(defcustom nael-margin-goals-accomplished "✓"
  "Sign on the first line of a proof that the server found complete.
The infoview shows it as well, after \"Goals accomplished\".  An empty
string shows no sign.  The margin has room for two columns.  Emoji
fit too, but with most fonts they make their lines taller."
  :type 'string
  :safe #'stringp
  :group 'nael-margin)

(defcustom nael-margin-unsolved-goals "✗"
  "Sign on the last line of a proof that has unsolved goals.
That is where the proof goes on.  An empty string shows the sign of the
error instead.  The margin has room for two columns.  Emoji fit too,
but with most fonts they make their lines taller."
  :type 'string
  :safe #'stringp
  :group 'nael-margin)

(defcustom nael-margin-admitted "⚠"
  "Sign on a declaration that uses `sorry'.
It replaces the sign of the server's warning that the declaration uses
`sorry'.  An empty string shows the sign of the warning instead.  The
margin has room for two columns.  Emoji fit too, but with most fonts
they make their lines taller."
  :type 'string
  :safe #'stringp
  :group 'nael-margin)

(defcustom nael-margin-bar '("┌" "│" "└")
  "Signs for a diagnostic that spans several lines.
The first is shown on its first line, the second on the lines between,
and the third on its last line."
  :type '(list (string :tag "First line")
               (string :tag "Lines between")
               (string :tag "Last line"))
  :safe (lambda (value) (and (proper-list-p value) (seq-every-p #'stringp value)))
  :group 'nael-margin)

(defcustom nael-margin-processing "│"
  "Sign on lines that the server is still processing."
  :type 'string
  :safe #'stringp
  :group 'nael-margin)

(defface nael-margin-error '((t :inherit compilation-error))
  "Face for the signs of errors."
  :group 'nael-margin)

(defface nael-margin-warning '((t :inherit compilation-warning))
  "Face for the signs of warnings."
  :group 'nael-margin)

(defface nael-margin-info '((t :inherit compilation-info))
  "Face for the signs of information and hints."
  :group 'nael-margin)

(defface nael-margin-processing
  '((((class color) (min-colors 88)) :foreground "orange")
    (t :inherit warning))
  "Face for the signs of lines that the server is still processing."
  :group 'nael-margin)

(defface nael-margin-goals-accomplished '((t :inherit compilation-info))
  "Face for the sign of a complete proof."
  :group 'nael-margin)

(defface nael-margin-unsolved-goals '((t :inherit compilation-error))
  "Face for the sign of a proof with unsolved goals."
  :group 'nael-margin)

(defface nael-margin-admitted '((t :inherit compilation-warning))
  "Face for the sign of a declaration that uses `sorry'."
  :group 'nael-margin)

(defvar nael-margin-delay 0.1
  "Seconds to wait before drawing signs after the server reported.
The server reports progress often while it processes a file, and
diagnostics a few at a time; this draws once for each burst.")

(defconst nael-margin-width 2
  "Width of the signs in the left margin, in columns.
The margin is at least as wide, and wider where the signs are wider in
pixels; see `nael-margin--needed-width'.")

;;;; Signs:

(defconst nael-margin--fallbacks
  '((nael-margin-goals-accomplished "*")
    (nael-margin-unsolved-goals "x")
    (nael-margin-admitted ""))
  "Signs to use instead of a default that cannot be displayed.")

(defun nael-margin--displayable-p (string)
  "Return non-nil if all characters of STRING can be displayed."
  (seq-every-p #'char-displayable-p string))

(defun nael-margin-sign (option)
  "Return the sign that OPTION, a symbol, holds in the current buffer.
OPTION is one of the options for signs, such as
`nael-margin-goals-accomplished'.
If it holds its default, which cannot be displayed, return the first
fallback in `nael-margin--fallbacks' that can.  Values that users set
are returned as they are."
  (let ((value (symbol-value option)))
    (if (or (not (equal value (eval (car (get option 'standard-value)) t)))
            (nael-margin--displayable-p value))
        value
      (or (seq-find #'nael-margin--displayable-p
                    (alist-get option nael-margin--fallbacks))
          ""))))

(defun nael-margin--severity (diagnostic)
  "Return the Flymake type and face for the severity of DIAGNOSTIC.
Like Eglot, take a missing severity for an error."
  (let ((severity (or (plist-get diagnostic :severity) 1)))
    (cond ((<= severity 1) '(error . nael-margin-error))
          ((= severity 2) '(warning . nael-margin-warning))
          (t '(note . nael-margin-info)))))

(defun nael-margin--severity-sign (type)
  "Return the sign for diagnostics of Flymake TYPE.
That is the user's string in `flymake-margin-indicators-string', or \"!\"
when it cannot be displayed."
  (let ((string (car (alist-get type flymake-margin-indicators-string))))
    (cond ((null string) "!")
          ((nael-margin--displayable-p string) string)
          ((equal string "‼") "!!")
          (t "!"))))

(defun nael-margin--admitted-p (diagnostic)
  "Return non-nil if DIAGNOSTIC says that a declaration uses `sorry'."
  (and (eql (plist-get diagnostic :severity) 2)
       (string-match-p "\\`declaration uses [`']"
                       (plist-get diagnostic :message))))

(defun nael-margin--bar-p (diagnostic line-count)
  "Return non-nil if DIAGNOSTIC gets a bar over its lines.
That is the case when the server shortened its range, unless its full
range is the whole buffer of LINE-COUNT lines, as for messages about
imports.  The same criterion as lean.nvim's."
  (let ((range (plist-get diagnostic :range))
        (full (plist-get diagnostic :fullRange)))
    (and full
         (> (plist-get (plist-get full :end) :line)
            (plist-get (plist-get range :end) :line))
         (not (and (zerop (plist-get (plist-get full :start) :line))
                   (>= (plist-get (plist-get full :end) :line)
                       (1- line-count))))
         (pcase-let ((`(,first . ,last)
                      (nael-eglot-diagnostic-lines diagnostic t)))
           (> last first)))))

;; A sign is a list (RANK STRING FACE HELP).  Each line shows the sign
;; of the highest rank.  The rank is 10 times the class (4 errors, 3
;; warnings, 2 information and hints, 1 processing, 0 complete proofs)
;; plus the kind (3 signs for proofs, 2 first or single lines, 1 last
;; lines, 0 lines between).

(defun nael-margin-signs (diagnostics processing line-count)
  "Return the signs for DIAGNOSTICS and PROCESSING in the current buffer.
DIAGNOSTICS is as `nael-eglot-diagnostics' and PROCESSING as
`nael-eglot-file-progress'; LINE-COUNT is the number of lines in the
buffer.  The value is a list of (LINE RANK STRING FACE HELP), sorted by
LINE, which is 0-based; each line occurs at most once."
  (let ((signs (make-hash-table))
        (bar nael-margin-bar))
    (cl-flet ((add (line class kind string face help)
                (let ((rank (+ (* 10 class) kind))
                      (old (gethash line signs)))
                  (when (and string (not (string-empty-p string))
                             (or (null old) (> rank (car old))))
                    (puthash line (list rank string face help) signs)))))
      (seq-doseq (item processing)
        (when (memql (plist-get item :kind) '(nil 1))
          (let ((range (plist-get item :range)))
            (cl-loop for line from (plist-get (plist-get range :start) :line)
                     to (min (plist-get (plist-get range :end) :line)
                             (1- line-count))
                     do (add line 1 0 (nael-margin-sign
                                       'nael-margin-processing)
                             'nael-margin-processing
                             "Lean is processing this line")))))
      (seq-doseq (diagnostic diagnostics)
        (let ((message (plist-get diagnostic :message)))
          (cond
           ((nael-eglot-diagnostic-tag-p diagnostic 'goals-accomplished)
            (add (car (nael-eglot-diagnostic-lines diagnostic)) 0 2
                 (nael-margin-sign 'nael-margin-goals-accomplished)
                 'nael-margin-goals-accomplished message))
           ((eq (plist-get diagnostic :isSilent) t))
           (t
            (pcase-let* ((`(,type . ,face) (nael-margin--severity diagnostic))
                         (class (pcase type ('error 4) ('warning 3) (_ 2)))
                         (`(,first . ,last)
                          (if (nael-margin--bar-p diagnostic line-count)
                              (nael-eglot-diagnostic-lines diagnostic t)
                            (let ((line (car (nael-eglot-diagnostic-lines
                                              diagnostic))))
                              (cons line line)))))
              (if (= first last)
                  (add first class 2 (nael-margin--severity-sign type)
                       face message)
                (add first class 2 (nth 0 bar) face message)
                (cl-loop for line from (1+ first) below last
                         do (add line class 0 (nth 1 bar) face message))
                (add last class 1 (nth 2 bar) face message))
              (cond
               ((nael-eglot-diagnostic-tag-p diagnostic 'unsolved-goals)
                (add last class 3
                     (nael-margin-sign 'nael-margin-unsolved-goals)
                     'nael-margin-unsolved-goals message))
               ((nael-margin--admitted-p diagnostic)
                (add first class 3
                     (nael-margin-sign 'nael-margin-admitted)
                     'nael-margin-admitted message)))))))))
    (sort (let (list)
            (maphash (lambda (line sign) (push (cons line sign) list)) signs)
            list)
          #'car-less-than-car)))

;;;; Drawing:

(defvar-keymap nael-margin-sign-map
  :doc "Keymap of the signs in the margin."
  "<left-margin> <mouse-1>" #'flymake-show-buffer-diagnostics)

(defvar nael-margin-mode)

(defvar-local nael-margin--overlays nil
  "The overlays that show the signs in the current buffer.")

(defvar-local nael-margin--timer nil
  "Timer that draws the signs of the current buffer, or nil.")

(defun nael-margin--string (string face help)
  "Return a string that shows STRING in FACE in the left margin.
HELP is shown in its tooltip."
  (propertize
   " " 'display
   `((margin left-margin)
     ,(propertize (truncate-string-to-width string nael-margin-width)
                  ;; The face `margin' is new in Emacs 31.
                  'face `(:inherit (,face ,(if (facep 'margin) 'margin 'default)))
                  'help-echo help
                  'mouse-face 'highlight
                  'keymap nael-margin-sign-map))))

(defun nael-margin--clear ()
  "Remove the signs of the current buffer and cancel drawing them."
  (mapc #'delete-overlay nael-margin--overlays)
  (setq nael-margin--overlays nil)
  (when nael-margin--timer
    (cancel-timer nael-margin--timer)
    (setq nael-margin--timer nil)))

(defun nael-margin-draw ()
  "Draw the signs of the current buffer from what the server reported."
  (nael-margin--clear)
  (nael-margin--widen)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((line 0))
        (pcase-dolist (`(,target ,_rank ,string ,face ,help)
                       (nael-margin-signs nael-eglot-diagnostics
                                          nael-eglot-file-progress
                                          (count-lines (point-min)
                                                       (point-max))))
          (when (zerop (forward-line (- target line)))
            (setq line target)
            (let ((overlay (make-overlay (point) (point))))
              (overlay-put overlay 'nael-margin t)
              (overlay-put overlay 'before-string
                           (nael-margin--string string face help))
              (push overlay nael-margin--overlays))))))))

(defun nael-margin--draw-later (buffer)
  "Draw the signs of BUFFER, which the timer was set for."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq nael-margin--timer nil)
      (when nael-margin-mode
        (nael-margin-draw)))))

(defun nael-margin--schedule (&rest _)
  "Draw the signs of the current buffer after `nael-margin-delay'.
Until then, later calls change nothing."
  (unless nael-margin--timer
    (setq nael-margin--timer
          (run-with-timer nael-margin-delay nil #'nael-margin--draw-later
                          (current-buffer)))))

;;;; The margin:

(defvar-local nael-margin--widened nil
  "Non-nil if `nael-margin-mode' widened the left margin.
The value is then a cons (BEFORE . WIDTH) of the width before, which
may be nil, and the width it set.")

(defun nael-margin--signs-in-use ()
  "Return the signs that the current buffer may show."
  (mapcar (lambda (sign) (truncate-string-to-width sign nael-margin-width))
          (append (mapcar #'nael-margin-sign
                          '(nael-margin-goals-accomplished
                            nael-margin-unsolved-goals
                            nael-margin-admitted
                            nael-margin-processing))
                  nael-margin-bar
                  (mapcar #'nael-margin--severity-sign
                          '(error warning note)))))

(defun nael-margin--needed-width ()
  "Return how many columns the left margin needs for all signs.
That is `nael-margin-width', unless a sign is wider in pixels than so
many columns of the default font: emoji come from fonts of their own,
whose glyphs may be wider.  All signs count, not only those shown, so
that the margin does not change its width when one first appears."
  (if (not (display-graphic-p))
      nael-margin-width
    (apply #'max nael-margin-width
           (mapcar (lambda (sign)
                     (ceiling (string-pixel-width sign) (frame-char-width)))
                   (nael-margin--signs-in-use)))))

(defun nael-margin--apply-width ()
  "Give the windows that show the current buffer its left margin width."
  (dolist (window (get-buffer-window-list nil nil t))
    (set-window-margins window left-margin-width
                        (cdr (window-margins window)))))

(defun nael-margin--widen ()
  "Make the left margin as wide as `nael-margin--needed-width' says.
Only widen it, and remember the width before, for `nael-margin--narrow'.
If the mode widened it already and nobody changed it since, the width
before stays the one from then."
  (let ((width (nael-margin--needed-width))
        (current left-margin-width))
    (when (< (or current 0) width)
      (setq nael-margin--widened
            (cons (if (and nael-margin--widened
                           (eql current (cdr nael-margin--widened)))
                      (car nael-margin--widened)
                    current)
                  width)
            left-margin-width width)
      (nael-margin--apply-width))))

(defun nael-margin--narrow ()
  "Give the left margin back the width it had before it was widened.
Leave it alone if someone else has changed it since."
  (when nael-margin--widened
    (when (eql left-margin-width (cdr nael-margin--widened))
      (setq left-margin-width (car nael-margin--widened))
      (nael-margin--apply-width))
    (setq nael-margin--widened nil)))

;;;; Flymake and Flycheck:

;; Flymake has no setting that hides its indicators reliably: with its
;; fringe and margin positions or `flymake-indicator-type' nil, Flymake
;; 1.4 shows the placeholder "!" of its indicator in the text instead.
;; It looks up the indicators of a diagnostic type in the obsolete
;; `flymake-diagnostic-types-alist' before the properties of the type,
;; so a buffer-local value of it gives Eglot's types an empty bitmap
;; and an empty margin string, in Nael buffers only.  Checked with
;; Flymake 1.3.7 (Emacs 30.2) and 1.4.5; remove this once Flymake can
;; hide its indicators without a "!" in the text.

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'nael-margin-empty [0]))

(defvar nael-margin--empty-bitmap '(nael-margin-empty)
  "Fringe bitmap that shows nothing, for hiding Flymake's indicators.")

(defconst nael-margin--hidden-indicators
  (mapcar (lambda (type)
            `(,type (flymake-bitmap . nael-margin--empty-bitmap)
                    (flymake-margin-string "" default)))
          '(eglot-error eglot-warning eglot-note))
  "Entries of `flymake-diagnostic-types-alist' that hide indicators.")

(defvar flycheck-indication-mode)
(declare-function flycheck-buffer "flycheck" ())

(defun nael-margin--redo-checkers ()
  "Let Flymake or Flycheck show their diagnostics again."
  (when (bound-and-true-p flymake-mode)
    (flymake-start))
  (when (and (bound-and-true-p flycheck-mode) (fboundp 'flycheck-buffer))
    (flycheck-buffer)))

(defconst nael-margin--indicator-variables
  '(flymake-diagnostic-types-alist flymake-autoresize-margins
    flycheck-indication-mode)
  "Variables that `nael-margin--hide-indicators' sets buffer-locally.")

(defvar-local nael-margin--saved-indicators nil
  "Non-nil while `nael-margin--hide-indicators' hides the indicators.
The value is then a cons (t . SAVED), where SAVED has a cons (VARIABLE
. VALUE) for each of `nael-margin--indicator-variables' that was
buffer-local before.")

(defun nael-margin--hide-indicators ()
  "Hide the indicators of Flymake and Flycheck in the current buffer.
Also keep Flymake from resizing the margin, which it would do for its
indicators."
  (unless nael-margin--saved-indicators
    (setq nael-margin--saved-indicators
          (cons t (seq-keep (lambda (variable)
                              (and (local-variable-p variable)
                                   (cons variable (symbol-value variable))))
                            nael-margin--indicator-variables)))
    (with-suppressed-warnings ((obsolete flymake-diagnostic-types-alist))
      (setq-local flymake-diagnostic-types-alist
                  (append nael-margin--hidden-indicators
                          flymake-diagnostic-types-alist)))
    (setq-local flymake-autoresize-margins nil)
    (setq-local flycheck-indication-mode nil))
  (nael-margin--redo-checkers))

(defun nael-margin--show-indicators ()
  "Show the indicators of Flymake and Flycheck in the current buffer again.
Settings that were buffer-local before they were hidden come back."
  (when nael-margin--saved-indicators
    (mapc #'kill-local-variable nael-margin--indicator-variables)
    (pcase-dolist (`(,variable . ,value) (cdr nael-margin--saved-indicators))
      (set (make-local-variable variable) value))
    (setq nael-margin--saved-indicators nil))
  (nael-margin--redo-checkers))

;;;; The mode:

;;;###autoload
(define-minor-mode nael-margin-mode
  "Show signs in the left margin for what the Lean server reports.

The signs are, in this order of precedence:

- `‼' on the line of an error, `!' on warnings, information and
  hints, as in `flymake-margin-indicators-string', or a bar over the
  lines of one that spans several, as in `nael-margin-bar';
- on the last line of a proof with unsolved goals,
  `nael-margin-unsolved-goals' instead;
- on a declaration that uses `sorry', `nael-margin-admitted' instead;
- `nael-margin-processing' on lines that the server is processing;
- `nael-margin-goals-accomplished' on the first line of a proof that
  is complete.

Diagnostics stay with Flymake, which still underlines them and shows
their messages; its indicators are hidden while this mode is on.  The
mode needs a buffer that Eglot manages with Nael's server; option
`nael-margin' turns it on in all of them."
  :group 'nael-margin
  (nael-margin--clear)
  (if nael-margin-mode
      (progn
        (add-hook 'nael-eglot-diagnostics-functions
                  #'nael-margin--schedule nil t)
        (add-hook 'nael-eglot-file-progress-functions
                  #'nael-margin--schedule nil t)
        (nael-margin--hide-indicators)
        (nael-margin-draw))
    (remove-hook 'nael-eglot-diagnostics-functions
                 #'nael-margin--schedule t)
    (remove-hook 'nael-eglot-file-progress-functions
                 #'nael-margin--schedule t)
    (nael-margin--narrow)
    (nael-margin--show-indicators)))

(defun nael-margin-update ()
  "Turn `nael-margin-mode' on or off as option `nael-margin' says.
It is on in Nael buffers that Eglot manages.  For
`eglot-managed-mode-hook'."
  (let ((on (and nael-margin (eglot-managed-p) t)))
    (unless (eq on nael-margin-mode)
      (nael-margin-mode (if on 1 -1)))))

(provide 'nael-margin)

;;; nael-margin.el ends here
