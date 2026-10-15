;;; lena-margin.el --- Signs in the margin of Lena buffers  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see LICENSE.GPL3.

;;; Commentary:

;; `lena-margin-mode' shows what the Lean server reports about a Lena
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
;; indicators in the fringe or margin are hidden in Lena buffers, since
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

(require 'lena-eglot)

(defgroup lena-margin nil
  "Signs in the left margin of Lena buffers."
  :group 'lena
  :link '(emacs-library-link :tag "Source Lisp File" "lena-margin.el")
  :prefix "lena-margin-")

(defcustom lena-margin t
  "Whether to show signs in the left margin of Lena buffers.
If non-nil, `lena-margin-mode' is turned on in Lena buffers that Eglot
manages, and the indicators of Flymake and Flycheck are hidden there.
Set this before Lena buffers are opened."
  :type 'boolean
  :group 'lena-margin)

(defcustom lena-margin-goals-accomplished "✓"
  "Sign on the first line of a proof that the server found complete.
The infoview shows it as well, after \"Goals accomplished\".  An empty
string shows no sign.  A sign has room for two columns.  Emoji fit
too, but with most fonts they make their lines taller."
  :type 'string
  :safe #'stringp
  :group 'lena-margin)

(defcustom lena-margin-unsolved-goals "✗"
  "Sign on the last line of a proof that has unsolved goals.
That is where the proof goes on.  An empty string shows the sign of the
error instead.  A sign has room for two columns.  Emoji fit too, but
with most fonts they make their lines taller."
  :type 'string
  :safe #'stringp
  :group 'lena-margin)

(defcustom lena-margin-admitted "⚠"
  "Sign on a declaration that uses `sorry'.
It replaces the sign of the server's warning that the declaration uses
`sorry'.  An empty string shows the sign of the warning instead.  The
sign has room for two columns.  Emoji fit too, but with most fonts they
make their lines taller."
  :type 'string
  :safe #'stringp
  :group 'lena-margin)

(defcustom lena-margin-emoji-height 1.0
  "Height of emoji in signs, relative to the text around them.
Emoji come from fonts of their own, which with most fonts make lines
taller than the others.  A smaller height can keep the lines as tall
as the others; which height does depends on the fonts, such as 0.7
for Apple Color Emoji next to a font of 12 points.  It also applies
to the sign after \"Goals accomplished\" in the infoview."
  :type 'number
  :safe #'numberp
  :group 'lena-margin)

(defcustom lena-margin-bar '("┌" "│" "└")
  "Signs for a diagnostic that spans several lines.
The first is shown on its first line, the second on the lines between,
and the third on its last line."
  :type '(list (string :tag "First line")
               (string :tag "Lines between")
               (string :tag "Last line"))
  :safe (lambda (value) (and (proper-list-p value) (seq-every-p #'stringp value)))
  :group 'lena-margin)

(defcustom lena-margin-processing "│"
  "Sign on lines that the server is still processing."
  :type 'string
  :safe #'stringp
  :group 'lena-margin)

(defface lena-margin-error '((t :inherit compilation-error))
  "Face for the signs of errors."
  :group 'lena-margin)

(defface lena-margin-warning '((t :inherit compilation-warning))
  "Face for the signs of warnings."
  :group 'lena-margin)

(defface lena-margin-info '((t :inherit compilation-info))
  "Face for the signs of information and hints."
  :group 'lena-margin)

(defface lena-margin-processing
  '((((class color) (min-colors 88)) :foreground "orange")
    (t :inherit warning))
  "Face for the signs of lines that the server is still processing."
  :group 'lena-margin)

(defface lena-margin-goals-accomplished '((t :inherit compilation-info))
  "Face for the sign of a complete proof."
  :group 'lena-margin)

(defface lena-margin-unsolved-goals '((t :inherit compilation-error))
  "Face for the sign of a proof with unsolved goals."
  :group 'lena-margin)

(defface lena-margin-admitted '((t :inherit compilation-warning))
  "Face for the sign of a declaration that uses `sorry'."
  :group 'lena-margin)

(defvar lena-margin-delay 0.1
  "Seconds to wait before drawing signs after the server reported.
The server reports progress often while it processes a file, and
diagnostics a few at a time; this draws once for each burst.")

(defconst lena-margin-width 2
  "Most columns that a sign in the left margin takes; longer ones are cut.
The margin is as wide as the widest sign, which may be wider in pixels;
see `lena-margin--needed-width'.")

;;;; Signs:

(defconst lena-margin--fallbacks
  '((lena-margin-goals-accomplished "*")
    (lena-margin-unsolved-goals "x")
    (lena-margin-admitted ""))
  "Signs to use instead of a default that cannot be displayed.")

(defun lena-margin--displayable-p (string)
  "Return non-nil if all characters of STRING can be displayed."
  (seq-every-p #'char-displayable-p string))

(defun lena-margin-sign (option)
  "Return the sign that OPTION, a symbol, holds in the current buffer.
OPTION is one of the options for signs, such as
`lena-margin-goals-accomplished'.
If it holds its default, which cannot be displayed, return the first
fallback in `lena-margin--fallbacks' that can.  Values that users set
are returned as they are."
  (let ((value (symbol-value option)))
    (if (or (not (equal value (eval (car (get option 'standard-value)) t)))
            (lena-margin--displayable-p value))
        value
      (or (seq-find #'lena-margin--displayable-p
                    (alist-get option lena-margin--fallbacks))
          ""))))

(defun lena-margin-scale-emoji (string)
  "Return STRING with its emoji at the height `lena-margin-emoji-height'."
  (let ((string (copy-sequence string)))
    (unless (= lena-margin-emoji-height 1)
      (dotimes (i (length string))
        (when (eq (aref char-script-table (aref string i)) 'emoji)
          (add-face-text-property i (1+ i)
                                  (list :height lena-margin-emoji-height)
                                  nil string))))
    string))

(defun lena-margin--severity (diagnostic)
  "Return the Flymake type and face for the severity of DIAGNOSTIC.
Like Eglot, take a missing severity for an error."
  (let ((severity (or (plist-get diagnostic :severity) 1)))
    (cond ((<= severity 1) '(error . lena-margin-error))
          ((= severity 2) '(warning . lena-margin-warning))
          (t '(note . lena-margin-info)))))

(defun lena-margin--severity-sign (type)
  "Return the sign for diagnostics of Flymake TYPE.
That is the user's string in `flymake-margin-indicators-string', or \"!\"
when it cannot be displayed."
  (let ((string (car (alist-get type flymake-margin-indicators-string))))
    (cond ((null string) "!")
          ((lena-margin--displayable-p string) string)
          ((equal string "‼") "!!")
          (t "!"))))

(defun lena-margin--admitted-p (diagnostic)
  "Return non-nil if DIAGNOSTIC says that a declaration uses `sorry'."
  (and (eql (plist-get diagnostic :severity) 2)
       (string-match-p "\\`declaration uses [`']"
                       (plist-get diagnostic :message))))

(defun lena-margin--bar-p (diagnostic line-count)
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
                      (lena-eglot-diagnostic-lines diagnostic t)))
           (> last first)))))

;; A sign is a list (RANK STRING FACE HELP).  Each line shows the sign
;; of the highest rank.  The rank is 10 times the class (4 errors, 3
;; warnings, 2 information and hints, 1 processing, 0 complete proofs)
;; plus the kind (3 signs for proofs, 2 first or single lines, 1 last
;; lines, 0 lines between).

(defun lena-margin-signs (diagnostics processing line-count)
  "Return the signs for DIAGNOSTICS and PROCESSING in the current buffer.
DIAGNOSTICS is as `lena-eglot-diagnostics' and PROCESSING as
`lena-eglot-file-progress'; LINE-COUNT is the number of lines in the
buffer.  The value is a list of (LINE RANK STRING FACE HELP), sorted by
LINE, which is 0-based; each line occurs at most once."
  (let ((signs (make-hash-table))
        (bar lena-margin-bar))
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
                     do (add line 1 0 (lena-margin-sign
                                       'lena-margin-processing)
                             'lena-margin-processing
                             "Lean is processing this line")))))
      (seq-doseq (diagnostic diagnostics)
        (let ((message (plist-get diagnostic :message)))
          (cond
           ((lena-eglot-diagnostic-tag-p diagnostic 'goals-accomplished)
            (add (car (lena-eglot-diagnostic-lines diagnostic)) 0 2
                 (lena-margin-sign 'lena-margin-goals-accomplished)
                 'lena-margin-goals-accomplished message))
           ((eq (plist-get diagnostic :isSilent) t))
           (t
            (pcase-let* ((`(,type . ,face) (lena-margin--severity diagnostic))
                         (class (pcase type ('error 4) ('warning 3) (_ 2)))
                         (`(,first . ,last)
                          (if (lena-margin--bar-p diagnostic line-count)
                              (lena-eglot-diagnostic-lines diagnostic t)
                            (let ((line (car (lena-eglot-diagnostic-lines
                                              diagnostic))))
                              (cons line line)))))
              (if (= first last)
                  (add first class 2 (lena-margin--severity-sign type)
                       face message)
                (add first class 2 (nth 0 bar) face message)
                (cl-loop for line from (1+ first) below last
                         do (add line class 0 (nth 1 bar) face message))
                (add last class 1 (nth 2 bar) face message))
              (cond
               ((lena-eglot-diagnostic-tag-p diagnostic 'unsolved-goals)
                (add last class 3
                     (lena-margin-sign 'lena-margin-unsolved-goals)
                     'lena-margin-unsolved-goals message))
               ((lena-margin--admitted-p diagnostic)
                (add first class 3
                     (lena-margin-sign 'lena-margin-admitted)
                     'lena-margin-admitted message)))))))))
    (sort (let (list)
            (maphash (lambda (line sign) (push (cons line sign) list)) signs)
            list)
          #'car-less-than-car)))

;;;; Drawing:

(defvar-keymap lena-margin-sign-map
  :doc "Keymap of the signs in the margin."
  "<left-margin> <mouse-1>" #'flymake-show-buffer-diagnostics)

(defvar lena-margin-mode)

(defvar-local lena-margin--overlays nil
  "The overlays that show the signs in the current buffer.")

(defvar-local lena-margin--timer nil
  "Timer that draws the signs of the current buffer, or nil.")

(defun lena-margin--string (string face help)
  "Return a string that shows STRING in FACE in the left margin.
HELP is shown in its tooltip."
  (let ((sign (lena-margin-scale-emoji
               (truncate-string-to-width string lena-margin-width))))
    ;; The face `margin' is new in Emacs 31.
    (add-face-text-property
     0 (length sign)
     `(:inherit (,face ,(if (facep 'margin) 'margin 'default))) t sign)
    (add-text-properties 0 (length sign)
                         `(help-echo ,help mouse-face highlight
                                     keymap ,lena-margin-sign-map)
                         sign)
    (propertize " " 'display `((margin left-margin) ,sign))))

(defun lena-margin--clear ()
  "Remove the signs of the current buffer and cancel drawing them."
  (mapc #'delete-overlay lena-margin--overlays)
  (setq lena-margin--overlays nil)
  (when lena-margin--timer
    (cancel-timer lena-margin--timer)
    (setq lena-margin--timer nil)))

(defun lena-margin-draw ()
  "Draw the signs of the current buffer from what the server reported."
  (lena-margin--clear)
  (lena-margin--widen)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((line 0))
        (pcase-dolist (`(,target ,_rank ,string ,face ,help)
                       (lena-margin-signs lena-eglot-diagnostics
                                          lena-eglot-file-progress
                                          (count-lines (point-min)
                                                       (point-max))))
          (when (zerop (forward-line (- target line)))
            (setq line target)
            (let ((overlay (make-overlay (point) (point))))
              (overlay-put overlay 'lena-margin t)
              (overlay-put overlay 'before-string
                           (lena-margin--string string face help))
              (push overlay lena-margin--overlays))))))))

(defun lena-margin--draw-later (buffer)
  "Draw the signs of BUFFER, which the timer was set for."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq lena-margin--timer nil)
      (when lena-margin-mode
        (lena-margin-draw)))))

(defun lena-margin--schedule (&rest _)
  "Draw the signs of the current buffer after `lena-margin-delay'.
Until then, later calls change nothing."
  (unless lena-margin--timer
    (setq lena-margin--timer
          (run-with-timer lena-margin-delay nil #'lena-margin--draw-later
                          (current-buffer)))))

;;;; The margin:

(defvar-local lena-margin--widened nil
  "Non-nil if `lena-margin-mode' widened the left margin.
The value is then a cons (BEFORE . WIDTH) of the width before, which
may be nil, and the width it set.")

(defun lena-margin--signs-in-use ()
  "Return the signs that the current buffer may show."
  (mapcar (lambda (sign) (truncate-string-to-width sign lena-margin-width))
          (append (mapcar #'lena-margin-sign
                          '(lena-margin-goals-accomplished
                            lena-margin-unsolved-goals
                            lena-margin-admitted
                            lena-margin-processing))
                  lena-margin-bar
                  (mapcar #'lena-margin--severity-sign
                          '(error warning note)))))

(defun lena-margin--needed-width ()
  "Return how many columns the left margin needs for all signs.
That is the width of the widest sign, and at least one column.  In
graphical frames, it is measured in pixels: emoji come from fonts of
their own, whose glyphs may be wider than their columns.  All signs
count, not only those shown, so that the margin does not change its
width when one first appears."
  (apply #'max 1
         (mapcar (lambda (sign)
                   (if (display-graphic-p)
                       (ceiling (string-pixel-width
                                 (lena-margin-scale-emoji sign))
                                (frame-char-width))
                     (string-width sign)))
                 (lena-margin--signs-in-use))))

(defun lena-margin--apply-width ()
  "Give the windows that show the current buffer its left margin width."
  (dolist (window (get-buffer-window-list nil nil t))
    (set-window-margins window left-margin-width
                        (cdr (window-margins window)))))

(defun lena-margin--widen ()
  "Make the left margin as wide as `lena-margin--needed-width' says.
Only widen it, and remember the width before, for `lena-margin--narrow'.
If the mode widened it already and nobody changed it since, the width
before stays the one from then."
  (let ((width (lena-margin--needed-width))
        (current left-margin-width))
    (when (< (or current 0) width)
      (setq lena-margin--widened
            (cons (if (and lena-margin--widened
                           (eql current (cdr lena-margin--widened)))
                      (car lena-margin--widened)
                    current)
                  width)
            left-margin-width width)
      (lena-margin--apply-width))))

(defun lena-margin--narrow ()
  "Give the left margin back the width it had before it was widened.
Leave it alone if someone else has changed it since."
  (when lena-margin--widened
    (when (eql left-margin-width (cdr lena-margin--widened))
      (setq left-margin-width (car lena-margin--widened))
      (lena-margin--apply-width))
    (setq lena-margin--widened nil)))

;;;; Flymake and Flycheck:

;; Flymake has no setting that hides its indicators reliably: with its
;; fringe and margin positions or `flymake-indicator-type' nil, Flymake
;; 1.4 shows the placeholder "!" of its indicator in the text instead.
;; It looks up the indicators of a diagnostic type in the obsolete
;; `flymake-diagnostic-types-alist' before the properties of the type,
;; so a buffer-local value of it gives Eglot's types an empty bitmap
;; and an empty margin string, in Lena buffers only.  Checked with
;; Flymake 1.3.7 (Emacs 30.2) and 1.4.5; remove this once Flymake can
;; hide its indicators without a "!" in the text.

(when (fboundp 'define-fringe-bitmap)
  (define-fringe-bitmap 'lena-margin-empty [0]))

(defvar lena-margin--empty-bitmap '(lena-margin-empty)
  "Fringe bitmap that shows nothing, for hiding Flymake's indicators.")

(defconst lena-margin--hidden-indicators
  (mapcar (lambda (type)
            `(,type (flymake-bitmap . lena-margin--empty-bitmap)
                    (flymake-margin-string "" default)))
          '(eglot-error eglot-warning eglot-note))
  "Entries of `flymake-diagnostic-types-alist' that hide indicators.")

(defvar flycheck-indication-mode)
(declare-function flycheck-buffer "flycheck" ())

(defun lena-margin--redo-checkers ()
  "Let Flymake or Flycheck show their diagnostics again."
  (when (bound-and-true-p flymake-mode)
    (flymake-start))
  (when (and (bound-and-true-p flycheck-mode) (fboundp 'flycheck-buffer))
    (flycheck-buffer)))

(defconst lena-margin--indicator-variables
  '(flymake-diagnostic-types-alist flymake-autoresize-margins
    flycheck-indication-mode)
  "Variables that `lena-margin--hide-indicators' sets buffer-locally.")

(defvar-local lena-margin--saved-indicators nil
  "Non-nil while `lena-margin--hide-indicators' hides the indicators.
The value is then a cons (t . SAVED), where SAVED has a cons (VARIABLE
. VALUE) for each of `lena-margin--indicator-variables' that was
buffer-local before.")

(defun lena-margin--hide-indicators ()
  "Hide the indicators of Flymake and Flycheck in the current buffer.
Also keep Flymake from resizing the margin, which it would do for its
indicators."
  (unless lena-margin--saved-indicators
    (setq lena-margin--saved-indicators
          (cons t (seq-keep (lambda (variable)
                              (and (local-variable-p variable)
                                   (cons variable (symbol-value variable))))
                            lena-margin--indicator-variables)))
    (with-suppressed-warnings ((obsolete flymake-diagnostic-types-alist))
      (setq-local flymake-diagnostic-types-alist
                  (append lena-margin--hidden-indicators
                          flymake-diagnostic-types-alist)))
    (setq-local flymake-autoresize-margins nil)
    (setq-local flycheck-indication-mode nil))
  (lena-margin--redo-checkers))

(defun lena-margin--show-indicators ()
  "Show the indicators of Flymake and Flycheck in the current buffer again.
Settings that were buffer-local before they were hidden come back."
  (when lena-margin--saved-indicators
    (mapc #'kill-local-variable lena-margin--indicator-variables)
    (pcase-dolist (`(,variable . ,value) (cdr lena-margin--saved-indicators))
      (set (make-local-variable variable) value))
    (setq lena-margin--saved-indicators nil))
  (lena-margin--redo-checkers))

;;;; The mode:

;;;###autoload
(define-minor-mode lena-margin-mode
  "Show signs in the left margin for what the Lean server reports.

The signs are, in this order of precedence:

- `‼' on the line of an error, `!' on warnings, information and
  hints, as in `flymake-margin-indicators-string', or a bar over the
  lines of one that spans several, as in `lena-margin-bar';
- on the last line of a proof with unsolved goals,
  `lena-margin-unsolved-goals' instead;
- on a declaration that uses `sorry', `lena-margin-admitted' instead;
- `lena-margin-processing' on lines that the server is processing;
- `lena-margin-goals-accomplished' on the first line of a proof that
  is complete.

Diagnostics stay with Flymake, which still underlines them and shows
their messages; its indicators are hidden while this mode is on.  The
mode needs a buffer that Eglot manages with Lena's server; option
`lena-margin' turns it on in all of them."
  :group 'lena-margin
  (lena-margin--clear)
  (if lena-margin-mode
      (progn
        (add-hook 'lena-eglot-diagnostics-functions
                  #'lena-margin--schedule nil t)
        (add-hook 'lena-eglot-file-progress-functions
                  #'lena-margin--schedule nil t)
        (lena-margin--hide-indicators)
        (lena-margin-draw))
    (remove-hook 'lena-eglot-diagnostics-functions
                 #'lena-margin--schedule t)
    (remove-hook 'lena-eglot-file-progress-functions
                 #'lena-margin--schedule t)
    (lena-margin--narrow)
    (lena-margin--show-indicators)))

(defun lena-margin-update ()
  "Turn `lena-margin-mode' on or off as option `lena-margin' says.
It is on in Lena buffers that Eglot manages.  For
`eglot-managed-mode-hook'."
  (let ((on (and lena-margin (eglot-managed-p) t)))
    (unless (eq on lena-margin-mode)
      (lena-margin-mode (if on 1 -1)))))

(provide 'lena-margin)

;;; lena-margin.el ends here
