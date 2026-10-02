;;; nael-infoview-buffer.el --- The buffer of Nael's infoview  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see LICENSE.GPL3.

;;; Commentary:

;; The buffer in which `nael-infoview' shows goals: its major mode,
;; rendering results into it while keeping folds and window positions,
;; its status, and the commands that act on its content.  When and what
;; to show is decided by `nael-infoview'.

;;; Code:

(require 'cl-lib)
(require 'face-remap)
(require 'outline)

(require 'nael-render)
(require 'nael-rpc)

(declare-function nael-infoview-refresh "nael-infoview" ())

(defgroup nael-infoview nil
  "Infoview for `nael-mode'."
  :group 'nael-eglot
  :link '(emacs-library-link :tag "Source Lisp File" "nael-infoview.el")
  :prefix "nael-infoview-")

(defconst nael-infoview-buffer-name "*Nael Infoview*")

(defface nael-infoview-point-node
  '((t :inherit highlight))
  "Face of the subterm at point in the infoview."
  :group 'nael-infoview)

(defface nael-infoview-filter-on
  '((t :inherit mode-line-emphasis))
  "Face of an active filter in the header line of the infoview."
  :group 'nael-infoview)

(defface nael-infoview-filter-off
  '((t :inherit shadow))
  "Face of an inactive filter in the header line of the infoview."
  :group 'nael-infoview)

;;;; State:

(defvar nael-infoview-options nil
  "View options of the infoview, as described in `nael-render-goals'.")

(defvar-local nael-infoview--owner nil
  "Owner of the references that the infoview shows.")

(defvar-local nael-infoview--status nil
  "Status of the infoview, shown in its header line.
nil when up to date, or one of `updating', `processing',
`unsupported', `no-server', or (error . MESSAGE).")

(defvar-local nael-infoview--location nil
  "Description of the position whose information is shown.")

(defvar-local nael-infoview--results nil
  "The shown results, as (GOALS TERM-GOAL), for rendering them again.")

(defvar-local nael-infoview--point-overlay nil
  "Overlay that highlights the subterm at point.")

(defvar-local nael-infoview--node-overlays nil
  "Hash table mapping each shown node to its overlay.
The overlay keeps the node's extent when text is inserted before it,
such as the placeholders of outline buttons.")

(defvar-local nael-infoview--dim-cookie nil
  "Face remapping that dims outdated content, or nil.")

;;;; The buffer:

(defvar-keymap nael-infoview-mode-map
  :doc "Keymap of `nael-infoview-mode'."
  "g" #'nael-infoview-refresh
  "n" #'nael-infoview-next-goal
  "p" #'nael-infoview-previous-goal
  "w" #'nael-infoview-copy-goal
  "i" #'nael-infoview-toggle-instances
  "t" #'nael-infoview-toggle-types
  "a" #'nael-infoview-toggle-inaccessible
  "l" #'nael-infoview-toggle-let-values
  "r" #'nael-infoview-toggle-reverse)

(defun nael-infoview--outline-level ()
  "Return the outline level of the heading at point."
  (or (get-text-property (point) 'outline-level) 1))

(define-derived-mode nael-infoview-mode special-mode "Nael Infoview"
  "Major mode of the Nael infoview, which shows goals at point.

Headings of sections and goals can be folded by clicking the triangle
before them, or with \\[outline-cycle] and \\[outline-cycle-buffer], as in
`outline-minor-mode'.

\\{nael-infoview-mode-map}"
  (setq-local revert-buffer-function
              (lambda (&rest _) (nael-infoview-refresh)))
  (setq-local header-line-format '(:eval (nael-infoview--header-line)))
  (setq-local outline-search-function #'outline-search-level
              outline-level #'nael-infoview--outline-level
              outline-minor-mode-cycle t
              ;; As in other special buffers, such as `describe-bindings'
              ;; and xref: buttons get a placeholder in the text, so that
              ;; they are followed by a space and `RET' works on them.
              outline-minor-mode-use-buttons 'insert)
  (outline-minor-mode)
  (add-hook 'post-command-hook #'nael-infoview--highlight-point nil t)
  (add-hook 'kill-buffer-hook #'nael-infoview--killed nil t))

(defun nael-infoview--buffer ()
  "Return the infoview buffer, or nil."
  (get-buffer nael-infoview-buffer-name))

(defun nael-infoview--get-buffer-create ()
  "Return the infoview buffer, creating it if needed."
  (or (nael-infoview--buffer)
      (with-current-buffer (get-buffer-create nael-infoview-buffer-name)
        (nael-infoview-mode)
        (current-buffer))))

(defun nael-infoview-visible-p ()
  "Return non-nil if the infoview is displayed in some window."
  (when-let* ((buffer (nael-infoview--buffer)))
    (get-buffer-window buffer t)))

(defconst nael-infoview--filters
  '((:hide-instances "instances" nael-infoview-toggle-instances)
    (:hide-types "types" nael-infoview-toggle-types)
    (:hide-inaccessible "✝" nael-infoview-toggle-inaccessible)
    (:hide-let-values "let values" nael-infoview-toggle-let-values)
    (:reverse "reverse" nael-infoview-toggle-reverse))
  "View options of the infoview: key, label and toggle command.")

(defun nael-infoview--filter-buttons ()
  "Return the clickable filter toggles for the header line."
  (mapconcat
   (pcase-lambda (`(,key ,label ,command))
     (propertize label
                 'face (if (plist-get nael-infoview-options key)
                           'nael-infoview-filter-on
                         'nael-infoview-filter-off)
                 'mouse-face 'mode-line-highlight
                 'help-echo (format "mouse-1: %s"
                                    (documentation command t))
                 'keymap (make-mode-line-mouse-map 'mouse-1 command)))
   nael-infoview--filters " "))

(defun nael-infoview--header-line ()
  "Return the header line of the infoview."
  (concat
   (nael-infoview--filter-buttons)
   "  "
   (or nael-infoview--location "")
   (pcase nael-infoview--status
     ('nil "")
     ('updating "  Updating…")
     ('processing "  Processing…")
     ('unsupported "  Needs Lean 4.30 or later; see C-h . for goals")
     ('no-server "  No Lean server")
     (`(error . ,message)
      (concat "  " (propertize (format "Error: %s" message)
                               'face 'error))))))

(defun nael-infoview--set-status (status)
  "Set the STATUS of the infoview and dim its content if outdated."
  (when-let* ((buffer (nael-infoview--buffer)))
    (with-current-buffer buffer
      (setq nael-infoview--status status)
      (let ((dim (memq status '(processing no-server unsupported))))
        (cond ((and dim (not nael-infoview--dim-cookie))
               (setq nael-infoview--dim-cookie
                     (face-remap-add-relative 'default 'shadow)))
              ((and (not dim) nael-infoview--dim-cookie)
               (face-remap-remove-relative nael-infoview--dim-cookie)
               (setq nael-infoview--dim-cookie nil))))
      (force-mode-line-update))))


(defun nael-infoview--killed ()
  "Release the references that the infoview shows."
  (when nael-infoview--owner
    (nael-rpc-owner-release nael-infoview--owner)
    (setq nael-infoview--owner nil)))

;;;; Windows:

(defun nael-infoview--window-anchor (window)
  "Return where WINDOW looks into the infoview, to restore it later.
The result is (START . POINT), each an anchor (GOAL LINE COLUMN): the
goal at the position, the line relative to the goal's first line, or
to the buffer if not in a goal, and the column."
  (cl-flet ((anchor (pos)
              (save-excursion
                (goto-char pos)
                (let* ((goal (get-text-property pos 'nael-goal))
                       (goal-start
                        (if goal
                            (or (previous-single-property-change
                                 (1+ pos) 'nael-goal nil (point-min))
                                (point-min))
                          (point-min))))
                  (list goal
                        (count-lines goal-start (line-beginning-position))
                        (current-column))))))
    (cons (anchor (window-start window))
          (anchor (window-point window)))))

(defun nael-infoview--anchor-position (anchor)
  "Return the position of ANCHOR in the current buffer."
  (pcase-let ((`(,goal ,line ,column) anchor))
    (save-excursion
      (goto-char (point-min))
      (when goal
        (if-let* ((match (text-property-search-forward 'nael-goal goal t)))
            (goto-char (prop-match-beginning match))
          (goto-char (point-min))))
      (forward-line line)
      (move-to-column column)
      (point))))

(defun nael-infoview--restore-window (window anchors)
  "Restore WINDOW to ANCHORS from `nael-infoview--window-anchor'."
  (set-window-start window (save-excursion
                             (goto-char (nael-infoview--anchor-position
                                         (car anchors)))
                             (line-beginning-position))
                    t)
  (set-window-point window (nael-infoview--anchor-position (cdr anchors))))

;;;; Showing results:

(defun nael-infoview--location (buffer pos)
  "Describe position POS in BUFFER for the header line."
  (with-current-buffer buffer
    (save-excursion
      (goto-char pos)
      (format "%s:%d:%d" (buffer-name) (line-number-at-pos)
              (current-column)))))

(defun nael-infoview--heading-key ()
  "Return a key for the heading at point that survives a refresh."
  (or (get-text-property (point) 'nael-goal)
      (get-text-property (point) 'nael-section)))

(defun nael-infoview--folded-keys ()
  "Return the keys of the folded headings of the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (keys)
      (unless (outline-on-heading-p) (outline-next-heading))
      (while (not (eobp))
        (when (outline-invisible-p (line-end-position))
          (push (nael-infoview--heading-key) keys))
        (outline-next-heading))
      keys)))

(defun nael-infoview--fold (keys)
  "Fold the headings of the current buffer whose key is in KEYS."
  (when keys
    (save-excursion
      (goto-char (point-min))
      (unless (outline-on-heading-p) (outline-next-heading))
      (while (not (eobp))
        (when (member (nael-infoview--heading-key) keys)
          (outline-hide-subtree))
        (outline-next-heading)))))

(defun nael-infoview--render ()
  "Render the shown results again, keeping folds and window positions."
  (let* ((rendered (apply #'nael-render-goals
                          (append nael-infoview--results
                                  (list nael-infoview-options))))
         (inhibit-read-only t)
         (folded (nael-infoview--folded-keys))
         (windows (mapcar (lambda (window)
                            (cons window
                                  (nael-infoview--window-anchor window)))
                          (get-buffer-window-list nil nil t))))
    (delete-all-overlays)
    ;; Outline inserts button placeholders from `after-change-functions';
    ;; they must come after the node overlays, which the renderer's
    ;; positions refer to.
    (let ((inhibit-modification-hooks t))
      (erase-buffer)
      (if (string-empty-p (nael-render-result-text rendered))
          (insert (propertize "No goals or expected type at point.\n"
                              'face 'shadow))
        (insert (nael-render-result-text rendered))))
    (setq nael-infoview--node-overlays (make-hash-table :test #'eq)
          nael-infoview--point-overlay nil)
    (dolist (node (nael-render-result-nodes rendered))
      (let ((overlay (make-overlay (nael-render-node-beg node)
                                   (nael-render-node-end node))))
        (overlay-put overlay 'mouse-face 'highlight)
        (puthash node overlay nael-infoview--node-overlays)))
    (when (fboundp 'outline--fix-up-all-buttons)
      (outline--fix-up-all-buttons))
    (nael-infoview--fold folded)
    (pcase-dolist (`(,window . ,anchors) windows)
      (nael-infoview--restore-window window anchors))
    (nael-infoview--highlight-point)))

(defun nael-infoview--show (goals term-goal owner location)
  "Show GOALS and TERM-GOAL, whose references OWNER keeps.
LOCATION describes where they were requested."
  (with-current-buffer (nael-infoview--get-buffer-create)
    (let ((previous nael-infoview--owner))
      (setq nael-infoview--results (list goals term-goal)
            nael-infoview--owner owner
            nael-infoview--location location)
      (nael-infoview--render)
      ;; Only now is nothing shown any longer that uses the old
      ;; references.
      (when previous (nael-rpc-owner-release previous))))
  (nael-infoview--set-status nil))

(defun nael-infoview--highlight-point ()
  "Highlight the subterm at point in the infoview."
  (if-let* ((node (nael-render-node-at (point)))
            (extent (and nael-infoview--node-overlays
                         (gethash node nael-infoview--node-overlays))))
      (if nael-infoview--point-overlay
          (move-overlay nael-infoview--point-overlay
                        (overlay-start extent) (overlay-end extent))
        (setq nael-infoview--point-overlay
              (make-overlay (overlay-start extent) (overlay-end extent)))
        (overlay-put nael-infoview--point-overlay
                     'face 'nael-infoview-point-node))
    (when nael-infoview--point-overlay
      (delete-overlay nael-infoview--point-overlay)
      (setq nael-infoview--point-overlay nil))))

;;;; Commands:

(defun nael-infoview--toggle-option (key)
  "Toggle the view option KEY and render the infoview again."
  (setq nael-infoview-options
        (plist-put (copy-sequence nael-infoview-options) key
                   (not (plist-get nael-infoview-options key))))
  (when-let* ((buffer (nael-infoview--buffer)))
    (with-current-buffer buffer
      (when nael-infoview--results
        (nael-infoview--render))
      (force-mode-line-update))))

(defun nael-infoview-toggle-instances ()
  "Toggle hiding instance hypotheses in the infoview."
  (interactive)
  (nael-infoview--toggle-option :hide-instances))

(defun nael-infoview-toggle-types ()
  "Toggle hiding hypotheses that are types in the infoview."
  (interactive)
  (nael-infoview--toggle-option :hide-types))

(defun nael-infoview-toggle-inaccessible ()
  "Toggle hiding inaccessible names, which contain \"✝\", in the infoview."
  (interactive)
  (nael-infoview--toggle-option :hide-inaccessible))

(defun nael-infoview-toggle-let-values ()
  "Toggle hiding the values of let-bound hypotheses in the infoview."
  (interactive)
  (nael-infoview--toggle-option :hide-let-values))

(defun nael-infoview-toggle-reverse ()
  "Toggle showing targets before hypotheses in the infoview."
  (interactive)
  (nael-infoview--toggle-option :reverse))

(defun nael-infoview--goal-start (pos)
  "Return the start of the goal at POS, or nil if POS is in no goal."
  (when (get-text-property pos 'nael-goal)
    (if (and (> pos (point-min))
             (equal (get-text-property (1- pos) 'nael-goal)
                    (get-text-property pos 'nael-goal)))
        (previous-single-property-change pos 'nael-goal nil (point-min))
      pos)))

(defun nael-infoview-next-goal (&optional n)
  "Move to the start of the Nth next goal in the infoview."
  (interactive "p")
  (dotimes (_ (or n 1))
    (let ((pos (point)))
      (when (get-text-property pos 'nael-goal)
        (setq pos (next-single-property-change pos 'nael-goal nil
                                               (point-max))))
      (unless (get-text-property pos 'nael-goal)
        (setq pos (next-single-property-change pos 'nael-goal)))
      (if pos (goto-char pos) (user-error "No next goal")))))

(defun nael-infoview-previous-goal (&optional n)
  "Move to the start of the Nth previous goal in the infoview."
  (interactive "p")
  (dotimes (_ (or n 1))
    (let* ((start (nael-infoview--goal-start (point)))
           (pos (if (and start (< start (point))) start
                  (let ((before (previous-single-property-change
                                 (or start (point)) 'nael-goal)))
                    (and before
                         (nael-infoview--goal-start
                          (if (get-text-property before 'nael-goal)
                              before
                            (max (point-min) (1- before)))))))))
      (if pos (goto-char pos) (user-error "No previous goal")))))

(defun nael-infoview--text (beg end)
  "Return the text between BEG and END without outline button placeholders."
  (let ((pos beg)
        (parts nil))
    (dolist (overlay (sort (seq-filter (lambda (o) (overlay-get o 'outline-button))
                                       (overlays-in beg end))
                           (lambda (a b) (< (overlay-start a) (overlay-start b)))))
      (push (buffer-substring-no-properties pos (overlay-start overlay)) parts)
      ;; `outline--insert-button' inserts two spaces per button.
      (setq pos (min end (+ 2 (overlay-start overlay)))))
    (push (buffer-substring-no-properties pos end) parts)
    (apply #'concat (nreverse parts))))

(defun nael-infoview-copy-goal ()
  "Copy the goal at point in the infoview as plain text."
  (interactive)
  (let ((start (or (nael-infoview--goal-start (point))
                   (user-error "No goal at point"))))
    (kill-new (string-trim-right
               (nael-infoview--text
                start (next-single-property-change start 'nael-goal nil
                                                   (point-max)))))
    (message "Copied goal")))

(provide 'nael-infoview-buffer)

;;; nael-infoview-buffer.el ends here
