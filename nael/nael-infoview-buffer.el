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

;;;; State:

(defcustom nael-infoview-hide-instances nil
  "Whether the infoview hides instance hypotheses, like `inst✝ : Ring R'.
Toggle it in the infoview with \\<nael-infoview-mode-map>\\[nael-infoview-toggle-instances]."
  :type 'boolean
  :group 'nael-infoview)

(defcustom nael-infoview-hide-types nil
  "Whether the infoview hides hypotheses that are types, like `α : Type'.
Toggle it in the infoview with \\<nael-infoview-mode-map>\\[nael-infoview-toggle-types]."
  :type 'boolean
  :group 'nael-infoview)

(defcustom nael-infoview-hide-inaccessible nil
  "Whether the infoview hides inaccessible names, which contain \"✝\".
Toggle it in the infoview with \\<nael-infoview-mode-map>\\[nael-infoview-toggle-inaccessible]."
  :type 'boolean
  :group 'nael-infoview)

(defcustom nael-infoview-hide-let-values nil
  "Whether the infoview hides the values of let-bound hypotheses.
Toggle it in the infoview with \\<nael-infoview-mode-map>\\[nael-infoview-toggle-let-values]."
  :type 'boolean
  :group 'nael-infoview)

(defcustom nael-infoview-reverse nil
  "Whether the infoview shows targets first, then hypotheses upwards.
Toggle it in the infoview with \\<nael-infoview-mode-map>\\[nael-infoview-toggle-reverse]."
  :type 'boolean
  :group 'nael-infoview)

(defcustom nael-infoview-show-diffs t
  "Whether the infoview marks what the tactic at point changes.
The marks use the faces `nael-render-diff-inserted' and
`nael-render-diff-removed'.  Toggle it in the infoview with
\\<nael-infoview-mode-map>\\[nael-infoview-toggle-diffs]."
  :type 'boolean
  :group 'nael-infoview)

(defcustom nael-infoview-expected-type 'expanded
  "How the infoview shows the expected type at point.
`expanded' shows it, `collapsed' shows its heading folded, and `hidden'
does not request it at all."
  :type '(choice (const :tag "Expanded" expanded)
                 (const :tag "Collapsed" collapsed)
                 (const :tag "Hidden" hidden))
  :group 'nael-infoview)

(defun nael-infoview--options ()
  "Return the view options for `nael-render-goals'."
  (list :hide-instances nael-infoview-hide-instances
        :hide-types nael-infoview-hide-types
        :hide-inaccessible nael-infoview-hide-inaccessible
        :hide-let-values nael-infoview-hide-let-values
        :reverse nael-infoview-reverse
        :hide-diffs (not nael-infoview-show-diffs)))

(defvar-local nael-infoview--owner nil
  "Owner of the references that the infoview shows.")

(defvar-local nael-infoview--status nil
  "Status of the infoview, shown in its mode line.
nil when up to date, or one of `updating', `processing',
`unsupported', `no-server', or (error . MESSAGE).")

(defvar-local nael-infoview--location nil
  "Description of the position whose information is shown.")

(defvar-local nael-infoview--results nil
  "The shown results, as (GOALS TERM-GOAL), for rendering them again.")

(defvar-local nael-infoview--point-overlay nil
  "Overlay that highlights the subterm at point.")

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
  "r" #'nael-infoview-toggle-reverse
  "d" #'nael-infoview-toggle-diffs)

(defun nael-infoview--outline-level ()
  "Return the outline level of the heading at point."
  (or (get-text-property (point) 'outline-level) 1))

(define-derived-mode nael-infoview-mode special-mode "Nael Infoview"
  "Major mode of the Nael infoview, which shows goals at point.

Headings of sections and goals can be folded by clicking the triangle
in the margin before them, or with \\[outline-cycle] and \\[outline-cycle-buffer], as in
`outline-minor-mode'.

\\{nael-infoview-mode-map}"
  (setq-local revert-buffer-function
              (lambda (&rest _) (nael-infoview-refresh)))
  (setq-local mode-line-process '(:eval (nael-infoview--mode-line-process)))
  (setq-local outline-search-function #'outline-search-level
              outline-level #'nael-infoview--outline-level
              outline-minor-mode-cycle t
              ;; Buttons in the margin leave the text alone, so that
              ;; positions in the buffer are those of the renderer.
              outline-minor-mode-use-buttons 'in-margins)
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

(defun nael-infoview--mode-line-process ()
  "Return the location and status of the infoview for its mode line.
This is the value of `mode-line-process', which is where buffers of
processes such as compilations show their state."
  (concat
   (when nael-infoview--location
     (concat " " nael-infoview--location))
   (pcase nael-infoview--status
     ('nil nil)
     ('updating " updating")
     ('processing " processing")
     ('unsupported
      (propertize " needs Lean 4.30" 'help-echo
                  "Lean 4.30 or later is needed; C-h . shows the goals"))
     ('no-server " no server")
     (`(error . ,message)
      (propertize " error" 'face 'error 'help-echo message)))))

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
key of the goal at the position (see `nael-key' in `nael-render'), the
line relative to the goal's first line, or to the buffer if not in a
goal, and the column."
  (cl-flet ((anchor (pos)
              (save-excursion
                (goto-char pos)
                (let* ((goal (get-text-property pos 'nael-key))
                       (goal-start
                        (if goal
                            (or (previous-single-property-change
                                 (1+ pos) 'nael-key nil (point-min))
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
        (if-let* ((match (text-property-search-forward 'nael-key goal t)))
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
  "Describe position POS in BUFFER for the mode line."
  (with-current-buffer buffer
    (save-excursion
      (goto-char pos)
      (format "%s:%d:%d" (buffer-name) (line-number-at-pos)
              (current-column)))))

(defun nael-infoview--heading-key ()
  "Return a key for the heading at point that survives a refresh.
Goals are identified by their place, see `nael-key' in `nael-render',
so that folds survive moving to another tactic."
  (or (get-text-property (point) 'nael-section)
      (get-text-property (point) 'nael-key)))

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

(defun nael-infoview--heading-keys ()
  "Return the keys of all headings of the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (keys)
      (unless (outline-on-heading-p) (outline-next-heading))
      (while (not (eobp))
        (push (nael-infoview--heading-key) keys)
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
                                  (list (nael-infoview--options)))))
         (inhibit-read-only t)
         (folded (nael-infoview--folded-keys))
         (present (nael-infoview--heading-keys))
         (windows (mapcar (lambda (window)
                            (cons window
                                  (nael-infoview--window-anchor window)))
                          (get-buffer-window-list nil nil t))))
    (delete-all-overlays)
    (erase-buffer)
    ;; Outline adds its buttons from `after-change-functions'.
    (if (string-empty-p (nael-render-result-text rendered))
        (insert (propertize "No goals or expected type at point.\n"
                            'face 'shadow))
      (insert (nael-render-result-text rendered)))
    (setq nael-infoview--point-overlay nil)
    (dolist (node (nael-render-result-nodes rendered))
      (overlay-put (make-overlay (nael-render-node-beg node)
                                 (nael-render-node-end node))
                   'mouse-face 'highlight))
    (nael-infoview--fold
     ;; The expected type starts folded if so customized, and remembers
     ;; being unfolded only while it stays.
     (if (and (eq nael-infoview-expected-type 'collapsed)
              (not (memq 'term-goal present)))
         (cons 'term-goal folded)
       folded))
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
  (if-let* ((node (nael-render-node-at (point))))
      (if nael-infoview--point-overlay
          (move-overlay nael-infoview--point-overlay
                        (nael-render-node-beg node)
                        (nael-render-node-end node))
        (setq nael-infoview--point-overlay
              (make-overlay (nael-render-node-beg node)
                            (nael-render-node-end node)))
        (overlay-put nael-infoview--point-overlay
                     'face 'nael-infoview-point-node))
    (when nael-infoview--point-overlay
      (delete-overlay nael-infoview--point-overlay)
      (setq nael-infoview--point-overlay nil))))

;;;; Commands:

(defun nael-infoview--toggle-option (variable)
  "Toggle the user option VARIABLE and render the infoview again."
  (set variable (not (symbol-value variable)))
  (message "%s %s" variable (if (symbol-value variable) "enabled" "disabled"))
  (when-let* ((buffer (nael-infoview--buffer)))
    (with-current-buffer buffer
      (when nael-infoview--results
        (nael-infoview--render))
      (force-mode-line-update))))

(defun nael-infoview-toggle-instances ()
  "Toggle hiding instance hypotheses in the infoview."
  (interactive)
  (nael-infoview--toggle-option 'nael-infoview-hide-instances))

(defun nael-infoview-toggle-types ()
  "Toggle hiding hypotheses that are types in the infoview."
  (interactive)
  (nael-infoview--toggle-option 'nael-infoview-hide-types))

(defun nael-infoview-toggle-inaccessible ()
  "Toggle hiding inaccessible names, which contain \"✝\", in the infoview."
  (interactive)
  (nael-infoview--toggle-option 'nael-infoview-hide-inaccessible))

(defun nael-infoview-toggle-let-values ()
  "Toggle hiding the values of let-bound hypotheses in the infoview."
  (interactive)
  (nael-infoview--toggle-option 'nael-infoview-hide-let-values))

(defun nael-infoview-toggle-reverse ()
  "Toggle showing targets before hypotheses in the infoview."
  (interactive)
  (nael-infoview--toggle-option 'nael-infoview-reverse))

(defun nael-infoview-toggle-diffs ()
  "Toggle marking what the tactic at point changes in the infoview."
  (interactive)
  (nael-infoview--toggle-option 'nael-infoview-show-diffs))

(defun nael-infoview-set-expected-type (visibility)
  "Set how the infoview shows the expected type to VISIBILITY.
See `nael-infoview-expected-type'."
  (setq nael-infoview-expected-type visibility)
  (nael-infoview-refresh))

(easy-menu-define nael-infoview-mode-menu nael-infoview-mode-map
  "Menu for the Nael infoview."
  '("Infoview"
    ["Refresh" nael-infoview-refresh]
    "---"
    ["Hide instances" nael-infoview-toggle-instances
     :style toggle :selected nael-infoview-hide-instances]
    ["Hide types" nael-infoview-toggle-types
     :style toggle :selected nael-infoview-hide-types]
    ["Hide inaccessible names" nael-infoview-toggle-inaccessible
     :style toggle :selected nael-infoview-hide-inaccessible]
    ["Hide let values" nael-infoview-toggle-let-values
     :style toggle :selected nael-infoview-hide-let-values]
    ["Target first" nael-infoview-toggle-reverse
     :style toggle :selected nael-infoview-reverse]
    ["Show diffs" nael-infoview-toggle-diffs
     :style toggle :selected nael-infoview-show-diffs]
    ("Expected type"
     ["Expanded" (nael-infoview-set-expected-type 'expanded)
      :style radio :selected (eq nael-infoview-expected-type 'expanded)]
     ["Collapsed" (nael-infoview-set-expected-type 'collapsed)
      :style radio :selected (eq nael-infoview-expected-type 'collapsed)]
     ["Hidden" (nael-infoview-set-expected-type 'hidden)
      :style radio :selected (eq nael-infoview-expected-type 'hidden)])
    "---"
    ["Next goal" nael-infoview-next-goal]
    ["Previous goal" nael-infoview-previous-goal]
    ["Copy goal" nael-infoview-copy-goal]
    "---"
    ["Customize" (customize-group 'nael-infoview)]))

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

(defun nael-infoview-copy-goal ()
  "Copy the goal at point in the infoview as plain text."
  (interactive)
  (let ((start (or (nael-infoview--goal-start (point))
                   (user-error "No goal at point"))))
    (kill-new (string-trim-right
               (buffer-substring-no-properties
                start (next-single-property-change start 'nael-goal nil
                                                   (point-max)))))
    (message "Copied goal")))

(provide 'nael-infoview-buffer)

;;; nael-infoview-buffer.el ends here
