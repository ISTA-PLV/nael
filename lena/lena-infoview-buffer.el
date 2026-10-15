;;; lena-infoview-buffer.el --- The buffer of Lena's infoview  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see LICENSE.GPL3.

;;; Commentary:

;; The buffer in which `lena-infoview' shows goals and messages: its
;; major mode, rendering results into it while keeping folds and window
;; positions, its status, and the commands that act on its content.
;; When and what to show is decided by `lena-infoview'.

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'face-remap)
(require 'outline)
(require 'transient)

(require 'lena-infoview-hover)
(require 'lena-render)
(require 'lena-rpc)

(declare-function lena-infoview-refresh "lena-infoview" ())
(declare-function lena-eglot-restart-file "lena-eglot" ())

(defgroup lena-infoview nil
  "Infoview for `lena-mode'."
  :group 'lena-eglot
  :link '(emacs-library-link :tag "Source Lisp File" "lena-infoview.el")
  :prefix "lena-infoview-")

(defconst lena-infoview-buffer-name "*Lena Infoview*")

(defface lena-infoview-point-node
  '((t :inherit highlight))
  "Face of the subterm at point in the infoview."
  :group 'lena-infoview)

;;;; State:

(defcustom lena-infoview-hide-instances nil
  "Whether the infoview hides instance hypotheses, like `inst✝ : Ring R'.
Toggle it in the infoview with \\<lena-infoview-mode-map>\\[lena-infoview-toggle-instances]."
  :type 'boolean
  :group 'lena-infoview)

(defcustom lena-infoview-hide-types nil
  "Whether the infoview hides hypotheses that are types, like `α : Type'.
Toggle it in the infoview with \\<lena-infoview-mode-map>\\[lena-infoview-toggle-types]."
  :type 'boolean
  :group 'lena-infoview)

(defcustom lena-infoview-hide-inaccessible nil
  "Whether the infoview hides inaccessible names, which contain \"✝\".
Toggle it in the infoview with \\<lena-infoview-mode-map>\\[lena-infoview-toggle-inaccessible]."
  :type 'boolean
  :group 'lena-infoview)

(defcustom lena-infoview-hide-let-values nil
  "Whether the infoview hides the values of let-bound hypotheses.
Toggle it in the infoview with \\<lena-infoview-mode-map>\\[lena-infoview-toggle-let-values]."
  :type 'boolean
  :group 'lena-infoview)

(defcustom lena-infoview-reverse nil
  "Whether the infoview shows targets first, then hypotheses upwards.
Toggle it in the infoview with \\<lena-infoview-mode-map>\\[lena-infoview-toggle-reverse]."
  :type 'boolean
  :group 'lena-infoview)

(defcustom lena-infoview-show-diffs t
  "Whether the infoview marks what the tactic at point changes.
The marks use the faces `lena-render-diff-inserted' and
`lena-render-diff-removed'.  Toggle it in the infoview with
\\<lena-infoview-mode-map>\\[lena-infoview-toggle-diffs]."
  :type 'boolean
  :group 'lena-infoview)

(defcustom lena-infoview-expected-type 'expanded
  "How the infoview shows the expected type at point.
`expanded' shows it, `collapsed' shows its heading folded, and `hidden'
does not request it at all."
  :type '(choice (const :tag "Expanded" expanded)
                 (const :tag "Collapsed" collapsed)
                 (const :tag "Hidden" hidden))
  :group 'lena-infoview)

(defun lena-infoview--options ()
  "Return the view options for `lena-render-goals'."
  (list :hide-instances lena-infoview-hide-instances
        :hide-types lena-infoview-hide-types
        :hide-inaccessible lena-infoview-hide-inaccessible
        :hide-let-values lena-infoview-hide-let-values
        :reverse lena-infoview-reverse
        :hide-diffs (not lena-infoview-show-diffs)))

(defvar-local lena-infoview--owner nil
  "Owner of the references that the infoview shows.")

(defvar-local lena-infoview--status nil
  "Status of the infoview, shown in its mode line.
nil when up to date, or one of `updating', `processing',
`unsupported', `no-server', or (error . MESSAGE).")

(defvar-local lena-infoview--location nil
  "Description of the position whose information is shown.")

(defvar-local lena-infoview--results nil
  "The shown results, as (GOALS TERM-GOAL), for rendering them again.")

(defvar-local lena-infoview--messages nil
  "The shown messages, or nil.
A plist with the `:diagnostics' and the `:line' of `lena-render-goals',
and the source `:buffer' with its `buffer-chars-modified-tick' as
`:tick' at the time of the request.")

(defvar-local lena-infoview--trace-children nil
  "Hash table of the children of shown traces, fetched on request, by key.")

(defvar-local lena-infoview--expanded-traces nil
  "Keys of the traces whose children were fetched, newest first.
Where such traces are shown again after an update, their children are
fetched again.")

(defvar-local lena-infoview--fetching nil
  "Keys of the traces whose children are being fetched.")

(defvar-local lena-infoview--unfold nil
  "Keys of headings to unfold at the next rendering.")

(defvar-local lena-infoview--fold-defaults nil
  "Alist (KEY . FOLDED) of how the last rendering folded its headings.
A heading that is still as it was rendered keeps following its default,
so that a message unfolds once point is on its first line.  Once the
user folds or unfolds it, it stays so while it is shown.")

(defvar-local lena-infoview--accomplished nil
  "Line shown above the goals if their proof is complete, or nil.")

(defvar-local lena-infoview--point-overlay nil
  "Overlay that highlights the subterm at point.")

(defvar-local lena-infoview--dim-cookie nil
  "Face remapping that dims outdated content, or nil.")

(defvar lena-infoview--window nil
  "The window in which `lena-infoview' displayed the infoview, or nil.")

;;;; The buffer:

(defvar-keymap lena-infoview-mode-map
  :doc "Keymap of `lena-infoview-mode'."
  "n" #'lena-infoview-next-goal
  "p" #'lena-infoview-previous-goal
  "w" #'lena-infoview-copy-goal
  "i" #'lena-infoview-toggle-instances
  "t" #'lena-infoview-toggle-types
  "a" #'lena-infoview-toggle-inaccessible
  "l" #'lena-infoview-toggle-let-values
  "r" #'lena-infoview-toggle-reverse
  "d" #'lena-infoview-toggle-diffs
  "v" #'lena-infoview-view-options)

(defun lena-infoview--outline-level ()
  "Return the outline level of the heading at point."
  (or (get-text-property (point) 'outline-level) 1))

(define-derived-mode lena-infoview-mode special-mode "Lena Infoview"
  "Major mode of the Lena infoview, which shows goals and messages at point.

Headings of sections, goals, messages and traces can be folded by
clicking the triangle in the margin before them, or with \\[outline-cycle]
and \\[outline-cycle-buffer], as in `outline-minor-mode'.

In messages, positions, suggestions such as \"[apply]\", codes of errors
and the button to restart the file are buttons: \\<lena-infoview-button-map>\\[push-button] or a click
goes to the message in the source, applies the suggestion, shows the
explanation or restarts the file.  On a trace whose children are not
shown yet, \\<lena-infoview-trace-map>\\[push-button] fetches them.

On a subterm, ElDoc shows it with its type, and so does its tooltip
with `tooltip-mode'.  \\<lena-infoview-node-map>\\[lena-infoview-describe] or a click describes it in a help
buffer, and \\[xref-find-definitions] shows its definition in another window.

\\{lena-infoview-mode-map}"
  (setq-local revert-buffer-function
              (lambda (&rest _) (lena-infoview-refresh)))
  (setq-local mode-line-process '(:eval (lena-infoview--mode-line-process)))
  (setq-local outline-search-function #'outline-search-level
              outline-level #'lena-infoview--outline-level
              outline-minor-mode-cycle t
              ;; Buttons in the margin leave the text alone, so that
              ;; positions in the buffer are those of the renderer.
              outline-minor-mode-use-buttons 'in-margins
              ;; One column for the button and one to separate it from
              ;; the text.
              outline-margin-width 2)
  ;; Wrap long lines, as VS Code and lean.nvim do: Lean breaks them at
  ;; `format.width', but the window may be narrower.
  (setq-local truncate-lines nil
              word-wrap t)
  (outline-minor-mode)
  (lena-infoview-hover-setup)
  ;; Jumps from the infoview, as to a definition, show their target in
  ;; another window: the infoview's window, a side window, refuses other
  ;; buffers, but `switch-to-buffer' ignores that by default.
  (setq-local switch-to-buffer-obey-display-actions t)
  (add-hook 'post-command-hook #'lena-infoview--keep-window nil t)
  (add-hook 'post-command-hook #'lena-infoview--highlight-point nil t)
  (add-hook 'kill-buffer-hook #'lena-infoview--killed nil t))

(defun lena-infoview--buffer ()
  "Return the infoview buffer, or nil."
  (get-buffer lena-infoview-buffer-name))

(defun lena-infoview--get-buffer-create ()
  "Return the infoview buffer, creating it if needed."
  (or (lena-infoview--buffer)
      (with-current-buffer (get-buffer-create lena-infoview-buffer-name)
        (lena-infoview-mode)
        (current-buffer))))

(defun lena-infoview-visible-p ()
  "Return non-nil if the infoview is displayed in some window."
  (when-let* ((buffer (lena-infoview--buffer)))
    (get-buffer-window buffer t)))

(defun lena-infoview--mode-line-process ()
  "Return the location and status of the infoview for its mode line.
This is the value of `mode-line-process', which is where buffers of
processes such as compilations show their state."
  (concat
   (when lena-infoview--location
     (concat " " lena-infoview--location))
   (pcase lena-infoview--status
     ('nil nil)
     ('updating " updating")
     ('processing " processing")
     ('unsupported
      (propertize " needs Lean 4.30" 'help-echo
                  "Lean 4.30 or later is needed; C-h . shows the goals"))
     ('no-server " no server")
     (`(error . ,message)
      (propertize " error" 'face 'error 'help-echo message)))))

(defun lena-infoview--set-status (status)
  "Set the STATUS of the infoview and dim its content if outdated."
  (when-let* ((buffer (lena-infoview--buffer)))
    (with-current-buffer buffer
      (setq lena-infoview--status status)
      (let ((dim (memq status '(processing no-server unsupported))))
        (cond ((and dim (not lena-infoview--dim-cookie))
               (setq lena-infoview--dim-cookie
                     (face-remap-add-relative 'default 'shadow)))
              ((and (not dim) lena-infoview--dim-cookie)
               (face-remap-remove-relative lena-infoview--dim-cookie)
               (setq lena-infoview--dim-cookie nil))))
      (force-mode-line-update))))

(defun lena-infoview--killed ()
  "Release the references that the infoview shows."
  (when lena-infoview--owner
    (lena-rpc-owner-release lena-infoview--owner)
    (setq lena-infoview--owner nil)))

;;;; Windows:

(defun lena-infoview--keep-window ()
  "Show the infoview only in the window it was displayed in.
Commands that return to the infoview, like `xref-go-back' or Doom Emacs'
\\`C-o', switch to it in the selected window, which then shows it a
second time.  Show that window's previous buffer again, and select the
infoview's own window instead, at the same position."
  (let ((window (selected-window))
        (home lena-infoview--window))
    (when (and (window-live-p home)
               (not (eq window home))
               (eq (window-buffer home) (current-buffer))
               (eq (window-buffer window) (current-buffer)))
      (let ((pos (window-point window)))
        (switch-to-prev-buffer window)
        (select-window home)
        (goto-char pos)))))

(defun lena-infoview--window-anchor (window)
  "Return where WINDOW looks into the infoview, to restore it later.
The result is (START . POINT), each an anchor (GOAL LINE COLUMN): the
key of the goal at the position (see `lena-key' in `lena-render'), the
line relative to the goal's first line, or to the buffer if not in a
goal, and the column."
  (cl-flet ((anchor (pos)
              (save-excursion
                (goto-char pos)
                (let* ((goal (get-text-property pos 'lena-key))
                       (goal-start
                        (if goal
                            (or (previous-single-property-change
                                 (1+ pos) 'lena-key nil (point-min))
                                (point-min))
                          (point-min))))
                  (list goal
                        (count-lines goal-start (line-beginning-position))
                        (current-column))))))
    (cons (anchor (window-start window))
          (anchor (window-point window)))))

(defun lena-infoview--anchor-position (anchor)
  "Return the position of ANCHOR in the current buffer."
  (pcase-let ((`(,goal ,line ,column) anchor))
    (save-excursion
      (goto-char (point-min))
      (when goal
        (if-let* ((match (text-property-search-forward 'lena-key goal t)))
            (goto-char (prop-match-beginning match))
          (goto-char (point-min))))
      (forward-line line)
      (move-to-column column)
      (point))))

(defun lena-infoview--restore-window (window anchors)
  "Restore WINDOW to ANCHORS from `lena-infoview--window-anchor'."
  (set-window-start window (save-excursion
                             (goto-char (lena-infoview--anchor-position
                                         (car anchors)))
                             (line-beginning-position))
                    t)
  (set-window-point window (lena-infoview--anchor-position (cdr anchors))))

;;;; Showing results:

(defun lena-infoview--heading-key ()
  "Return a key for the heading at point that survives a refresh.
Goals are identified by their place, see `lena-key' in `lena-render',
so that folds survive moving to another tactic."
  (or (get-text-property (point) 'lena-section)
      (get-text-property (point) 'lena-key)))

(defun lena-infoview--heading-states ()
  "Return an alist (KEY . FOLDED) of the headings of the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (states)
      (unless (outline-on-heading-p) (outline-next-heading))
      (while (not (eobp))
        (push (cons (lena-infoview--heading-key)
                    (outline-invisible-p (line-end-position)))
              states)
        (outline-next-heading))
      (nreverse states))))

(defun lena-infoview--default-folded-p ()
  "Return non-nil if the heading at point should start folded.
Messages that start on another line than point and traces that the
server sends collapsed are marked by the renderer; the expected type is
folded as `lena-infoview-expected-type' says."
  (or (get-text-property (point) 'lena-folded)
      (and (eq (lena-infoview--heading-key) 'term-goal)
           (eq lena-infoview-expected-type 'collapsed))))

(defun lena-infoview--fold-as-before (states)
  "Fold the headings of the newly rendered buffer.
STATES are those of the buffer before, from
`lena-infoview--heading-states'.  A heading that the user folded or
unfolded stays so; others follow their default, and those in
`lena-infoview--unfold' unfold."
  (let (defaults folded)
    (save-excursion
      (goto-char (point-min))
      (unless (outline-on-heading-p) (outline-next-heading))
      (while (not (eobp))
        (let* ((key (lena-infoview--heading-key))
               (default (and (lena-infoview--default-folded-p) t))
               (before (assoc key states))
               (rendered (assoc key lena-infoview--fold-defaults)))
          (push (cons key default) defaults)
          (when (cond ((member key lena-infoview--unfold) nil)
                      ;; Changed by the user since it was rendered.
                      ((and before
                            (not (eq (and (cdr before) t)
                                     (and (cdr rendered) t))))
                       (cdr before))
                      (t default))
            (push key folded)))
        (outline-next-heading)))
    (lena-infoview--fold folded)
    (setq lena-infoview--fold-defaults defaults
          lena-infoview--unfold nil)))

(defun lena-infoview--fold (keys)
  "Fold the headings of the current buffer whose key is in KEYS."
  (when keys
    (save-excursion
      (goto-char (point-min))
      (unless (outline-on-heading-p) (outline-next-heading))
      (while (not (eobp))
        (when (member (lena-infoview--heading-key) keys)
          (outline-hide-subtree))
        (outline-next-heading)))))

(defun lena-infoview--render ()
  "Render the shown results again, keeping folds and window positions.
Each heading starts as `lena-infoview--default-folded-p' says, and
keeps following it until the user folds or unfolds it."
  (let* ((rendered (lena-render-goals
                    (nth 0 lena-infoview--results)
                    (nth 1 lena-infoview--results)
                    (append (list :accomplished lena-infoview--accomplished)
                            (lena-infoview--options))
                    (when lena-infoview--messages
                      (append (list :trace-children
                                    lena-infoview--trace-children)
                              lena-infoview--messages))))
         (inhibit-read-only t)
         (states (lena-infoview--heading-states))
         (windows (mapcar (lambda (window)
                            (cons window
                                  (lena-infoview--window-anchor window)))
                          (get-buffer-window-list nil nil t))))
    ;; Only our overlays, those of the subterms and the highlight.
    ;; Outline replaces its buttons itself, and others are other
    ;; packages' business.
    (remove-overlays nil nil 'lena-infoview-node t)
    (remove-overlays nil nil 'lena-infoview-control t)
    (when lena-infoview--point-overlay
      (delete-overlay lena-infoview--point-overlay))
    (erase-buffer)
    ;; Outline adds its buttons from `after-change-functions'.
    (if (string-empty-p (lena-render-result-text rendered))
        (insert (propertize "No goals or expected type at point.\n"
                            'face 'shadow))
      (insert (lena-render-result-text rendered)))
    (setq lena-infoview--point-overlay nil)
    (lena-infoview--decorate (lena-render-result-nodes rendered) 0)
    (lena-infoview--decorate-controls)
    (lena-infoview--fold-as-before states)
    (pcase-dolist (`(,window . ,anchors) windows)
      (lena-infoview--restore-window window anchors))
    (lena-infoview--highlight-point)
    (lena-infoview--fetch-expanded-traces)))

(defun lena-infoview--show (goals term-goal owner location
                                   &optional handle accomplished messages)
  "Show GOALS, TERM-GOAL and MESSAGES, whose references OWNER keeps.
LOCATION describes where they were requested.  HANDLE is the RPC handle
through which they came, for asking about their subterms.  ACCOMPLISHED
is the line to show above them if their proof is complete.  MESSAGES
is as `lena-infoview--messages'."
  (with-current-buffer (lena-infoview--get-buffer-create)
    (let ((previous lena-infoview--owner))
      (setq lena-infoview--results (list goals term-goal)
            lena-infoview--accomplished accomplished
            lena-infoview--messages messages
            lena-infoview--trace-children (make-hash-table :test #'equal)
            lena-infoview--fetching nil
            lena-infoview--owner owner
            lena-infoview--location location)
      (lena-infoview-hover-reset handle)
      (lena-infoview--render)
      ;; Only now is nothing shown any longer that uses the old
      ;; references.
      (when previous (lena-rpc-owner-release previous))))
  (lena-infoview--set-status nil))

(defun lena-infoview--highlight-point ()
  "Highlight the subterm at point in the infoview."
  (if-let* ((node (lena-render-node-at (point))))
      (if lena-infoview--point-overlay
          (move-overlay lena-infoview--point-overlay
                        (lena-render-node-beg node)
                        (lena-render-node-end node))
        (setq lena-infoview--point-overlay
              (make-overlay (lena-render-node-beg node)
                            (lena-render-node-end node)))
        (overlay-put lena-infoview--point-overlay
                     'face 'lena-infoview-point-node))
    (when lena-infoview--point-overlay
      (delete-overlay lena-infoview--point-overlay)
      (setq lena-infoview--point-overlay nil))))

;;;; Messages:

(defvar-keymap lena-infoview-button-map
  :doc "Keymap of the buttons in the infoview.
Like `button-map', but without its \\`TAB', which moves between buttons:
in the infoview, \\`TAB' folds headings."
  "RET" #'push-button
  "<mouse-2>" #'push-button
  "<follow-link>" 'mouse-face)

(defvar-keymap lena-infoview-trace-map
  :doc "Keymap of the heading of a trace whose children are not shown yet."
  :parent lena-infoview-button-map
  "TAB" #'push-button
  "<tab>" #'push-button)

(define-button-type 'lena-infoview-message
  'action (lambda (button) (lena-infoview-goto-message (button-start button)))
  'help-echo "RET, mouse-1: go to the message"
  'keymap lena-infoview-button-map 'face nil 'follow-link t)

(define-button-type 'lena-infoview-suggestion
  'action (lambda (button)
            (lena-infoview-apply-suggestion (button-start button)))
  'keymap lena-infoview-button-map 'face nil 'follow-link t)

(define-button-type 'lena-infoview-explanation
  'action (lambda (button)
            (lena-infoview-browse-explanation (button-start button)))
  'help-echo "RET, mouse-1: read the explanation of this error"
  'keymap lena-infoview-button-map 'face nil 'follow-link t)

(define-button-type 'lena-infoview-restart
  'action (lambda (_) (lena-infoview-restart-file))
  'help-echo "RET, mouse-1: restart the file, which rebuilds its imports"
  'keymap lena-infoview-button-map 'face nil 'follow-link t)

(define-button-type 'lena-infoview-trace
  'action (lambda (button) (lena-infoview-expand-trace (button-start button)))
  'help-echo "TAB, mouse-1: show the trace"
  'keymap lena-infoview-trace-map 'face nil 'follow-link t)

(defun lena-infoview--decorate-controls ()
  "Make buttons of what the renderer marked as actions in messages.
Overlay buttons, so that the text keeps the renderer's faces."
  (pcase-dolist (`(,property . ,type)
                 '((lena-message-range . lena-infoview-message)
                   (lena-suggestion . lena-infoview-suggestion)
                   (lena-explanation . lena-infoview-explanation)
                   (lena-restart-file . lena-infoview-restart)
                   (lena-trace-lazy . lena-infoview-trace)))
    (save-excursion
      (goto-char (point-min))
      (let (match)
        (while (setq match (text-property-search-forward property))
          (let ((button (make-button (prop-match-beginning match)
                                     (prop-match-end match)
                                     'type type 'lena-infoview-control t)))
            (when (eq property 'lena-suggestion)
              (button-put button 'help-echo
                          (plist-get (prop-match-value match) :help)))))))))

(defun lena-infoview--source-buffer ()
  "Return the live source buffer of the shown messages, or signal."
  (let ((buffer (plist-get lena-infoview--messages :buffer)))
    (unless (buffer-live-p buffer)
      (user-error "The buffer of these messages is gone"))
    buffer))

(defun lena-infoview--show-source (buffer pos)
  "Select a window that shows BUFFER and go to POS there."
  (pop-to-buffer buffer)
  (goto-char pos))

(defun lena-infoview-goto-message (&optional pos)
  "Go to the source of the message whose position is at POS or point."
  (interactive)
  (let* ((pos (or pos (point)))
         (range (or (get-text-property pos 'lena-message-range)
                    (user-error "No message here")))
         (buffer (lena-infoview--source-buffer)))
    (lena-infoview--show-source
     buffer (with-current-buffer buffer (car (eglot-range-region range))))))

(defun lena-infoview-apply-suggestion (&optional pos)
  "Apply the suggestion at POS or point, such as Lean's \"Try this\".
Replace its range in the source with it, if the source has not changed
since the suggestion was made, and go there."
  (interactive)
  (let* ((pos (or pos (point)))
         (suggestion (or (get-text-property pos 'lena-suggestion)
                         (user-error "No suggestion here")))
         (buffer (lena-infoview--source-buffer))
         (tick (plist-get lena-infoview--messages :tick)))
    (with-current-buffer buffer
      (unless (eql (buffer-chars-modified-tick) tick)
        (user-error "The file has changed since; update the infoview first"))
      (pcase-let ((`(,beg . ,end)
                   (eglot-range-region (plist-get suggestion :range))))
        (save-excursion
          (goto-char beg)
          (delete-region beg end)
          (insert (plist-get suggestion :suggestion))
          (setq pos (point)))))
    (lena-infoview--show-source buffer pos)))

(defun lena-infoview-browse-explanation (&optional pos)
  "Show the explanation of the error whose code is at POS or point.
The explanations are in Lean's reference manual, on the web."
  (interactive)
  (browse-url (or (get-text-property (or pos (point)) 'lena-explanation)
                  (user-error "No explanation here"))))

(defun lena-infoview-restart-file ()
  "Restart the file of the shown messages, which rebuilds its imports.
For the button in a message that the imports are out of date.  See
`lena-eglot-restart-file'."
  (interactive)
  (with-current-buffer (lena-infoview--source-buffer)
    (lena-eglot-restart-file)))

(defun lena-infoview--fetch-trace (key children &optional interactive)
  "Fetch CHILDREN, the reference of the trace with KEY, and show them.
If INTERACTIVE is non-nil, report failures."
  (let ((owner lena-infoview--owner)
        (buffer (current-buffer)))
    (cond
     ((lena-rpc-handle-live-p lena-infoview--handle)
      (push key lena-infoview--fetching)
      (lena-infoview--request-trace key children owner buffer interactive))
     (interactive
      (user-error "Lean no longer knows this trace; update the infoview")))))

(defun lena-infoview--request-trace (key children owner buffer interactive)
  "Request CHILDREN of the trace with KEY for OWNER, shown in BUFFER.
See `lena-infoview--fetch-trace' for INTERACTIVE."
  (lena-rpc-lazy-trace-children
   lena-infoview--handle children
   (lambda (result)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (setq lena-infoview--fetching (delete key lena-infoview--fetching))
         ;; Only for the rendering that asked.
         (when (eq owner lena-infoview--owner)
           (puthash key result lena-infoview--trace-children)
           (push key lena-infoview--unfold)
           (let ((inhibit-read-only t))
             (lena-infoview--render))))))
   :owner owner
   :failure
   (lambda (error)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (setq lena-infoview--fetching
               (delete key lena-infoview--fetching))))
     (when interactive
       (message "Lean could not show the trace: %s"
                (plist-get error :message))))))

(defun lena-infoview-expand-trace (&optional pos)
  "Show the children of the trace whose heading is at POS or point.
The server sends them on request only."
  (interactive)
  (let* ((pos (or pos (point)))
         (children (or (get-text-property pos 'lena-trace-lazy)
                       (user-error "No trace to show here")))
         (key (get-text-property pos 'lena-key)))
    (unless (member key lena-infoview--fetching)
      (setq lena-infoview--expanded-traces
            (seq-take (cons key (delete key lena-infoview--expanded-traces))
                      100))
      (lena-infoview--fetch-trace key children t))))

(defun lena-infoview--fetch-expanded-traces ()
  "Fetch the children of shown traces that were expanded before.
After an update, the references of the earlier children are gone."
  (save-excursion
    (goto-char (point-min))
    (let (match)
      (while (setq match (text-property-search-forward 'lena-trace-lazy))
        (let ((key (get-text-property (prop-match-beginning match)
                                      'lena-key)))
          (when (and (member key lena-infoview--expanded-traces)
                     (not (member key lena-infoview--fetching)))
            (lena-infoview--fetch-trace key (prop-match-value match))))))))

;;;; Commands:

(defun lena-infoview--toggle-option (variable)
  "Toggle the user option VARIABLE and render the infoview again."
  (set variable (not (symbol-value variable)))
  (message "%s %s" variable (if (symbol-value variable) "enabled" "disabled"))
  (when-let* ((buffer (lena-infoview--buffer)))
    (with-current-buffer buffer
      (when lena-infoview--results
        (lena-infoview--render))
      (force-mode-line-update))))

(defun lena-infoview-toggle-instances ()
  "Toggle hiding instance hypotheses in the infoview."
  (interactive)
  (lena-infoview--toggle-option 'lena-infoview-hide-instances))

(defun lena-infoview-toggle-types ()
  "Toggle hiding hypotheses that are types in the infoview."
  (interactive)
  (lena-infoview--toggle-option 'lena-infoview-hide-types))

(defun lena-infoview-toggle-inaccessible ()
  "Toggle hiding inaccessible names, which contain \"✝\", in the infoview."
  (interactive)
  (lena-infoview--toggle-option 'lena-infoview-hide-inaccessible))

(defun lena-infoview-toggle-let-values ()
  "Toggle hiding the values of let-bound hypotheses in the infoview."
  (interactive)
  (lena-infoview--toggle-option 'lena-infoview-hide-let-values))

(defun lena-infoview-toggle-reverse ()
  "Toggle showing targets before hypotheses in the infoview."
  (interactive)
  (lena-infoview--toggle-option 'lena-infoview-reverse))

(defun lena-infoview-toggle-diffs ()
  "Toggle marking what the tactic at point changes in the infoview."
  (interactive)
  (lena-infoview--toggle-option 'lena-infoview-show-diffs))

(defun lena-infoview-cycle-expected-type ()
  "Show the expected type expanded, collapsed, or not at all, in turn.
See `lena-infoview-expected-type'."
  (interactive)
  (setq lena-infoview-expected-type
        (pcase lena-infoview-expected-type
          ('expanded 'collapsed)
          ('collapsed 'hidden)
          (_ 'expanded)))
  (lena-infoview-refresh)
  (message "Expected type %s" lena-infoview-expected-type))

(defun lena-infoview--switch-description (label on)
  "Describe the view option LABEL, which is ON or not, for a menu."
  (concat label " "
          (if on
              (propertize "on" 'face 'transient-value)
            (propertize "off" 'face 'transient-inactive-value))))

(defun lena-infoview--expected-type-description ()
  "Describe how the expected type is shown, for a menu."
  (concat "Expected type "
          (propertize "[" 'face 'transient-delimiter)
          (mapconcat (lambda (value)
                       (propertize (symbol-name value) 'face
                                   (if (eq value lena-infoview-expected-type)
                                       'transient-value
                                     'transient-inactive-value)))
                     '(expanded collapsed hidden)
                     (propertize "|" 'face 'transient-delimiter))
          (propertize "]" 'face 'transient-delimiter)))

(transient-define-prefix lena-infoview-view-options ()
  "Choose what the infoview shows.
The menu stays open for further choices; \\`C-g' closes it."
  [["Hypotheses"
    ("i" lena-infoview-toggle-instances :transient t
     :description (lambda ()
                    (lena-infoview--switch-description
                     "Hide instances" lena-infoview-hide-instances)))
    ("t" lena-infoview-toggle-types :transient t
     :description (lambda ()
                    (lena-infoview--switch-description
                     "Hide types" lena-infoview-hide-types)))
    ("a" lena-infoview-toggle-inaccessible :transient t
     :description (lambda ()
                    (lena-infoview--switch-description
                     "Hide inaccessible names"
                     lena-infoview-hide-inaccessible)))
    ("l" lena-infoview-toggle-let-values :transient t
     :description (lambda ()
                    (lena-infoview--switch-description
                     "Hide let values" lena-infoview-hide-let-values)))]
   ["Goals"
    ("r" lena-infoview-toggle-reverse :transient t
     :description (lambda ()
                    (lena-infoview--switch-description
                     "Target first" lena-infoview-reverse)))
    ("d" lena-infoview-toggle-diffs :transient t
     :description (lambda ()
                    (lena-infoview--switch-description
                     "Show diffs" lena-infoview-show-diffs)))
    ("e" lena-infoview-cycle-expected-type :transient t
     :description lena-infoview--expected-type-description)]])

(defun lena-infoview--goal-start (pos)
  "Return the start of the goal at POS, or nil if POS is in no goal."
  (when (get-text-property pos 'lena-goal)
    (if (and (> pos (point-min))
             (equal (get-text-property (1- pos) 'lena-goal)
                    (get-text-property pos 'lena-goal)))
        (previous-single-property-change pos 'lena-goal nil (point-min))
      pos)))

(eldoc-add-command 'lena-infoview-next-goal 'lena-infoview-previous-goal)

;; ElDoc shows the subterm at point only after commands that move point.
;; These also land in the infoview: back from a description, and back
;; from a definition, as Emacs, Evil and Doom Emacs do it.  ElDoc's list
;; of commands is global, so ElDoc also runs after them elsewhere, which
;; does no harm.
(eldoc-add-command 'quit-window 'xref-go-back 'evil-jump-backward
                   '+popup/quit-window 'better-jumper-jump-backward)

(defun lena-infoview-next-goal (&optional n)
  "Move to the start of the Nth next goal in the infoview."
  (interactive "p")
  (dotimes (_ (or n 1))
    (let ((pos (point)))
      (when (get-text-property pos 'lena-goal)
        (setq pos (next-single-property-change pos 'lena-goal nil
                                               (point-max))))
      (unless (get-text-property pos 'lena-goal)
        (setq pos (next-single-property-change pos 'lena-goal)))
      (if pos (goto-char pos) (user-error "No next goal")))))

(defun lena-infoview-previous-goal (&optional n)
  "Move to the start of the Nth previous goal in the infoview."
  (interactive "p")
  (dotimes (_ (or n 1))
    (let* ((start (lena-infoview--goal-start (point)))
           (pos (if (and start (< start (point))) start
                  (let ((before (previous-single-property-change
                                 (or start (point)) 'lena-goal)))
                    (and before
                         (lena-infoview--goal-start
                          (if (get-text-property before 'lena-goal)
                              before
                            (max (point-min) (1- before)))))))))
      (if pos (goto-char pos) (user-error "No previous goal")))))

(defun lena-infoview-copy-goal ()
  "Copy the goal at point in the infoview as plain text."
  (interactive)
  (let ((start (or (lena-infoview--goal-start (point))
                   (user-error "No goal at point"))))
    (kill-new (string-trim-right
               (buffer-substring-no-properties
                start (next-single-property-change start 'lena-goal nil
                                                   (point-max)))))
    (message "Copied goal")))

(provide 'lena-infoview-buffer)

;;; lena-infoview-buffer.el ends here
