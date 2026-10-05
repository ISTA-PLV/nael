;;; nael-infoview-buffer.el --- The buffer of Nael's infoview  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see LICENSE.GPL3.

;;; Commentary:

;; The buffer in which `nael-infoview' shows goals and messages: its
;; major mode, rendering results into it while keeping folds and window
;; positions, its status, and the commands that act on its content.
;; When and what to show is decided by `nael-infoview'.

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'face-remap)
(require 'outline)
(require 'transient)

(require 'nael-infoview-hover)
(require 'nael-render)
(require 'nael-rpc)

(declare-function nael-infoview-refresh "nael-infoview" ())
(declare-function nael-eglot-restart-file "nael-eglot" ())

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

(defvar-local nael-infoview--messages nil
  "The shown messages, or nil.
A plist with the `:diagnostics' and the `:line' of `nael-render-goals',
and the source `:buffer' with its `buffer-chars-modified-tick' as
`:tick' at the time of the request.")

(defvar-local nael-infoview--trace-children nil
  "Hash table of the children of shown traces, fetched on request, by key.")

(defvar-local nael-infoview--expanded-traces nil
  "Keys of the traces whose children were fetched, newest first.
Where such traces are shown again after an update, their children are
fetched again.")

(defvar-local nael-infoview--fetching nil
  "Keys of the traces whose children are being fetched.")

(defvar-local nael-infoview--unfold nil
  "Keys of headings to unfold at the next rendering.")

(defvar-local nael-infoview--fold-defaults nil
  "Alist (KEY . FOLDED) of how the last rendering folded its headings.
A heading that is still as it was rendered keeps following its default,
so that a message unfolds once point is on its first line.  Once the
user folds or unfolds it, it stays so while it is shown.")

(defvar-local nael-infoview--accomplished nil
  "Line shown above the goals if their proof is complete, or nil.")

(defvar-local nael-infoview--point-overlay nil
  "Overlay that highlights the subterm at point.")

(defvar-local nael-infoview--dim-cookie nil
  "Face remapping that dims outdated content, or nil.")

(defvar nael-infoview--window nil
  "The window in which `nael-infoview' displayed the infoview, or nil.")

;;;; The buffer:

(defvar-keymap nael-infoview-mode-map
  :doc "Keymap of `nael-infoview-mode'."
  "n" #'nael-infoview-next-goal
  "p" #'nael-infoview-previous-goal
  "w" #'nael-infoview-copy-goal
  "i" #'nael-infoview-toggle-instances
  "t" #'nael-infoview-toggle-types
  "a" #'nael-infoview-toggle-inaccessible
  "l" #'nael-infoview-toggle-let-values
  "r" #'nael-infoview-toggle-reverse
  "d" #'nael-infoview-toggle-diffs
  "v" #'nael-infoview-view-options)

(defun nael-infoview--outline-level ()
  "Return the outline level of the heading at point."
  (or (get-text-property (point) 'outline-level) 1))

(define-derived-mode nael-infoview-mode special-mode "Nael Infoview"
  "Major mode of the Nael infoview, which shows goals and messages at point.

Headings of sections, goals, messages and traces can be folded by
clicking the triangle in the margin before them, or with \\[outline-cycle]
and \\[outline-cycle-buffer], as in `outline-minor-mode'.

In messages, positions, suggestions such as \"[apply]\", codes of errors
and the button to restart the file are buttons: \\<nael-infoview-button-map>\\[push-button] or a click
goes to the message in the source, applies the suggestion, shows the
explanation or restarts the file.  On a trace whose children are not
shown yet, \\<nael-infoview-trace-map>\\[push-button] fetches them.

On a subterm, ElDoc shows it with its type, and so does its tooltip
with `tooltip-mode'.  \\<nael-infoview-node-map>\\[nael-infoview-describe] or a click describes it in a help
buffer, and \\[xref-find-definitions] shows its definition in another window.

\\{nael-infoview-mode-map}"
  (setq-local revert-buffer-function
              (lambda (&rest _) (nael-infoview-refresh)))
  (setq-local mode-line-process '(:eval (nael-infoview--mode-line-process)))
  (setq-local outline-search-function #'outline-search-level
              outline-level #'nael-infoview--outline-level
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
  (nael-infoview-hover-setup)
  ;; Jumps from the infoview, as to a definition, show their target in
  ;; another window: the infoview's window, a side window, refuses other
  ;; buffers, but `switch-to-buffer' ignores that by default.
  (setq-local switch-to-buffer-obey-display-actions t)
  (add-hook 'post-command-hook #'nael-infoview--keep-window nil t)
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

(defun nael-infoview--keep-window ()
  "Show the infoview only in the window it was displayed in.
Commands that return to the infoview, like `xref-go-back' or Doom Emacs'
\\`C-o', switch to it in the selected window, which then shows it a
second time.  Show that window's previous buffer again, and select the
infoview's own window instead, at the same position."
  (let ((window (selected-window))
        (home nael-infoview--window))
    (when (and (window-live-p home)
               (not (eq window home))
               (eq (window-buffer home) (current-buffer))
               (eq (window-buffer window) (current-buffer)))
      (let ((pos (window-point window)))
        (switch-to-prev-buffer window)
        (select-window home)
        (goto-char pos)))))

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

(defun nael-infoview--heading-key ()
  "Return a key for the heading at point that survives a refresh.
Goals are identified by their place, see `nael-key' in `nael-render',
so that folds survive moving to another tactic."
  (or (get-text-property (point) 'nael-section)
      (get-text-property (point) 'nael-key)))

(defun nael-infoview--heading-states ()
  "Return an alist (KEY . FOLDED) of the headings of the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (states)
      (unless (outline-on-heading-p) (outline-next-heading))
      (while (not (eobp))
        (push (cons (nael-infoview--heading-key)
                    (outline-invisible-p (line-end-position)))
              states)
        (outline-next-heading))
      (nreverse states))))

(defun nael-infoview--default-folded-p ()
  "Return non-nil if the heading at point should start folded.
Messages that start on another line than point and traces that the
server sends collapsed are marked by the renderer; the expected type is
folded as `nael-infoview-expected-type' says."
  (or (get-text-property (point) 'nael-folded)
      (and (eq (nael-infoview--heading-key) 'term-goal)
           (eq nael-infoview-expected-type 'collapsed))))

(defun nael-infoview--fold-as-before (states)
  "Fold the headings of the newly rendered buffer.
STATES are those of the buffer before, from
`nael-infoview--heading-states'.  A heading that the user folded or
unfolded stays so; others follow their default, and those in
`nael-infoview--unfold' unfold."
  (let (defaults folded)
    (save-excursion
      (goto-char (point-min))
      (unless (outline-on-heading-p) (outline-next-heading))
      (while (not (eobp))
        (let* ((key (nael-infoview--heading-key))
               (default (and (nael-infoview--default-folded-p) t))
               (before (assoc key states))
               (rendered (assoc key nael-infoview--fold-defaults)))
          (push (cons key default) defaults)
          (when (cond ((member key nael-infoview--unfold) nil)
                      ;; Changed by the user since it was rendered.
                      ((and before
                            (not (eq (and (cdr before) t)
                                     (and (cdr rendered) t))))
                       (cdr before))
                      (t default))
            (push key folded)))
        (outline-next-heading)))
    (nael-infoview--fold folded)
    (setq nael-infoview--fold-defaults defaults
          nael-infoview--unfold nil)))

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
  "Render the shown results again, keeping folds and window positions.
Each heading starts as `nael-infoview--default-folded-p' says, and
keeps following it until the user folds or unfolds it."
  (let* ((rendered (nael-render-goals
                    (nth 0 nael-infoview--results)
                    (nth 1 nael-infoview--results)
                    (append (list :accomplished nael-infoview--accomplished)
                            (nael-infoview--options))
                    (when nael-infoview--messages
                      (append (list :trace-children
                                    nael-infoview--trace-children)
                              nael-infoview--messages))))
         (inhibit-read-only t)
         (states (nael-infoview--heading-states))
         (windows (mapcar (lambda (window)
                            (cons window
                                  (nael-infoview--window-anchor window)))
                          (get-buffer-window-list nil nil t))))
    ;; Only our overlays, those of the subterms and the highlight.
    ;; Outline replaces its buttons itself, and others are other
    ;; packages' business.
    (remove-overlays nil nil 'nael-infoview-node t)
    (remove-overlays nil nil 'nael-infoview-control t)
    (when nael-infoview--point-overlay
      (delete-overlay nael-infoview--point-overlay))
    (erase-buffer)
    ;; Outline adds its buttons from `after-change-functions'.
    (if (string-empty-p (nael-render-result-text rendered))
        (insert (propertize "No goals or expected type at point.\n"
                            'face 'shadow))
      (insert (nael-render-result-text rendered)))
    (setq nael-infoview--point-overlay nil)
    (nael-infoview--decorate (nael-render-result-nodes rendered) 0)
    (nael-infoview--decorate-controls)
    (nael-infoview--fold-as-before states)
    (pcase-dolist (`(,window . ,anchors) windows)
      (nael-infoview--restore-window window anchors))
    (nael-infoview--highlight-point)
    (nael-infoview--fetch-expanded-traces)))

(defun nael-infoview--show (goals term-goal owner location
                                   &optional handle accomplished messages)
  "Show GOALS, TERM-GOAL and MESSAGES, whose references OWNER keeps.
LOCATION describes where they were requested.  HANDLE is the RPC handle
through which they came, for asking about their subterms.  ACCOMPLISHED
is the line to show above them if their proof is complete.  MESSAGES
is as `nael-infoview--messages'."
  (with-current-buffer (nael-infoview--get-buffer-create)
    (let ((previous nael-infoview--owner))
      (setq nael-infoview--results (list goals term-goal)
            nael-infoview--accomplished accomplished
            nael-infoview--messages messages
            nael-infoview--trace-children (make-hash-table :test #'equal)
            nael-infoview--fetching nil
            nael-infoview--owner owner
            nael-infoview--location location)
      (nael-infoview-hover-reset handle)
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

;;;; Messages:

(defvar-keymap nael-infoview-button-map
  :doc "Keymap of the buttons in the infoview.
Like `button-map', but without its \\`TAB', which moves between buttons:
in the infoview, \\`TAB' folds headings."
  "RET" #'push-button
  "<mouse-2>" #'push-button
  "<follow-link>" 'mouse-face)

(defvar-keymap nael-infoview-trace-map
  :doc "Keymap of the heading of a trace whose children are not shown yet."
  :parent nael-infoview-button-map
  "TAB" #'push-button
  "<tab>" #'push-button)

(define-button-type 'nael-infoview-message
  'action (lambda (button) (nael-infoview-goto-message (button-start button)))
  'help-echo "RET, mouse-1: go to the message"
  'keymap nael-infoview-button-map 'face nil 'follow-link t)

(define-button-type 'nael-infoview-suggestion
  'action (lambda (button)
            (nael-infoview-apply-suggestion (button-start button)))
  'keymap nael-infoview-button-map 'face nil 'follow-link t)

(define-button-type 'nael-infoview-explanation
  'action (lambda (button)
            (nael-infoview-browse-explanation (button-start button)))
  'help-echo "RET, mouse-1: read the explanation of this error"
  'keymap nael-infoview-button-map 'face nil 'follow-link t)

(define-button-type 'nael-infoview-restart
  'action (lambda (_) (nael-infoview-restart-file))
  'help-echo "RET, mouse-1: restart the file, which rebuilds its imports"
  'keymap nael-infoview-button-map 'face nil 'follow-link t)

(define-button-type 'nael-infoview-trace
  'action (lambda (button) (nael-infoview-expand-trace (button-start button)))
  'help-echo "TAB, mouse-1: show the trace"
  'keymap nael-infoview-trace-map 'face nil 'follow-link t)

(defun nael-infoview--decorate-controls ()
  "Make buttons of what the renderer marked as actions in messages.
Overlay buttons, so that the text keeps the renderer's faces."
  (pcase-dolist (`(,property . ,type)
                 '((nael-message-range . nael-infoview-message)
                   (nael-suggestion . nael-infoview-suggestion)
                   (nael-explanation . nael-infoview-explanation)
                   (nael-restart-file . nael-infoview-restart)
                   (nael-trace-lazy . nael-infoview-trace)))
    (save-excursion
      (goto-char (point-min))
      (let (match)
        (while (setq match (text-property-search-forward property))
          (let ((button (make-button (prop-match-beginning match)
                                     (prop-match-end match)
                                     'type type 'nael-infoview-control t)))
            (when (eq property 'nael-suggestion)
              (button-put button 'help-echo
                          (plist-get (prop-match-value match) :help)))))))))

(defun nael-infoview--source-buffer ()
  "Return the live source buffer of the shown messages, or signal."
  (let ((buffer (plist-get nael-infoview--messages :buffer)))
    (unless (buffer-live-p buffer)
      (user-error "The buffer of these messages is gone"))
    buffer))

(defun nael-infoview--show-source (buffer pos)
  "Select a window that shows BUFFER and go to POS there."
  (pop-to-buffer buffer)
  (goto-char pos))

(defun nael-infoview-goto-message (&optional pos)
  "Go to the source of the message whose position is at POS or point."
  (interactive)
  (let* ((pos (or pos (point)))
         (range (or (get-text-property pos 'nael-message-range)
                    (user-error "No message here")))
         (buffer (nael-infoview--source-buffer)))
    (nael-infoview--show-source
     buffer (with-current-buffer buffer (car (eglot-range-region range))))))

(defun nael-infoview-apply-suggestion (&optional pos)
  "Apply the suggestion at POS or point, such as Lean's \"Try this\".
Replace its range in the source with it, if the source has not changed
since the suggestion was made, and go there."
  (interactive)
  (let* ((pos (or pos (point)))
         (suggestion (or (get-text-property pos 'nael-suggestion)
                         (user-error "No suggestion here")))
         (buffer (nael-infoview--source-buffer))
         (tick (plist-get nael-infoview--messages :tick)))
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
    (nael-infoview--show-source buffer pos)))

(defun nael-infoview-browse-explanation (&optional pos)
  "Show the explanation of the error whose code is at POS or point.
The explanations are in Lean's reference manual, on the web."
  (interactive)
  (browse-url (or (get-text-property (or pos (point)) 'nael-explanation)
                  (user-error "No explanation here"))))

(defun nael-infoview-restart-file ()
  "Restart the file of the shown messages, which rebuilds its imports.
For the button in a message that the imports are out of date.  See
`nael-eglot-restart-file'."
  (interactive)
  (with-current-buffer (nael-infoview--source-buffer)
    (nael-eglot-restart-file)))

(defun nael-infoview--fetch-trace (key children &optional interactive)
  "Fetch CHILDREN, the reference of the trace with KEY, and show them.
If INTERACTIVE is non-nil, report failures."
  (let ((owner nael-infoview--owner)
        (buffer (current-buffer)))
    (cond
     ((nael-rpc-handle-live-p nael-infoview--handle)
      (push key nael-infoview--fetching)
      (nael-infoview--request-trace key children owner buffer interactive))
     (interactive
      (user-error "Lean no longer knows this trace; update the infoview")))))

(defun nael-infoview--request-trace (key children owner buffer interactive)
  "Request CHILDREN of the trace with KEY for OWNER, shown in BUFFER.
See `nael-infoview--fetch-trace' for INTERACTIVE."
  (nael-rpc-lazy-trace-children
   nael-infoview--handle children
   (lambda (result)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (setq nael-infoview--fetching (delete key nael-infoview--fetching))
         ;; Only for the rendering that asked.
         (when (eq owner nael-infoview--owner)
           (puthash key result nael-infoview--trace-children)
           (push key nael-infoview--unfold)
           (let ((inhibit-read-only t))
             (nael-infoview--render))))))
   :owner owner
   :failure
   (lambda (error)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (setq nael-infoview--fetching
               (delete key nael-infoview--fetching))))
     (when interactive
       (message "Lean could not show the trace: %s"
                (plist-get error :message))))))

(defun nael-infoview-expand-trace (&optional pos)
  "Show the children of the trace whose heading is at POS or point.
The server sends them on request only."
  (interactive)
  (let* ((pos (or pos (point)))
         (children (or (get-text-property pos 'nael-trace-lazy)
                       (user-error "No trace to show here")))
         (key (get-text-property pos 'nael-key)))
    (unless (member key nael-infoview--fetching)
      (setq nael-infoview--expanded-traces
            (seq-take (cons key (delete key nael-infoview--expanded-traces))
                      100))
      (nael-infoview--fetch-trace key children t))))

(defun nael-infoview--fetch-expanded-traces ()
  "Fetch the children of shown traces that were expanded before.
After an update, the references of the earlier children are gone."
  (save-excursion
    (goto-char (point-min))
    (let (match)
      (while (setq match (text-property-search-forward 'nael-trace-lazy))
        (let ((key (get-text-property (prop-match-beginning match)
                                      'nael-key)))
          (when (and (member key nael-infoview--expanded-traces)
                     (not (member key nael-infoview--fetching)))
            (nael-infoview--fetch-trace key (prop-match-value match))))))))

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

(defun nael-infoview-cycle-expected-type ()
  "Show the expected type expanded, collapsed, or not at all, in turn.
See `nael-infoview-expected-type'."
  (interactive)
  (setq nael-infoview-expected-type
        (pcase nael-infoview-expected-type
          ('expanded 'collapsed)
          ('collapsed 'hidden)
          (_ 'expanded)))
  (nael-infoview-refresh)
  (message "Expected type %s" nael-infoview-expected-type))

(defun nael-infoview--switch-description (label on)
  "Describe the view option LABEL, which is ON or not, for a menu."
  (concat label " "
          (if on
              (propertize "on" 'face 'transient-value)
            (propertize "off" 'face 'transient-inactive-value))))

(defun nael-infoview--expected-type-description ()
  "Describe how the expected type is shown, for a menu."
  (concat "Expected type "
          (propertize "[" 'face 'transient-delimiter)
          (mapconcat (lambda (value)
                       (propertize (symbol-name value) 'face
                                   (if (eq value nael-infoview-expected-type)
                                       'transient-value
                                     'transient-inactive-value)))
                     '(expanded collapsed hidden)
                     (propertize "|" 'face 'transient-delimiter))
          (propertize "]" 'face 'transient-delimiter)))

(transient-define-prefix nael-infoview-view-options ()
  "Choose what the infoview shows.
The menu stays open for further choices; \\`C-g' closes it."
  [["Hypotheses"
    ("i" nael-infoview-toggle-instances :transient t
     :description (lambda ()
                    (nael-infoview--switch-description
                     "Hide instances" nael-infoview-hide-instances)))
    ("t" nael-infoview-toggle-types :transient t
     :description (lambda ()
                    (nael-infoview--switch-description
                     "Hide types" nael-infoview-hide-types)))
    ("a" nael-infoview-toggle-inaccessible :transient t
     :description (lambda ()
                    (nael-infoview--switch-description
                     "Hide inaccessible names"
                     nael-infoview-hide-inaccessible)))
    ("l" nael-infoview-toggle-let-values :transient t
     :description (lambda ()
                    (nael-infoview--switch-description
                     "Hide let values" nael-infoview-hide-let-values)))]
   ["Goals"
    ("r" nael-infoview-toggle-reverse :transient t
     :description (lambda ()
                    (nael-infoview--switch-description
                     "Target first" nael-infoview-reverse)))
    ("d" nael-infoview-toggle-diffs :transient t
     :description (lambda ()
                    (nael-infoview--switch-description
                     "Show diffs" nael-infoview-show-diffs)))
    ("e" nael-infoview-cycle-expected-type :transient t
     :description nael-infoview--expected-type-description)]])

(defun nael-infoview--goal-start (pos)
  "Return the start of the goal at POS, or nil if POS is in no goal."
  (when (get-text-property pos 'nael-goal)
    (if (and (> pos (point-min))
             (equal (get-text-property (1- pos) 'nael-goal)
                    (get-text-property pos 'nael-goal)))
        (previous-single-property-change pos 'nael-goal nil (point-min))
      pos)))

(eldoc-add-command 'nael-infoview-next-goal 'nael-infoview-previous-goal)

;; ElDoc shows the subterm at point only after commands that move point.
;; These also land in the infoview: back from a description, and back
;; from a definition, as Emacs, Evil and Doom Emacs do it.
(eldoc-add-command 'quit-window 'xref-go-back 'evil-jump-backward
                   '+popup/quit-window 'better-jumper-jump-backward)

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
