;;; nael-infoview.el --- Infoview for Nael  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see LICENSE.GPL3.

;;; Commentary:

;; The infoview is a buffer that shows the goals and the expected type
;; at point in the selected Nael buffer, like the infoview of the Lean
;; extension for VS Code.  It is fetched over Lean RPC (`nael-rpc'), so
;; it needs Lean 4.30 or later; with older servers, ElDoc keeps
;; displaying the goals.
;;
;; `nael-infoview' toggles the infoview.  It is shown with
;; `display-buffer' in the category `nael-infoview', by default in a side
;; window on the right; customize `display-buffer-alist' to place it
;; elsewhere.
;;
;; The infoview is updated when point moves or the text changes, when
;; the server reports progress at point, and when diagnostics change.
;; Updates are throttled: the first one is immediate, then at most one
;; per `nael-infoview-delay'.  While the server is still processing the
;; line of point, no goals are requested; the infoview then shows its
;; previous content, dimmed.  Goals and expected type are requested
;; together and shown together; an answer to a superseded request is
;; never shown.  The previous content stays until the new one is ready,
;; and its RPC references are released only then.

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'face-remap)

(require 'nael-eglot)
(require 'nael-render)
(require 'nael-rpc)

(defgroup nael-infoview nil
  "Infoview for `nael-mode'."
  :group 'nael-eglot
  :link '(emacs-library-link :tag "Source Lisp File" "nael-infoview.el")
  :prefix "nael-infoview-")

(defcustom nael-infoview-delay 0.05
  "Minimum number of seconds between two updates of the infoview.
The first update after a pause happens at once."
  :type 'number
  :group 'nael-infoview)

(defcustom nael-infoview-display-action
  '((display-buffer-in-side-window)
    (side . right)
    (window-width . 0.4)
    (category . nael-infoview))
  "Action with which `nael-infoview' displays the infoview.
See `display-buffer' for the format.  Entries of `display-buffer-alist'
take precedence; they can match the category `nael-infoview'."
  :type 'sexp
  :group 'nael-infoview)

(defconst nael-infoview-buffer-name "*Nael Infoview*")

;;;; State:

(defvar nael-infoview--source nil
  "The Nael buffer that the infoview follows.")

(defvar nael-infoview--target nil
  "Where the next update asks about, as (BUFFER . POSITION).")

(defvar nael-infoview--timer nil
  "Timer that ends the current throttling period, or nil.")

(defvar nael-infoview--again nil
  "Non-nil if an update was requested during the throttling period.")

(defvar nael-infoview--generation 0
  "Number of the latest update that requested goals.")

(defvar nael-infoview--pending-owner nil
  "Owner of the references of the request that is not yet shown.")

(defvar-local nael-infoview--owner nil
  "Owner of the references that the infoview shows.")

(defvar-local nael-infoview--status nil
  "Status of the infoview, shown in its header line.
nil when up to date, or one of `updating', `processing',
`unsupported', `no-server', or (error . MESSAGE).")

(defvar-local nael-infoview--location nil
  "Description of the position whose information is shown.")

(defvar-local nael-infoview--dim-cookie nil
  "Face remapping that dims outdated content, or nil.")

(defvar-local nael-infoview--last-tick nil
  "Point and modification tick of a Nael buffer at the last update.")

(defvar-local nael-infoview--processing-before nil
  "Non-nil if the server was processing the line of point lately.")

(defvar-local nael-infoview--diagnostics-key nil
  "Diagnostics at the line of point when they last changed.")

;;;; The buffer:

(defvar-keymap nael-infoview-mode-map
  :doc "Keymap of `nael-infoview-mode'."
  "g" #'nael-infoview-refresh)

(define-derived-mode nael-infoview-mode special-mode "Nael Infoview"
  "Major mode of the Nael infoview, which shows goals at point.

\\{nael-infoview-mode-map}"
  (setq-local revert-buffer-function
              (lambda (&rest _) (nael-infoview-refresh)))
  (setq-local header-line-format '(:eval (nael-infoview--header-line)))
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

(defun nael-infoview--header-line ()
  "Return the header line of the infoview."
  (concat
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
  "Release what the infoview holds when its buffer is killed."
  (when nael-infoview--owner
    (nael-rpc-owner-release nael-infoview--owner))
  (when nael-infoview--pending-owner
    (nael-rpc-owner-release nael-infoview--pending-owner)
    (setq nael-infoview--pending-owner nil))
  (nael-infoview--remove-hooks))

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

(defvar nael-infoview-options nil
  "View options of the infoview, as described in `nael-render-goals'.")

(defun nael-infoview--location (buffer pos)
  "Describe position POS in BUFFER for the header line."
  (with-current-buffer buffer
    (save-excursion
      (goto-char pos)
      (format "%s:%d:%d" (buffer-name) (line-number-at-pos)
              (current-column)))))

(defun nael-infoview--show (goals term-goal owner location)
  "Show GOALS and TERM-GOAL, whose references OWNER keeps.
LOCATION describes where they were requested."
  (let ((buffer (nael-infoview--get-buffer-create))
        (rendered (nael-render-goals goals term-goal nael-infoview-options)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (windows (mapcar (lambda (window)
                               (cons window (nael-infoview--window-anchor
                                             window)))
                             (get-buffer-window-list buffer nil t)))
            (previous nael-infoview--owner))
        (delete-all-overlays)
        (erase-buffer)
        (if (string-empty-p (nael-render-result-text rendered))
            (insert (propertize "No goals or expected type at point.\n"
                                'face 'shadow))
          (insert (nael-render-result-text rendered)))
        (pcase-dolist (`(,window . ,anchors) windows)
          (nael-infoview--restore-window window anchors))
        (setq nael-infoview--owner owner
              nael-infoview--location location)
        ;; Only now is nothing shown any longer that uses the old
        ;; references.
        (when previous (nael-rpc-owner-release previous))))
    (nael-infoview--set-status nil)))

;;;; Requests:

(defun nael-infoview--rpc-supported-p ()
  "Return non-nil if the current buffer's server provides Lean RPC.
Lean 4.30 and later announce their RPC wire format."
  (thread-first (nael-eglot-compat-capabilities (eglot-current-server))
                (plist-get :experimental)
                (plist-get :rpcProvider)
                (plist-get :rpcWireFormat)))

(defun nael-infoview--request (buffer pos)
  "Request goals and expected type at POS in BUFFER and show them."
  (let ((generation (cl-incf nael-infoview--generation))
        (owner (nael-rpc-make-owner))
        (location (nael-infoview--location buffer pos))
        (goals 'pending)
        (term-goal 'pending)
        error)
    (when nael-infoview--pending-owner
      (nael-rpc-owner-release nael-infoview--pending-owner))
    (setq nael-infoview--pending-owner owner)
    (cl-labels
        ((finish ()
           (unless (or (eq goals 'pending) (eq term-goal 'pending))
             (when (= generation nael-infoview--generation)
               (setq nael-infoview--pending-owner nil)
               (if error
                   (progn
                     (nael-rpc-owner-release owner)
                     (nael-infoview--set-status
                      (cons 'error (or (plist-get error :message)
                                       (format "%S" error)))))
                 (nael-infoview--show goals term-goal owner location)))))
         (fail (e)
           (setq error e)))
      (nael-infoview--set-status 'updating)
      (let ((handle (with-current-buffer buffer (nael-rpc-open pos))))
        (nael-rpc-get-interactive-goals
         handle (lambda (result) (setq goals result) (finish))
         :owner owner :slot 'infoview-goals
         :failure (lambda (e) (fail e) (setq goals nil) (finish)))
        (nael-rpc-get-interactive-term-goal
         handle (lambda (result) (setq term-goal result) (finish))
         :owner owner :slot 'infoview-term-goal
         :failure (lambda (e) (fail e) (setq term-goal nil) (finish)))))))

(defun nael-infoview--update ()
  "Update the infoview for `nael-infoview--target' now."
  (pcase-let ((`(,buffer . ,pos) nael-infoview--target))
    (when (and (buffer-live-p buffer) (nael-infoview-visible-p))
      (with-current-buffer buffer
        (setq nael-infoview--source buffer)
        (cond
         ((not (eglot-managed-p))
          (nael-infoview--set-status 'no-server))
         ((not (nael-infoview--rpc-supported-p))
          (nael-infoview--set-status 'unsupported))
         ((or (null nael-eglot-file-progress)
              (nael-eglot-processing-line-p
               (save-excursion (goto-char pos)
                               (1- (line-number-at-pos nil t)))))
          (nael-infoview--set-status 'processing))
         (t
          (nael-infoview--request buffer pos)))))))

(defun nael-infoview--cooldown-end ()
  "End the throttling period and run a deferred update, if any."
  (setq nael-infoview--timer nil)
  (when nael-infoview--again
    (setq nael-infoview--again nil)
    (nael-infoview--schedule)))

(defun nael-infoview--schedule (&optional buffer pos)
  "Update the infoview for POS in BUFFER, throttled.
BUFFER and POS default to the last target.  The first update after a
pause runs at once, later ones at most every `nael-infoview-delay'."
  (when buffer
    (setq nael-infoview--target (cons buffer pos)))
  (if nael-infoview--timer
      (setq nael-infoview--again t)
    (setq nael-infoview--timer
          (run-with-timer nael-infoview-delay nil
                          #'nael-infoview--cooldown-end))
    (nael-infoview--update)))

;;;; Triggers:

(defun nael-infoview--followed-p (buffer)
  "Return non-nil if the infoview should follow BUFFER."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'nael-mode) (eglot-managed-p)))))

(defun nael-infoview--post-command ()
  "Update the infoview if point or text changed in a Nael buffer."
  (when (and (nael-infoview-visible-p)
             (nael-infoview--followed-p (current-buffer)))
    (let ((tick (cons (point) (buffer-chars-modified-tick))))
      (unless (and (eq (current-buffer) nael-infoview--source)
                   (equal tick nael-infoview--last-tick))
        (setq nael-infoview--last-tick tick)
        (nael-infoview--schedule (current-buffer) (point))))))

(defun nael-infoview--target-line ()
  "Return the 0-based line of the target of the infoview."
  (save-excursion
    (goto-char (cdr nael-infoview--target))
    (1- (line-number-at-pos nil t))))

(defun nael-infoview--file-progress (_processing)
  "Update the infoview if processing at point started or ended.
Also update it if it is waiting for processing to end."
  (when (and (eq (current-buffer) (car nael-infoview--target))
             (nael-infoview-visible-p))
    (let ((now (nael-eglot-processing-line-p (nael-infoview--target-line))))
      (when (or now
                nael-infoview--processing-before
                (eq (buffer-local-value 'nael-infoview--status
                                        (nael-infoview--buffer))
                    'processing))
        (nael-infoview--schedule))
      (setq nael-infoview--processing-before now))))

(defun nael-infoview--diagnostics (diagnostics)
  "Update the infoview if DIAGNOSTICS at the line of point changed."
  (when (and (eq (current-buffer) (car nael-infoview--target))
             (nael-infoview-visible-p))
    (let* ((line (nael-infoview--target-line))
           (key (seq-filter
                 (lambda (diagnostic)
                   (let ((range (plist-get diagnostic :range)))
                     (<= (plist-get (plist-get range :start) :line)
                         line
                         (plist-get (plist-get range :end) :line))))
                 diagnostics)))
      (unless (equal key nael-infoview--diagnostics-key)
        (setq nael-infoview--diagnostics-key key)
        (nael-infoview--schedule)))))

(defun nael-infoview--add-hooks ()
  "Make the infoview follow Nael buffers."
  (add-hook 'post-command-hook #'nael-infoview--post-command)
  (add-hook 'nael-eglot-file-progress-functions #'nael-infoview--file-progress)
  (add-hook 'nael-eglot-diagnostics-functions #'nael-infoview--diagnostics))

(defun nael-infoview--remove-hooks ()
  "Stop following Nael buffers."
  (remove-hook 'post-command-hook #'nael-infoview--post-command)
  (remove-hook 'nael-eglot-file-progress-functions
               #'nael-infoview--file-progress)
  (remove-hook 'nael-eglot-diagnostics-functions
               #'nael-infoview--diagnostics))

(defun nael-infoview-shows-goals-p ()
  "Return non-nil if the infoview shows goals for the current buffer.
That is, if it is visible and the buffer's server provides Lean RPC.
ElDoc then leaves goals to the infoview."
  (and (nael-infoview-visible-p)
       (eglot-current-server)
       (nael-infoview--rpc-supported-p)))

;;;; Commands:

(defun nael-infoview-refresh ()
  "Update the infoview now."
  (interactive)
  (when nael-infoview--target
    (nael-infoview--update)))

;;;###autoload
(defun nael-infoview ()
  "Toggle the display of the Nael infoview.
The infoview shows the goals and the expected type at point in the
selected Nael buffer.  It is displayed with
`nael-infoview-display-action'."
  (interactive)
  (if-let* ((window (nael-infoview-visible-p)))
      (quit-window nil window)
    (let ((source (current-buffer)))
      (display-buffer (nael-infoview--get-buffer-create)
                      nael-infoview-display-action)
      (nael-infoview--add-hooks)
      (when (nael-infoview--followed-p source)
        (with-current-buffer source
          (setq nael-infoview--last-tick nil))
        (nael-infoview--schedule source (with-current-buffer source
                                          (point)))))))

(provide 'nael-infoview)

;;; nael-infoview.el ends here
