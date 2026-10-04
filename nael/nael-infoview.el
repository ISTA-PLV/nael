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

(require 'nael-eglot)
(require 'nael-infoview-buffer)
(require 'nael-render)
(require 'nael-rpc)

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

(defvar-local nael-infoview--last-tick nil
  "Point and modification tick of a Nael buffer at the last update.")

(defvar-local nael-infoview--processing-before nil
  "Non-nil if the server was processing the line of point lately.")

(defvar-local nael-infoview--diagnostics-key nil
  "Diagnostics at the line of point when they last changed.")

(defun nael-infoview--buffer-killed ()
  "Stop following Nael buffers and release pending references."
  (when nael-infoview--pending-owner
    (nael-rpc-owner-release nael-infoview--pending-owner)
    (setq nael-infoview--pending-owner nil))
  (nael-infoview--remove-hooks))

(defun nael-infoview--setup-buffer ()
  "Prepare a new infoview buffer for being updated."
  (add-hook 'kill-buffer-hook #'nael-infoview--buffer-killed nil t))

(add-hook 'nael-infoview-mode-hook #'nael-infoview--setup-buffer)

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
        handle error)
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
                 (nael-infoview--show goals term-goal owner location
                                      handle)))))
         (fail (e)
           (setq error e)))
      (nael-infoview--set-status 'updating)
      (setq handle (with-current-buffer buffer (nael-rpc-open pos)))
        (nael-rpc-get-interactive-goals
       handle (lambda (result) (setq goals result) (finish))
       :owner owner :slot 'infoview-goals
       :failure (lambda (e) (fail e) (setq goals nil) (finish)))
      (if (eq nael-infoview-expected-type 'hidden)
          (setq term-goal nil)
        (nael-rpc-get-interactive-term-goal
         handle (lambda (result) (setq term-goal result) (finish))
         :owner owner :slot 'infoview-term-goal
         :failure (lambda (e) (fail e) (setq term-goal nil) (finish)))))))

(defun nael-infoview--update (&optional force)
  "Update the infoview for `nael-infoview--target' now.
While the server reports that it is processing the line of the target,
show that instead of requesting goals, unless FORCE is non-nil.  A
request made earlier would only wait on the server, so this is just to
save work; when the server has not reported anything yet, request."
  (pcase-let ((`(,buffer . ,pos) nael-infoview--target))
    (when (and (buffer-live-p buffer) (nael-infoview-visible-p))
      (with-current-buffer buffer
        (setq nael-infoview--source buffer)
        (cond
         ((not (eglot-managed-p))
          (nael-infoview--set-status 'no-server))
         ((not (nael-infoview--rpc-supported-p))
          (nael-infoview--set-status 'unsupported))
         ((and (not force)
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

(defun nael-infoview--track ()
  "Make the infoview follow the current buffer, if it is a Nael buffer."
  (if (nael-infoview--followed-p (current-buffer))
      (progn
        (add-hook 'post-command-hook #'nael-infoview--post-command nil t)
        (add-hook 'nael-eglot-file-progress-functions
                  #'nael-infoview--file-progress nil t)
        (add-hook 'nael-eglot-diagnostics-functions
                  #'nael-infoview--diagnostics nil t))
    (nael-infoview--untrack)))

(defun nael-infoview--untrack ()
  "Stop the infoview from following the current buffer."
  (remove-hook 'post-command-hook #'nael-infoview--post-command t)
  (remove-hook 'nael-eglot-file-progress-functions
               #'nael-infoview--file-progress t)
  (remove-hook 'nael-eglot-diagnostics-functions
               #'nael-infoview--diagnostics t))

(defun nael-infoview--add-hooks ()
  "Make the infoview follow Nael buffers managed by Eglot.
The hooks are local to those buffers.  `eglot-managed-mode-hook' adds
them to buffers that Eglot starts managing later."
  (add-hook 'eglot-managed-mode-hook #'nael-infoview--track)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'nael-mode)
        (nael-infoview--track)))))

(defun nael-infoview--remove-hooks ()
  "Stop following Nael buffers."
  (remove-hook 'eglot-managed-mode-hook #'nael-infoview--track)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (nael-infoview--untrack))))

(defun nael-infoview-shows-goals-p ()
  "Return non-nil if the infoview shows goals for the current buffer.
That is, if it is visible and the buffer's server provides Lean RPC.
ElDoc then leaves goals to the infoview."
  (and (nael-infoview-visible-p)
       (eglot-current-server)
       (nael-infoview--rpc-supported-p)))

;;;; Commands:

(defun nael-infoview-refresh ()
  "Update the infoview now, even while the server is processing."
  (interactive)
  (when nael-infoview--target
    (nael-infoview--update 'force)))

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
