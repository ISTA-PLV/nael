;;; lena-infoview.el --- Infoview for Lena  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see LICENSE.GPL3.

;;; Commentary:

;; The infoview is a buffer that shows the goals, the expected type and
;; the messages at point in the selected Lena buffer, like the infoview
;; of the Lean extension for VS Code.  It is fetched over Lean RPC
;; (`lena-rpc'), so it needs Lean 4.30 or later; with older servers,
;; ElDoc keeps displaying the goals.
;;
;; `lena-infoview' toggles the infoview.  It is shown with
;; `display-buffer' in the category `lena-infoview', by default in a side
;; window on the right; customize `display-buffer-alist' to place it
;; elsewhere.
;;
;; The infoview is updated when point moves or the text changes, when
;; the server reports progress at point, and when diagnostics change.
;; Updates are throttled: the first one is immediate, then at most one
;; per `lena-infoview-delay'.  While the server is still processing the
;; line of point, no goals are requested; the infoview then shows its
;; previous content, dimmed.  Goals, expected type and the messages of
;; the line of point are requested together and shown together; an
;; answer to a superseded request is never shown.  The previous content
;; stays until the new one is ready, and its RPC references are released
;; only then.
;;
;; Subterms in the infoview can be asked about, see
;; `lena-infoview-hover'.

;;; Code:

(require 'cl-lib)
(require 'eglot)

(require 'lena-eglot)
(require 'lena-infoview-buffer)
(require 'lena-margin)
(require 'lena-render)
(require 'lena-rpc)

(defcustom lena-infoview-delay 0.05
  "Minimum number of seconds between two updates of the infoview.
The first update after a pause happens at once."
  :type 'number
  :group 'lena-infoview)

(defcustom lena-infoview-display-action
  '((display-buffer-in-side-window)
    (side . right)
    (window-width . 0.4)
    (category . lena-infoview))
  "Action with which `lena-infoview' displays the infoview.
See `display-buffer' for the format.  Entries of `display-buffer-alist'
take precedence; they can match the category `lena-infoview'."
  :type 'sexp
  :group 'lena-infoview)

;;;; State:

(defvar lena-infoview--source nil
  "The Lena buffer that the infoview follows.")

(defvar lena-infoview--target nil
  "Where the next update asks about, as (BUFFER . POSITION).")

(defvar lena-infoview--timer nil
  "Timer that ends the current throttling period, or nil.")

(defvar lena-infoview--again nil
  "Non-nil if an update was requested during the throttling period.")

(defvar lena-infoview--generation 0
  "Number of the latest update that requested goals.")

(defvar lena-infoview--pending-owner nil
  "Owner of the references of the request that is not yet shown.")

(defvar-local lena-infoview--last-tick nil
  "Point and modification tick of a Lena buffer at the last update.")

(defvar-local lena-infoview--processing-before nil
  "Non-nil if the server was processing the line of point lately.")

(defvar-local lena-infoview--diagnostics-key nil
  "Diagnostics at the line of point when they last changed.")

(defun lena-infoview--abandon-pending ()
  "Make the answers to the pending update, if any, be dropped.
Their references are released when they arrive."
  (cl-incf lena-infoview--generation)
  (when lena-infoview--pending-owner
    (lena-rpc-owner-release lena-infoview--pending-owner)
    (setq lena-infoview--pending-owner nil)))

(defun lena-infoview--buffer-killed ()
  "Stop following Lena buffers and drop the pending update."
  (lena-infoview--abandon-pending)
  (lena-infoview--remove-hooks))

(defun lena-infoview--setup-buffer ()
  "Prepare a new infoview buffer for being updated."
  (add-hook 'kill-buffer-hook #'lena-infoview--buffer-killed nil t))

(add-hook 'lena-infoview-mode-hook #'lena-infoview--setup-buffer)

;;;; Requests:

(defun lena-infoview--rpc-supported-p ()
  "Return non-nil if the current buffer's server provides Lean RPC.
Lean 4.30 and later announce their RPC wire format."
  (eglot-server-capable :experimental :rpcProvider :rpcWireFormat))

(defun lena-infoview--accomplished-title (buffer pos)
  "Return the line for goals at POS in BUFFER if their proof is complete.
Return nil if it is not, as far as the server has said."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (lena-eglot-goals-accomplished-p
             (save-excursion (goto-char pos) (1- (line-number-at-pos nil t))))
        (string-trim-right
         (concat "Goals accomplished "
                 (lena-margin-scale-emoji
                  (lena-margin-sign 'lena-margin-goals-accomplished))))))))

(defun lena-infoview--request (buffer pos)
  "Request goals, expected type and messages at POS in BUFFER; show them.
The messages are those of the line of POS.  If they cannot be had, the
goals are shown without them."
  (let ((generation (cl-incf lena-infoview--generation))
        (owner (lena-rpc-make-owner))
        (location (lena-infoview--location buffer pos))
        (line (with-current-buffer buffer
                (save-excursion (goto-char pos)
                                (1- (line-number-at-pos nil t)))))
        (tick (buffer-chars-modified-tick buffer))
        (line-count (with-current-buffer buffer
                      (save-restriction
                        (widen)
                        (count-lines (point-min) (point-max)))))
        (goals 'pending)
        (term-goal 'pending)
        (messages 'pending)
        handle error)
    (when lena-infoview--pending-owner
      (lena-rpc-owner-release lena-infoview--pending-owner))
    (setq lena-infoview--pending-owner owner)
    (cl-labels
        ((finish ()
           (unless (or (eq goals 'pending) (eq term-goal 'pending)
                       (eq messages 'pending))
             (when (= generation lena-infoview--generation)
               (setq lena-infoview--pending-owner nil)
               (if error
                   (progn
                     (lena-rpc-owner-release owner)
                     (lena-infoview--set-status
                      (cons 'error (or (plist-get error :message)
                                       (format "%S" error)))))
                 (lena-infoview--show goals term-goal owner location
                                      handle
                                      (lena-infoview--accomplished-title
                                       buffer pos)
                                      (list :diagnostics messages :line line
                                            :line-count line-count
                                            :buffer buffer :tick tick))))))
         (fail (e)
           (setq error e)))
      (lena-infoview--set-status 'updating)
      (setq handle (with-current-buffer buffer (lena-rpc-open pos)))
      (lena-rpc-get-interactive-goals
       handle (lambda (result) (setq goals result) (finish))
       :owner owner :slot 'infoview-goals
       :failure (lambda (e) (fail e) (setq goals nil) (finish)))
      (if (eq lena-infoview-expected-type 'hidden)
          (setq term-goal nil)
        (lena-rpc-get-interactive-term-goal
         handle (lambda (result) (setq term-goal result) (finish))
         :owner owner :slot 'infoview-term-goal
         :failure (lambda (e) (fail e) (setq term-goal nil) (finish))))
      (lena-rpc-get-interactive-diagnostics
       handle line (lambda (result) (setq messages result) (finish))
       :owner owner :slot 'infoview-messages
       :failure (lambda (_) (setq messages nil) (finish))))))

(defun lena-infoview--update (&optional force)
  "Update the infoview for `lena-infoview--target' now.
While the server reports that it is processing the line of the target,
show that instead of requesting goals, unless FORCE is non-nil.  A
request made earlier would only wait on the server, so this is just to
save work; when the server has not reported anything yet, request.
An update that requests nothing drops the answers to an earlier one,
which would no longer be about the target."
  (pcase-let ((`(,buffer . ,pos) lena-infoview--target))
    (when (and (buffer-live-p buffer) (lena-infoview-visible-p))
      (with-current-buffer buffer
        (setq lena-infoview--source buffer)
        (let ((status
               (cond
                ((not (eglot-managed-p)) 'no-server)
                ((not (lena-infoview--rpc-supported-p)) 'unsupported)
                ((and (not force)
                      (lena-eglot-processing-line-p
                       (save-excursion (goto-char pos)
                                       (1- (line-number-at-pos nil t)))))
                 'processing))))
          (if (not status)
              (lena-infoview--request buffer pos)
            (lena-infoview--abandon-pending)
            (lena-infoview--set-status status)))))))

(defun lena-infoview--cooldown-end ()
  "End the throttling period and run a deferred update, if any."
  (setq lena-infoview--timer nil)
  (when lena-infoview--again
    (setq lena-infoview--again nil)
    (lena-infoview--schedule)))

(defun lena-infoview--schedule (&optional buffer pos)
  "Update the infoview for POS in BUFFER, throttled.
BUFFER and POS default to the last target.  The first update after a
pause runs at once, later ones at most every `lena-infoview-delay'."
  (when buffer
    (setq lena-infoview--target (cons buffer pos)))
  (if lena-infoview--timer
      (setq lena-infoview--again t)
    (setq lena-infoview--timer
          (run-with-timer lena-infoview-delay nil
                          #'lena-infoview--cooldown-end))
    (lena-infoview--update)))

;;;; Triggers:

(defun lena-infoview--followed-p (buffer)
  "Return non-nil if the infoview should follow BUFFER."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'lena-mode) (eglot-managed-p)))))

(defun lena-infoview--post-command ()
  "Update the infoview if point or text changed in a Lena buffer."
  (when (and (lena-infoview-visible-p)
             (lena-infoview--followed-p (current-buffer)))
    (let ((tick (cons (point) (buffer-chars-modified-tick))))
      (unless (and (eq (current-buffer) lena-infoview--source)
                   (equal tick lena-infoview--last-tick))
        (setq lena-infoview--last-tick tick)
        (lena-infoview--schedule (current-buffer) (point))))))

(defun lena-infoview--target-line ()
  "Return the 0-based line of the target of the infoview."
  (save-excursion
    (goto-char (cdr lena-infoview--target))
    (1- (line-number-at-pos nil t))))

(defun lena-infoview--file-progress (_processing)
  "Update the infoview if processing at point started or ended.
Also update it if it is waiting for processing to end."
  (when (and (eq (current-buffer) (car lena-infoview--target))
             (lena-infoview-visible-p))
    (let ((now (lena-eglot-processing-line-p (lena-infoview--target-line))))
      (when (or now
                lena-infoview--processing-before
                (eq (buffer-local-value 'lena-infoview--status
                                        (lena-infoview--buffer))
                    'processing))
        (lena-infoview--schedule))
      (setq lena-infoview--processing-before now))))

(defun lena-infoview--diagnostics (diagnostics)
  "Update the infoview if DIAGNOSTICS at the line of point changed.
The full ranges of diagnostics count, so that a proof that becomes
complete around point updates the infoview's title."
  (when (and (eq (current-buffer) (car lena-infoview--target))
             (lena-infoview-visible-p))
    (let* ((line (lena-infoview--target-line))
           (key (seq-filter
                 (lambda (diagnostic)
                   (pcase-let ((`(,first . ,last)
                                (lena-eglot-diagnostic-lines diagnostic t)))
                     (<= first line last)))
                 diagnostics)))
      (unless (equal key lena-infoview--diagnostics-key)
        (setq lena-infoview--diagnostics-key key)
        (lena-infoview--schedule)))))

(defun lena-infoview--track ()
  "Make the infoview follow the current buffer, if it is a Lena buffer."
  (if (lena-infoview--followed-p (current-buffer))
      (progn
        (add-hook 'post-command-hook #'lena-infoview--post-command nil t)
        (add-hook 'lena-eglot-file-progress-functions
                  #'lena-infoview--file-progress nil t)
        (add-hook 'lena-eglot-diagnostics-functions
                  #'lena-infoview--diagnostics nil t))
    (lena-infoview--untrack)))

(defun lena-infoview--untrack ()
  "Stop the infoview from following the current buffer."
  (remove-hook 'post-command-hook #'lena-infoview--post-command t)
  (remove-hook 'lena-eglot-file-progress-functions
               #'lena-infoview--file-progress t)
  (remove-hook 'lena-eglot-diagnostics-functions
               #'lena-infoview--diagnostics t))

(defun lena-infoview--add-hooks ()
  "Make the infoview follow Lena buffers managed by Eglot.
The hooks are local to those buffers.  `eglot-managed-mode-hook' adds
them to buffers that Eglot starts managing later."
  (add-hook 'eglot-managed-mode-hook #'lena-infoview--track)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'lena-mode)
        (lena-infoview--track)))))

(defun lena-infoview--remove-hooks ()
  "Stop following Lena buffers."
  (remove-hook 'eglot-managed-mode-hook #'lena-infoview--track)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (lena-infoview--untrack))))

(defun lena-infoview-shows-goals-p ()
  "Return non-nil if the infoview shows goals for the current buffer.
That is, if it is visible and the buffer's server provides Lean RPC.
ElDoc then leaves goals to the infoview."
  (and (lena-infoview-visible-p)
       (eglot-current-server)
       (lena-infoview--rpc-supported-p)))

;;;; Commands:

(defun lena-infoview-refresh ()
  "Update the infoview now, even while the server is processing."
  (interactive)
  (when lena-infoview--target
    (lena-infoview--update 'force)))

;;;###autoload
(defun lena-infoview ()
  "Toggle the display of the Lena infoview.
The infoview shows the goals and the expected type at point in the
selected Lena buffer.  It is displayed with
`lena-infoview-display-action'."
  (interactive)
  (if-let* ((window (lena-infoview-visible-p)))
      (quit-window nil window)
    (let ((source (current-buffer)))
      (setq lena-infoview--window
            (display-buffer (lena-infoview--get-buffer-create)
                            lena-infoview-display-action))
      (lena-infoview--add-hooks)
      (when (lena-infoview--followed-p source)
        (with-current-buffer source
          (setq lena-infoview--last-tick nil))
        (lena-infoview--schedule source (with-current-buffer source
                                          (point)))))))

(provide 'lena-infoview)

;;; lena-infoview.el ends here
