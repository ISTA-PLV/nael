;;; nael-infoview-hover.el --- Information on subterms in Nael's infoview  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see LICENSE.GPL3.

;;; Commentary:

;; Every subterm that the infoview shows carries a reference to what the
;; server knows about it (see `nael-render').  This file asks the server
;; about the subterm at point, as an editor asks about the symbol at
;; point in source code:
;;
;; - ElDoc shows the subterm with all its arguments made explicit and its
;;   type in the echo area, and its documentation in `eldoc-doc-buffer'.
;;   Packages that show ElDoc elsewhere, such as in a child frame, show
;;   it there.
;;
;; - With `tooltip-mode', the tooltip of a subterm under the mouse shows
;;   the same documentation, as Flymake's tooltips show diagnostics.
;;
;; - `nael-infoview-describe', on RET, a click, or `C-h .', shows the
;;   same and more in the *Help* buffer, like `describe-function'.  The
;;   subterms shown there can be described in turn, and Help's history
;;   leads back.  In Doom Emacs, `K' does the same.  Closing the help
;;   window returns to the infoview.
;;
;; - `xref-find-definitions', on `M-.' or Doom's `gd', goes to where the
;;   subterm's head is defined; `M-,' or Doom's `C-o' come back.
;;
;; The functions here work in any buffer that shows rendered nodes and
;; whose `nael-infoview--handle' is the RPC handle through which their
;; references were obtained: the infoview and the descriptions in the
;; *Help* buffer.
;;
;; A description keeps the references of the subterms it shows for as
;; long as Help's history can lead back to it.

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'eldoc)
(require 'help-mode)
(require 'xref)

(require 'nael-eglot-compat)
(require 'nael-render)
(require 'nael-rpc)

(defvar-local nael-infoview--handle nil
  "RPC handle through which the references shown in this buffer came.
References belong to an RPC session, so calls about them must use it.")

(defvar nael-infoview-node-map)

(defvar-local nael-infoview--info-cache nil
  "Documentation of the nodes shown, as far as asked for, or nil.
Maps nodes to (DOC . ECHO), see `nael-infoview--info-strings'.")

(defun nael-infoview--info-cache ()
  "Return the documentation of nodes of the current buffer, a table."
  (or nael-infoview--info-cache
      (setq nael-infoview--info-cache
            (make-hash-table :test #'eq :weakness 'key))))

(defvar-local nael-infoview--tooltip-node nil
  "The node whose documentation is being fetched for its tooltip.")

;;;; Content:

(defun nael-infoview--diff-note (status)
  "Return a sentence explaining STATUS, the diff status of a subterm."
  (pcase status
    ("wasChanged" "The tactic at point changed this subterm.")
    ("willChange" "The tactic at point will change this subterm.")
    ("wasInserted" "The tactic at point inserted this subterm.")
    ("willInsert" "The tactic at point will insert this subterm.")
    ("wasDeleted" "The tactic at point deleted this subterm.")
    ("willDelete" "The tactic at point will delete this subterm.")))

(defun nael-infoview--docstring (info)
  "Return the documentation in INFO, formatted, or nil.
Markdown is shown as Eglot shows it, with `markdown-mode' if installed."
  (let ((doc (plist-get info :doc)))
    (when (and (stringp doc) (not (string-blank-p doc)))
      (string-trim (nael-eglot-compat-format-markup doc)))))

(defun nael-infoview--without-nodes (string)
  "Return a copy of STRING without the nodes in it."
  (let ((string (copy-sequence string)))
    (remove-list-of-text-properties 0 (length string) '(nael-node) string)
    string))

(defun nael-infoview--info-strings (info status)
  "Return the documentation of INFO, as (DOC . ECHO).
ECHO is the subterm and its type; DOC adds the documentation and an
explanation of STATUS, the diff status of the subterm."
  (let ((echo (nael-infoview--without-nodes
               (nael-render-result-text (nael-render-info info)))))
    (cons (string-join (delq nil (list echo
                                       (nael-infoview--docstring info)
                                       (nael-infoview--diff-note status)))
                       "\n\n")
          echo)))

;;;; Documentation of nodes:

(defun nael-infoview--usable-handle ()
  "Return the RPC handle of the current buffer if it is live, or nil."
  (when-let* ((handle nael-infoview--handle)
              ((nael-rpc-handle-live-p handle)))
    handle))

(defun nael-infoview--fetch-info (node slot callback &optional failure)
  "Call CALLBACK with the documentation of NODE, as (DOC . ECHO).
Take it from the cache, at once, or ask Lean in the RPC slot SLOT and
cache the answer.  Call FAILURE, if non-nil, if Lean cannot answer."
  (if-let* ((strings (gethash node (nael-infoview--info-cache))))
      (funcall callback strings)
    (let ((cache (nael-infoview--info-cache))
          (owner (nael-rpc-make-owner)))
      (nael-rpc-info-to-interactive
       nael-infoview--handle (nael-render-node-info node)
       (lambda (info)
         (let ((strings (and info
                             (nael-infoview--info-strings
                              info (nael-render-node-diff-status node)))))
           ;; The strings refer to nothing on the server.
           (nael-rpc-owner-release owner)
           (if strings
               (progn (puthash node strings cache)
                      (funcall callback strings))
             (when failure (funcall failure nil)))))
       :owner owner :slot slot
       :failure (or failure #'ignore)))))

;;;; ElDoc:

(defun nael-infoview-eldoc-function (callback &rest _)
  "Report the subterm at point to ElDoc with CALLBACK.
The echo area shows the subterm with all its arguments made explicit,
and its type.  The documentation buffer of ElDoc also shows the
documentation of the subterm and how the tactic at point changes it."
  (when-let* ((node (nael-render-node-at (point)))
              ((nael-infoview--usable-handle)))
    (nael-infoview--fetch-info node 'infoview-eldoc
                               (pcase-lambda (`(,doc . ,echo))
                                 (funcall callback doc :echo echo)))
    t))

;;;; Tooltips:

(defun nael-infoview--node-under-mouse ()
  "Return the node under the mouse pointer, or nil."
  (pcase (mouse-pixel-position)
    (`(,frame ,(and x (pred numberp)) . ,y)
     (let* ((posn (posn-at-x-y x y frame))
            (window (posn-window posn))
            (pos (posn-point posn)))
       (when (and (windowp window) pos)
         (with-current-buffer (window-buffer window)
           (nael-render-node-at pos)))))))

(defun nael-infoview--tooltip-done (buffer node)
  "Note in BUFFER that the tooltip of NODE is no longer being fetched."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq nael-infoview--tooltip-node node)
        (setq nael-infoview--tooltip-node nil)))))

(defun nael-infoview--node-help-echo (window _object pos)
  "Return the tooltip for the subterm at POS in WINDOW, if there is one.
It is the documentation of the subterm, as in the documentation buffer
of ElDoc.  Without `tooltip-mode', Emacs would show it in the echo
area, where ElDoc shows the subterm at point; then there is none.
If the documentation is not known yet, ask Lean for it, return nil,
and show the tooltip once Lean answers, if the mouse is still there."
  (when (bound-and-true-p tooltip-mode)
    (with-current-buffer (if window (window-buffer window) (current-buffer))
      (when-let* ((node (nael-render-node-at pos))
                  ((nael-infoview--usable-handle)))
        (or (car (gethash node (nael-infoview--info-cache)))
            (let ((buffer (current-buffer)))
              (unless (eq node nael-infoview--tooltip-node)
                (setq nael-infoview--tooltip-node node)
                (nael-infoview--fetch-info
                 node 'infoview-tooltip
                 (lambda (strings)
                   (nael-infoview--tooltip-done buffer node)
                   (when (eq node (nael-infoview--node-under-mouse))
                     (funcall show-help-function (car strings))))
                 (lambda (_) (nael-infoview--tooltip-done buffer node))))
              nil))))))

;;;; Descriptions:

(cl-defstruct (nael-infoview--page (:constructor nael-infoview--page-create)
                                   (:copier nil))
  "The description of a subterm in the *Help* buffer."
  handle                                ; RPC handle of its references
  info                                  ; result of `infoToInteractive'
  owner                                 ; owner of the references in INFO
  status                                ; diff status of the subterm
  source)                               ; marker where the goals were asked

(defvar nael-infoview--pages nil
  "Descriptions whose references are kept, newest first.")

(defun nael-infoview--handle-marker (handle)
  "Return a marker at the position that HANDLE was opened at, or nil."
  (let ((buffer (nael-rpc-handle-buffer handle)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((position (plist-get (nael-rpc-handle-position handle)
                                   :position)))
          (copy-marker (car (eglot-range-region
                             (list :start position :end position)))))))))

(defun nael-infoview--location-string (marker)
  "Describe the position of MARKER as FILE:LINE:COLUMN."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      (format "%s:%d:%d" (buffer-name) (line-number-at-pos)
              (current-column)))))

(defun nael-infoview--live-handle ()
  "Return the RPC handle of the current buffer, if it can still be used.
Signal a user error otherwise."
  (let ((handle nael-infoview--handle))
    (unless (and handle (nael-rpc-handle-live-p handle)
                 (buffer-live-p (nael-rpc-handle-buffer handle)))
      (user-error "The Lean session of these goals has ended; \
describe the subterm again from the infoview"))
    handle))

(defun nael-infoview--failure-message (error)
  "Return a message for ERROR of a call about a subterm."
  (let ((code (nael-rpc-error-code error)))
    (if (or (eql code -32602)           ; invalid params: a dead reference
            (nael-rpc-dead-code-p code))
        "Lean no longer knows this subterm; \
describe it again from the infoview"
      (format "Lean could not look up this subterm: %s"
              (or (plist-get error :message) error)))))

(defun nael-infoview--report-failure (error)
  "Report ERROR of a call about a subterm to the user."
  (message "%s" (nael-infoview--failure-message error)))

(defun nael-infoview--decorate (nodes offset)
  "Make NODES, rendered at OFFSET in the current buffer, react to input.
Their extents are moved by OFFSET, to buffer positions.  Each gets an
overlay with `mouse-face' and the keymap `nael-infoview-node-map'.
Overlays rather than text properties, because nodes nest, and because
text properties also apply just after their end."
  (dolist (node nodes)
    (cl-incf (nael-render-node-beg node) offset)
    (cl-incf (nael-render-node-end node) offset)
    (let ((overlay (make-overlay (nael-render-node-beg node)
                                 (nael-render-node-end node))))
      (overlay-put overlay 'mouse-face 'highlight)
      (overlay-put overlay 'help-echo #'nael-infoview--node-help-echo)
      (overlay-put overlay 'keymap nael-infoview-node-map))))

(defun nael-infoview--insert-page (page)
  "Insert the description PAGE at point in the *Help* buffer."
  (let* ((info (nael-infoview--page-info page))
         (rendered (nael-render-info info))
         (start (point))
         (source (nael-infoview--page-source page)))
    (insert (nael-render-result-text rendered) "\n")
    (nael-infoview--decorate (nael-render-result-nodes rendered)
                             (1- start))
    (when-let* ((doc (nael-infoview--docstring info)))
      (insert "\n" doc "\n"))
    (when-let* ((note (nael-infoview--diff-note
                       (nael-infoview--page-status page))))
      (insert "\n" note "\n"))
    (when (and source (marker-buffer source))
      (insert "\nFrom the goals at ")
      (insert-text-button (nael-infoview--location-string source)
                          'action (lambda (_)
                                    (pop-to-buffer (marker-buffer source))
                                    (goto-char source))
                          'help-echo "mouse-1, RET: go there")
      (insert ".\n"))))

(defun nael-infoview--help-buffer-pages ()
  "Return the descriptions that the *Help* buffer shows or can go back to."
  ;; Not `help-buffer', which creates the buffer.
  (when-let* ((buffer (get-buffer "*Help*")))
    (with-current-buffer buffer
      (seq-filter #'nael-infoview--page-p
                  (flatten-tree (list help-xref-stack-item
                                      help-xref-stack
                                      help-xref-forward-stack))))))

(defun nael-infoview--release-pages ()
  "Release the references of descriptions that Help cannot go back to."
  (let ((reachable (nael-infoview--help-buffer-pages)))
    (setq nael-infoview--pages
          (seq-filter (lambda (page)
                        (or (memq page reachable)
                            (progn
                              (nael-rpc-owner-release
                               (nael-infoview--page-owner page))
                              (when-let* ((source (nael-infoview--page-source
                                                   page)))
                                (set-marker source nil))
                              nil)))
                      nael-infoview--pages))))

(defvar-local nael-infoview--eldoc-was nil
  "Whether ElDoc was on in this *Help* buffer before it described subterms.")

(defun nael-infoview--show-page (page &optional interactive)
  "Show the description PAGE in the *Help* buffer.
INTERACTIVE is non-nil if a command asked for it, rather than Help's
history.  This is the function of PAGE's items in Help's history."
  (help-setup-xref (list #'nael-infoview--show-page page) interactive)
  (with-help-window (help-buffer)
    (with-current-buffer standard-output
      (nael-infoview--insert-page page)
      (setq nael-infoview--handle (nael-infoview--page-handle page))
      (unless (local-variable-p 'nael-infoview--eldoc-was)
        (setq-local nael-infoview--eldoc-was eldoc-mode))
      (nael-infoview-hover-setup)))
  (nael-infoview--release-pages))

(defun nael-infoview--help-cleanup ()
  "Undo `nael-infoview-hover-setup' in a *Help* buffer showing other help.
The *Help* buffer keeps its local variables from one help to the next."
  (when (and (local-variable-p 'nael-infoview--eldoc-was)
             (not (text-property-not-all (point-min) (point-max)
                                         'nael-node nil)))
    (nael-infoview-hover-teardown)
    (unless nael-infoview--eldoc-was
      (eldoc-mode -1))
    (kill-local-variable 'nael-infoview--handle)
    (kill-local-variable 'nael-infoview--info-cache)
    (kill-local-variable 'nael-infoview--tooltip-node)
    (kill-local-variable 'nael-infoview--eldoc-was)))

;; Help runs this hook for each help it shows.
(add-hook 'temp-buffer-window-setup-hook #'nael-infoview--help-cleanup)

(defvar nael-infoview--return nil
  "Where to return when the help window closes, as (HELP . WINDOW).
HELP is the window showing a description, WINDOW the one it was asked
from.")

(defun nael-infoview--return-from-help (_frame)
  "Select the window that asked for a description, after its help closed.
Only if the help window was selected when it closed.  Emacs's own
`quit-window' returns there by itself, but Doom Emacs closes popups with
`delete-window', which selects the most recently used window that is
not dedicated, so never the infoview, a side window.  Run from
`window-selection-change-functions' while a description is shown."
  (pcase-let ((`(,help . ,origin) nael-infoview--return))
    (unless (and (window-live-p help)
                 (eq (window-buffer help) (get-buffer "*Help*")))
      (setq nael-infoview--return nil)
      (remove-hook 'window-selection-change-functions
                   #'nael-infoview--return-from-help)
      (when (and (eq (old-selected-window) help)
                 (window-live-p origin)
                 (not (eq (selected-window) origin)))
        (select-window origin)))))

(defun nael-infoview--return-after-help (origin)
  "Return to ORIGIN once the help window, now showing a description, closes.
ORIGIN is the window that asked for the description."
  (when-let* ((help (get-buffer-window "*Help*" t)))
    ;; A description asked for from another keeps the way back.
    (unless (eq help origin)
      (setq nael-infoview--return (cons help origin))
      (add-hook 'window-selection-change-functions
                #'nael-infoview--return-from-help))))

(defun nael-infoview-describe (&optional pos)
  "Describe the subterm at POS, or point, in the *Help* buffer.
Show it with all its arguments made explicit, its type, its
documentation, and how the tactic at point changes it.  The subterms in
the description can be described in turn."
  (interactive)
  (let* ((node (or (nael-render-node-at (or pos (point)))
                   (user-error "No subterm at point")))
         (handle (nael-infoview--live-handle))
         (owner (nael-rpc-make-owner))
         (origin (selected-window)))
    (nael-rpc-info-to-interactive
     handle (nael-render-node-info node)
     (lambda (info)
       (if (not info)
           (nael-rpc-owner-release owner)
         (let ((page (nael-infoview--page-create
                      :handle handle :info info :owner owner
                      :status (nael-render-node-diff-status node)
                      :source (nael-infoview--handle-marker handle))))
           (push page nael-infoview--pages)
           (nael-infoview--show-page page t)
           (nael-infoview--return-after-help origin))))
     :owner owner :slot 'infoview-describe
     :failure #'nael-infoview--report-failure)))

(defun nael-infoview-describe-at-mouse (event)
  "Describe the subterm clicked on with EVENT in the *Help* buffer."
  (interactive "e")
  (mouse-set-point event)
  (nael-infoview-describe))

(defvar-keymap nael-infoview-node-map
  :doc "Keymap of the subterms in the infoview and in their descriptions."
  "RET" #'nael-infoview-describe
  "<mouse-2>" #'nael-infoview-describe-at-mouse
  "<follow-link>" 'mouse-face
  "<remap> <display-local-help>" #'nael-infoview-describe)

;;;; Definitions:

(defun nael-infoview--xref-backend ()
  "Return the xref backend for the subterm at point, if there is one."
  (and (nael-render-node-at (point)) 'nael-infoview))

(cl-defmethod xref-backend-identifier-at-point
  ((_backend (eql nael-infoview)))
  (when-let* ((node (nael-render-node-at (point))))
    (propertize (truncate-string-to-width
                 (replace-regexp-in-string
                  "[ \n]+" " " (buffer-substring-no-properties
                                (nael-render-node-beg node)
                                (nael-render-node-end node)))
                 60 nil nil "…")
                'nael-node node)))

(cl-defmethod xref-backend-identifier-completion-table
  ((_backend (eql nael-infoview)))
  nil)

(defun nael-infoview--xref-item (link)
  "Return the xref item for LINK, an LSP `LocationLink', in a list.
Return nil if the target file cannot be read."
  (let* ((file (eglot-uri-to-path (plist-get link :targetUri)))
         (range (or (plist-get link :targetSelectionRange)
                    (plist-get link :targetRange)))
         (visiting (find-buffer-visiting file)))
    (cl-flet ((locate ()
                ;; In the target's buffer, so that columns, which LSP
                ;; counts in UTF-16 code units, are converted as Eglot
                ;; does.
                (save-excursion
                  (save-restriction
                    (widen)
                    (goto-char (car (eglot-range-region range)))
                    (xref-make (string-trim (buffer-substring-no-properties
                                             (line-beginning-position)
                                             (line-end-position)))
                               (xref-make-file-location
                                file (line-number-at-pos)
                                (- (point) (line-beginning-position))))))))
      (cond (visiting (list (with-current-buffer visiting (locate))))
            ((file-readable-p file)
             (with-temp-buffer
               (insert-file-contents file)
               (list (locate))))))))

(cl-defmethod xref-backend-definitions
  ((_backend (eql nael-infoview)) identifier)
  (let ((node (or (get-text-property 0 'nael-node identifier)
                  ;; Doom Emacs may pass the text without properties.
                  (nael-render-node-at (point))
                  (user-error "No subterm at point"))))
    (mapcan #'nael-infoview--xref-item
            (condition-case error
                (nael-rpc-call-sync (nael-infoview--live-handle)
                                    "Lean.Widget.getGoToLocation"
                                    (list :kind "definition"
                                          :info (nael-render-node-info node))
                                    :retry nil)
              (nael-rpc-error
               (user-error "%s" (nael-infoview--failure-message
                                 (nth 2 error))))))))

;;;; Doom Emacs:

(defvar +lookup-documentation-functions)

(defun nael-infoview--lookup-documentation (_identifier)
  "Describe the subterm at point, for Doom Emacs' `+lookup/documentation'.
Return `deferred': the description is shown once Lean answers, and
lookup must leave the windows alone until then."
  (when (nael-render-node-at (point))
    (nael-infoview-describe)
    'deferred))

;;;; Setup:

(defun nael-infoview-hover-setup ()
  "Make the subterms shown in the current buffer report on themselves."
  (add-hook 'eldoc-documentation-functions #'nael-infoview-eldoc-function
            nil t)
  (add-hook 'xref-backend-functions #'nael-infoview--xref-backend nil t)
  ;; Only with Doom's lookup module loaded: adding to the hook would
  ;; define it, and the module's own definition would not take effect.
  (when (boundp '+lookup-documentation-functions)
    (add-hook '+lookup-documentation-functions
              #'nael-infoview--lookup-documentation nil t))
  (when (bound-and-true-p global-eldoc-mode)
    (eldoc-mode)))

(defun nael-infoview-hover-teardown ()
  "Undo `nael-infoview-hover-setup' in the current buffer."
  (remove-hook 'eldoc-documentation-functions #'nael-infoview-eldoc-function
               t)
  (remove-hook 'xref-backend-functions #'nael-infoview--xref-backend t)
  (when (boundp '+lookup-documentation-functions)
    (remove-hook '+lookup-documentation-functions
                 #'nael-infoview--lookup-documentation t)))

(defun nael-infoview-hover-reset (handle)
  "Forget what was found out about the nodes shown so far.
HANDLE is the RPC handle of the references shown from now on."
  (setq nael-infoview--handle handle
        nael-infoview--info-cache nil
        nael-infoview--tooltip-node nil))

(provide 'nael-infoview-hover)

;;; nael-infoview-hover.el ends here
