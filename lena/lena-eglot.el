;;; lena-eglot.el --- Eglot for Lena  -*- lexical-binding: t; -*-

;; Copyright © 2024 Free Software Foundation, Inc.
;; Copyright © 2025 Mekeor Melire
;; Copyright © 2026 Doug Torrance
;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see LICENSE.GPL3.  Parts are adapted from lean4-mode under the Apache
;; License, Version 2.0; see NOTICE and LICENSE.APACHE2.

;;; Commentary:

;; This file configures `eglot' and `eldoc' for `lena-mode'.  Not only
;; but in particular, it defines the function
;; `lena-eglot-configure-when-managed' which is meant to be locally
;; hooked onto `eglot-managed-mode-hook' in Lena buffers.  When
;; called, it teaches Eglot about some LSP requests information (such
;; as information about the proof goals at point) that are special to
;; the Lean LSP server; and it teaches ElDoc how to display this
;; information.

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'seq)

(require 'lena)
(require 'lena-eglot-compat)

(declare-function lena-margin-update "lena-margin" ())

(defgroup lena-eglot nil
  "`eglot' and `eldoc' configured for `lena-mode'."
  :group 'lena
  :group 'eglot
  :link '(emacs-library-link :tag "Source Lisp File" "lena-eglot.el")
  :prefix "lena-eglot-")

(defface lena-eglot-eldoc-header
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Face for section-headers of Lena-specific ElDoc documentations."
  :group 'lena-eglot)

(defcustom lena-eglot-echo-goal nil
  "Whether to show the first goal in the echo area."
  :type 'boolean
  :group 'lena-eglot)

(defcustom lena-eldoc-idle-delay 0.05
  "Buffer-local value of `eldoc-idle-delay' in Lena buffers.
It also applies to the infoview and its help buffer.
If nil, leave `eldoc-idle-delay' alone."
  :type '(choice (number :tag "Seconds")
                 (const :tag "Keep global value" nil))
  :group 'lena-eglot)

(defvar lena-eglot-eldoc-fontify-buffer "*Lena Eglot ElDoc Fontify*"
  "Name of buffer that is reused in order to fontify Lena code.")

(defun lena-eglot-eldoc-fontify (string)
  "Apply Lena font-lock rules to STRING."
  (with-current-buffer
      (get-buffer-create lena-eglot-eldoc-fontify-buffer)
    (erase-buffer)
    (insert string)
    (set-syntax-table lena-mode-syntax-table)
    (setq-local font-lock-defaults lena-font-lock-defaults)
    (font-lock-ensure)
    (buffer-string)))

(defun lena-eglot-flush-changes ()
  "Send pending changes of the current buffer to the server now.

Eglot delays `textDocument/didChange' by `eglot-send-changes-idle-time'.
A request about a position must not overtake the edits before it, or the
server answers for text it has not seen yet."
  (lena-eglot-compat-flush))

(defun lena-eglot-server-or-lose ()
  "Return the Eglot server of the current buffer or signal an error."
  (or (eglot-current-server)
      (jsonrpc-error "No current JSON-RPC connection")))

(declare-function lena-infoview-shows-goals-p "lena-infoview" ())

(defun lena-eglot-eldoc-goals-elsewhere-p ()
  "Return non-nil if goals are displayed elsewhere than by ElDoc.
That is the case while the infoview is visible and works."
  (and (fboundp 'lena-infoview-shows-goals-p)
       (lena-infoview-shows-goals-p)))

(defun lena-eglot-eldoc-goal-fn (cb)
  "Construct ElDoc CB handler function for Lean LSP goal response."
  (lambda (response)
    (apply
     cb
     (if-let*
         ((goals (plist-get response :goals))
          ((not (seq-empty-p goals)))
          (first-goal (seq-first goals))
          (first-goal (lena-eglot-eldoc-fontify first-goal)))
         (list (concat
                ;; Propertize `\n' so that `:extend' works.
                (propertize "Tactic state:\n"
                            'face 'lena-eglot-eldoc-header)
                "\n"
                ;; Avoid rendering `first-goal' twice.
                (replace-regexp-in-string "^" "  " first-goal)
                (seq-mapcat
                 (lambda (goal)
                   (concat "\n\n" (replace-regexp-in-string
                                   "^" "  "
                                   (lena-eglot-eldoc-fontify goal))))
                 (seq-drop goals 1) 'string))
               :echo (if lena-eglot-echo-goal first-goal 'skip))
       (list nil)))))

(defun lena-eglot-eldoc-goal (cb &rest _)
  "ElDoc documentation function for plain goal.

Callback CB is provided to any member of
`eldoc-documentation-functions'.

The request target path is `$/lean/plainGoal' as documented here:
https://leanprover-community.github.io/mathlib4_docs/Lean/Data/Lsp/\
Extra.html#Lean.Lsp.PlainGoal"
  (unless (lena-eglot-eldoc-goals-elsewhere-p)
    (lena-eglot-flush-changes)
    (jsonrpc-async-request
     (lena-eglot-server-or-lose)
     :$/lean/plainGoal
     (lena-eglot-compat-position-params)
     :success-fn (lena-eglot-eldoc-goal-fn cb))
    t))

(defun lena-eglot-eldoc-term-goal-fn (cb)
  "Construct ElDoc CB handler function for Lean LSP term-goal response."
  (lambda (response)
    (apply
     cb
     (if-let*
         ((goal (plist-get response :goal))
          ((not (string= "" goal)))
          (doc (lena-eglot-compat-format-markup goal)))
         (list (concat
                ;; Propertize `\n' so that `:extend' works.
                (propertize "Expected type:\n"
                            'face 'lena-eglot-eldoc-header)
                "\n" (replace-regexp-in-string "^" "  " doc))
               ;; Don't echo any docstring at all.
               :echo 'skip)
       (list nil)))))

(defun lena-eglot-eldoc-term-goal (cb &rest _)
  "ElDoc documentation function for plain goal.

Callback CB is provided to any member of
`eldoc-documentation-functions'.

The request target path is `$/lean/plainTermGoal' as documented here:
https://leanprover-community.github.io/mathlib4_docs/Lean/Data/Lsp/\
Extra.html#Lean.Lsp.PlainTermGoal"
  (unless (lena-eglot-eldoc-goals-elsewhere-p)
    (lena-eglot-flush-changes)
    (jsonrpc-async-request
     (lena-eglot-server-or-lose)
     :$/lean/plainTermGoal
     (lena-eglot-compat-position-params)
     :success-fn (lena-eglot-eldoc-term-goal-fn cb))
    t))

;;;###autoload
(defun lena-eglot-configure-when-managed ()
  "Buffer-locally set up ElDoc and Eglot for Lena.

Use ElDoc documentation strategy `compose', apply
`lena-eldoc-idle-delay' and add ElDoc documentation functions for
goal and term goal.  Turn `lena-margin-mode' on or off, as option
`lena-margin' says and as Eglot manages the buffer or not."
  (interactive)
  (require 'lena-margin)
  (lena-margin-update)
  (setq-local eldoc-documentation-strategy
              #'eldoc-documentation-compose)
  (when lena-eldoc-idle-delay
    (setq-local eldoc-idle-delay lena-eldoc-idle-delay))
  (add-hook 'eldoc-documentation-functions
            #'lena-eglot-eldoc-goal -90 'local)
  (add-hook 'eldoc-documentation-functions
            #'lena-eglot-eldoc-term-goal -80 'local)
  (add-hook 'change-major-mode-hook
            #'lena-eglot-close-before-mode-change -90 'local))

;; Setting up the major mode of a managed buffer again, as `M-x
;; lena-mode' in a Lena buffer or `normal-mode' do, makes Eglot open
;; the document a second time without closing it first: Eglot only
;; turns itself off in the buffer on `change-major-mode-hook', and on
;; `after-change-major-mode-hook' it manages the buffer again and sends
;; textDocument/didOpen.  LSP forbids opening an open document again.
;; The Lean server then starts a second worker for the file and never
;; stops the first, which keeps its memory until the server exits.
;; Eglot itself closes documents only when their buffer is killed or
;; reverted.  So close the document before the major mode changes; the
;; didOpen that follows is then in order.  As of Eglot 1.24 (2026), this
;; is not reported upstream yet; remove this once Eglot closes the
;; document itself.

(defun lena-eglot-close-before-mode-change ()
  "Tell the server that the document is closed, if Eglot manages it.
For `change-major-mode-hook', where Eglot does not do so itself.
When the buffer is reverted, Eglot has closed it already."
  ;; Emacs 31 renames the variable to `revert-buffer-in-progress'.
  (when (and (eglot-managed-p)
             (not (with-no-warnings revert-buffer-in-progress-p)))
    (lena-eglot-compat-did-close)))

;;;###autoload
(defun lena-eglot-configure-when-initialized (_)
  "Buffer-locally correct Eglot's expectations on Lean LSP server.

Since `lake serve' does not output anything, instruct Eglot to not wait
for any output."
  (setq-local eglot-sync-connect
              nil))

(defcustom lena-eglot-contact (list "lake" "serve")
  "Contact for Eglot server program for `lena-mode'.

This is a list (PROGRAM [ARGS...]) or (HOST PORT [TCP-ARGS...]) as
described for CONTACT in `eglot-server-programs'.  Eglot uses it to
start a server whose class is the symbol `lena-eglot-server'."
  :type '(choice (repeat :tag "(PROGRAM [ARGS...])" string)
                 (sexp :tag "Other"))
  :group 'lena-eglot)

(defclass lena-eglot-server (eglot-lsp-server) ()
  :documentation "Eglot server class for the Lean language server.")

(add-to-list 'eglot-server-programs
             (cons 'lena-mode
                   (lambda (&optional _interactive _project)
                     (cons 'lena-eglot-server lena-eglot-contact))))

;;;; Lean extensions of LSP:

(defconst lena-eglot-rpc-wire-format "v1"
  "Encoding of RPC references that Lena asks the server for.
Wire format \"v1\" encodes a reference as {\"__rpcref\": ID}.  Servers
of Lean versions before 4.30 do not know it and use \"v0\", {\"p\": ID}.")

(cl-defmethod eglot-client-capabilities :around ((_server lena-eglot-server))
  "Add the Lean-specific client capabilities to those of Eglot.
Besides the RPC wire format, ask for silent diagnostics, such as the
one that says that a proof is complete; see `lena-eglot-diagnostics'."
  (append (cl-call-next-method)
          (list :lean (list :rpcWireFormat lena-eglot-rpc-wire-format
                            :silentDiagnosticSupport t))))

;;;; Widgets in messages:

(defcustom lena-eglot-widgets 'auto
  "Whether the server sends messages with their terms, goals and traces.
With them, the infoview shows the messages at point interactively, as
it shows goals; this needs Lean 4.30 or later.  The cost is that the
text of a trace that Flymake shows is only \"(trace)\".  If `auto',
ask for them when the `lean-toolchain' file of the project names Lean
4.30 or later, or a nightly.  Takes effect when the server starts.
This is the option `hasWidgets' of the Lean server."
  :type '(choice (const :tag "If the toolchain is Lean 4.30 or later" auto)
                 (const :tag "Always" t)
                 (const :tag "Never" nil))
  :group 'lena-eglot)

(defun lena-eglot-lean-version (directory)
  "Return the Lean version that the `lean-toolchain' for DIRECTORY names.
The file is looked for in DIRECTORY and its parents.  The value is a
list (MAJOR MINOR), the symbol `nightly', or nil if there is no such
file or it names something else."
  (when-let* ((found (locate-dominating-file directory "lean-toolchain"))
              (file (expand-file-name "lean-toolchain" found)))
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (cond ((looking-at
                (rx (? (* (not (any ":\n"))) ":") (? "v")
                    (group (+ digit)) "." (group (+ digit))))
               (list (string-to-number (match-string 1))
                     (string-to-number (match-string 2))))
              ((looking-at (rx (* (not (any "\n"))) "nightly"))
               'nightly))))))

(defun lena-eglot-widgets-p (directory)
  "Return non-nil if the server for DIRECTORY should send widgets.
See `lena-eglot-widgets'."
  (if (eq lena-eglot-widgets 'auto)
      (let ((version (lena-eglot-lean-version directory)))
        (or (eq version 'nightly)
            (and version (version-list-<= '(4 30) version))))
    lena-eglot-widgets))

(cl-defmethod eglot-initialization-options :around
  ((_server lena-eglot-server))
  "Ask for widgets in messages, as option `lena-eglot-widgets' says.
Eglot calls this in the root directory of the project."
  (let ((options (cl-call-next-method)))
    (cond ((not (lena-eglot-widgets-p default-directory)) options)
          ((and (hash-table-p options) (> (hash-table-count options) 0))
           (let ((copy (copy-hash-table options)))
             (puthash "hasWidgets" t copy)
             copy))
          ((consp options) (plist-put (copy-sequence options) :hasWidgets t))
          (t (list :hasWidgets t)))))

(defun lena-eglot-uri-buffer (uri)
  "Return the live buffer visiting the file of URI, or nil."
  (find-buffer-visiting (eglot-uri-to-path uri)))

(defvar-local lena-eglot-file-progress nil
  "Regions of the current buffer that the server is still processing.
This is the latest `processing' vector of `$/lean/fileProgress': each
element is a plist with an LSP `:range' and a `:kind', where 1 means
processing and 2 means a fatal error.  An empty vector means done.")

(defvar lena-eglot-file-progress-functions nil
  "Abnormal hook run when the server reports progress on a buffer.
Each function is called in that buffer with the new value of
`lena-eglot-file-progress' as argument.")

(defvar-local lena-eglot-diagnostics nil
  "The diagnostics that the server published last for the current buffer.
This is a vector of LSP diagnostics with Lean's extensions: a diagnostic
may have a `:fullRange' besides its `:range', `:leanTags' (see
`lena-eglot-diagnostic-tag-p'), and `:isSilent'.  Silent diagnostics are
kept here, but Eglot, and so Flymake, never sees them.")

(defvar lena-eglot-diagnostics-functions nil
  "Abnormal hook run when the server publishes diagnostics for a buffer.
Each function is called in that buffer with the vector of LSP
diagnostics as argument, after Eglot has handled them.  The vector
includes the silent diagnostics; see `lena-eglot-diagnostics'.")

(cl-defmethod eglot-handle-notification
  ((_server lena-eglot-server) (_method (eql $/lean/fileProgress))
   &key textDocument processing &allow-other-keys)
  "Record PROCESSING for the buffer of TEXTDOCUMENT and run hooks."
  (when-let* ((buffer (lena-eglot-uri-buffer
                       (plist-get textDocument :uri))))
    (with-current-buffer buffer
      (setq lena-eglot-file-progress processing)
      (run-hook-with-args 'lena-eglot-file-progress-functions
                          processing))))

(cl-defmethod eglot-handle-notification :around
  ((server lena-eglot-server)
   (method (eql textDocument/publishDiagnostics))
   &rest args &key uri diagnostics &allow-other-keys)
  "Hand the DIAGNOSTICS for URI to Eglot without the silent ones.
Silent diagnostics would be shown as notes otherwise.  Keep all in
`lena-eglot-diagnostics' and run `lena-eglot-diagnostics-functions'.
SERVER, METHOD and ARGS are passed on."
  (apply #'cl-call-next-method server method
         (plist-put (copy-sequence args) :diagnostics
                    (seq-into (seq-remove (lambda (diagnostic)
                                            (eq (plist-get diagnostic :isSilent)
                                                t))
                                          diagnostics)
                              'vector)))
  (when-let* ((buffer (lena-eglot-uri-buffer uri)))
    (with-current-buffer buffer
      (setq lena-eglot-diagnostics diagnostics)
      (run-hook-with-args 'lena-eglot-diagnostics-functions
                          diagnostics))))

(defconst lena-eglot-diagnostic-tags
  '((unsolved-goals . 1) (goals-accomplished . 2))
  "Lean's tags of diagnostics, as in the `:leanTags' of a diagnostic.")

(defun lena-eglot-diagnostic-tag-p (diagnostic tag)
  "Return non-nil if DIAGNOSTIC has TAG, a key of `lena-eglot-diagnostic-tags'."
  (seq-contains-p (plist-get diagnostic :leanTags)
                  (alist-get tag lena-eglot-diagnostic-tags)))

(defun lena-eglot-diagnostic-lines (diagnostic &optional full)
  "Return the first and last line of DIAGNOSTIC, 0-based, as a cons.
The server shortens a diagnostic's `:range' to its first line; with
FULL non-nil, use its `:fullRange' instead, when it has one.  A range
that ends at the start of a line ends on the line before."
  (let* ((range (or (and full (plist-get diagnostic :fullRange))
                    (plist-get diagnostic :range)))
         (start (plist-get range :start))
         (end (plist-get range :end))
         (first (plist-get start :line))
         (last (plist-get end :line)))
    (when (and (> last first) (zerop (plist-get end :character)))
      (cl-decf last))
    (cons first last)))

(defun lena-eglot-goals-accomplished-p (&optional line)
  "Return non-nil if LINE lies in a proof that the server found complete.
LINE is 0-based and defaults to the line of point.  The answer comes
from the silent diagnostics in `lena-eglot-diagnostics'."
  (let ((line (or line (1- (line-number-at-pos nil t)))))
    (seq-some (lambda (diagnostic)
                (and (lena-eglot-diagnostic-tag-p diagnostic
                                                  'goals-accomplished)
                     (pcase-let ((`(,first . ,last)
                                  (lena-eglot-diagnostic-lines diagnostic t)))
                       (<= first line last))))
              lena-eglot-diagnostics)))

(defvar eglot-semantic-tokens-mode)

;; Lean sends the semantic tokens of the part of a file that it has
;; processed so far, and asks for them again with
;; `workspace/semanticTokens/refresh' until it is done, as described in
;; Lean's `Lean/Server/ProtocolOverview.lean'.  Eglot ignores that
;; request on purpose (as of 1.24) and keeps the tokens of a document
;; version, so a file whose tokens were asked for too early, such as
;; one opened while Lean is busy, lacks them until it is edited.  Nor
;; does Eglot know that `lena-eglot-restart-file' makes its tokens
;; outdated: the file is reopened with the same version.  Remove the
;; handler once Eglot asks again itself; the restart still needs this.

(defun lena-eglot-refresh-semantic-tokens ()
  "Make Eglot ask for the semantic tokens of the current buffer again.
Only where Eglot shows them, with `eglot-semantic-tokens-mode'."
  (when (bound-and-true-p eglot-semantic-tokens-mode)
    (lena-eglot-compat-forget-semantic-tokens)
    (save-restriction
      (widen)
      (font-lock-flush))))

(cl-defmethod eglot-handle-request
  ((server lena-eglot-server) (_method (eql workspace/semanticTokens/refresh)))
  "Ask SERVER again for the semantic tokens of its buffers, as Lean expects."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (eglot-managed-p) (eq (eglot-current-server) server))
        (lena-eglot-refresh-semantic-tokens))))
  nil)

;; Lean streams inlay hints the same way, such as the auto-bound
;; implicit {B} of `def a (b : B) := b': it reports those of the part of
;; the file processed so far, and asks for them again with
;; `workspace/inlayHint/refresh' until it is done.  Eglot does not handle
;; that request (as of 1.24) and asks for hints only when it fontifies,
;; so the hints of a line just written stayed missing until the line
;; was fontified again.

(defvar eglot-inlay-hints-mode)

(defun lena-eglot-refresh-inlay-hints ()
  "Ask again for the inlay hints of the current buffer, where it is shown.
Only where Eglot shows them, with `eglot-inlay-hints-mode'."
  (when-let* (((bound-and-true-p eglot-inlay-hints-mode))
              (windows (get-buffer-window-list nil nil t)))
    (lena-eglot-compat-update-hints
     (apply #'min (mapcar #'window-start windows))
     (apply #'max (mapcar (lambda (window) (window-end window t))
                          windows)))))

(cl-defmethod eglot-handle-request
  ((server lena-eglot-server) (_method (eql workspace/inlayHint/refresh)))
  "Ask SERVER again for the inlay hints of its buffers, as Lean expects."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (eglot-managed-p) (eq (eglot-current-server) server))
        (lena-eglot-refresh-inlay-hints))))
  nil)

(defun lena-eglot-processing-line-p (&optional line)
  "Return non-nil if the server is still processing LINE.
LINE is 0-based as in LSP and defaults to the line of point.  The
answer comes from the latest `lena-eglot-file-progress'; regions where
processing stopped with a fatal error do not count."
  (let ((line (or line (1- (line-number-at-pos nil t)))))
    (seq-some (lambda (item)
                (let ((range (plist-get item :range)))
                  (and (memql (plist-get item :kind) '(nil 1))
                       (<= (plist-get (plist-get range :start) :line)
                           line
                           (plist-get (plist-get range :end) :line)))))
              lena-eglot-file-progress)))

;;;; Building dependencies:

;; Adapted from `lean4-eglot.el' of lean4-mode
;; <https://github.com/d-torrance/lean4-mode> at commit 0c2216dd43,
;; Copyright © 2026 Doug Torrance, licensed under the Apache License,
;; Version 2.0.  Changed: names and docstrings; the server class; the
;; restart command checks `eglot-managed-p' instead of
;; `eglot-current-server'.

(defcustom lena-eglot-build-dependencies nil
  "Whether opening a file makes the server build its imports.

If nil, opening a file never starts a build; when imports are out of
date, the server says so, and `lena-eglot-restart-file' builds them.
If non-nil, every opening builds whatever is out of date, which may
take long.  This corresponds to the setting
`lean4.automaticallyBuildDependencies' of the Lean extension for
VS Code, which is off by default as well."
  :type 'boolean
  :group 'lena-eglot)

(defvar lena-eglot--build-dependencies-once nil
  "Non-nil while `lena-eglot-restart-file' reopens a file.")

(defun lena-eglot-dependency-build-mode ()
  "Return the `dependencyBuildMode' for a `textDocument/didOpen'.

The server treats a missing mode as \"always\", so a mode is sent with
every opening.  Unlike \"always\", \"once\" does not build again when
the file worker restarts after a crash or an import change."
  (cond (lena-eglot-build-dependencies "always")
        (lena-eglot--build-dependencies-once "once")
        (t "never")))

(cl-defmethod jsonrpc-connection-send :around
  ((server lena-eglot-server) &rest args &key method params
   &allow-other-keys)
  "Add `dependencyBuildMode' to `textDocument/didOpen' sent to SERVER.

Eglot builds the parameters of `textDocument/didOpen' itself, so the
field is added here.  Pass ARGS with METHOD and PARAMS on."
  (if (eq method :textDocument/didOpen)
      (apply #'cl-call-next-method server
             (plist-put (copy-sequence args) :params
                        (append params
                                (list :dependencyBuildMode
                                      (lena-eglot-dependency-build-mode)))))
    (apply #'cl-call-next-method server args)))

;;;###autoload
(defun lena-eglot-restart-file ()
  "Restart the server's processing of the current file.

Close and reopen the file on the server, which builds imports that are
out of date once.  Use this after changing a file that the current file
imports."
  (interactive)
  (unless (eglot-managed-p)
    (user-error "Buffer is not managed by Eglot"))
  (lena-eglot-compat-did-close)
  (let ((lena-eglot--build-dependencies-once t))
    (lena-eglot-compat-did-open))
  (lena-eglot-refresh-semantic-tokens))

(provide 'lena-eglot)

;;; lena-eglot.el ends here
