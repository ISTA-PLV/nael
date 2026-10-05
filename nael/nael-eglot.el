;;; nael-eglot.el --- Eglot for Nael  -*- lexical-binding: t; -*-

;; Copyright © 2024 Free Software Foundation, Inc.
;; Copyright © 2025 Mekeor Melire
;; Copyright © 2026 Doug Torrance
;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see LICENSE.GPL3.  Parts are adapted from lean4-mode under the Apache
;; License, Version 2.0; see NOTICE and LICENSE.APACHE2.

;;; Commentary:

;; This file configures `eglot' and `eldoc' for `nael-mode'.  Not only
;; but in particular, it defines the function
;; `nael-eglot-configure-when-managed' which is meant to be locally
;; hooked onto `eglot-managed-mode-hook' in Nael buffers.  When
;; called, it teaches Eglot about some LSP requests information (such
;; as information about the proof goals at point) that are special to
;; the Lean LSP server; and it teaches ElDoc how to display this
;; information.

;;; Code:

(require 'cl-lib)
(require 'eglot)
(require 'seq)

(require 'nael)
(require 'nael-eglot-compat)

(defgroup nael-eglot nil
  "`eglot' and `eldoc' configured for `nael-mode'."
  :group 'nael
  :group 'eglot
  :link '(emacs-library-link :tag "Source Lisp File" "nael-eglot.el")
  :prefix "nael-eglot-")

(defface nael-eglot-eldoc-header
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Face for section-headers of Nael-specific ElDoc documentations."
  :group 'nael-eglot)

(defcustom nael-eglot-echo-goal nil
  "Whether to show the first goal in the echo area."
  :type 'boolean
  :group 'nael-eglot)

(defcustom nael-eldoc-idle-delay 0.05
  "Buffer-local value of `eldoc-idle-delay' in Nael buffers.
It also applies to the infoview and its help buffer.
If nil, leave `eldoc-idle-delay' alone."
  :type '(choice (number :tag "Seconds")
                 (const :tag "Keep global value" nil))
  :group 'nael-eglot)

(defvar nael-eglot-eldoc-fontify-buffer "*Nael Eglot ElDoc Fontify*"
  "Name of buffer that is reused in order to fontify Nael code.")

(defun nael-eglot-eldoc-fontify (string)
  "Apply Nael font-lock rules to STRING."
  (with-current-buffer
      (get-buffer-create nael-eglot-eldoc-fontify-buffer)
    (erase-buffer)
    (insert string)
    (set-syntax-table nael-mode-syntax-table)
    (setq-local font-lock-defaults nael-font-lock-defaults)
    (font-lock-ensure)
    (buffer-string)))

(defun nael-eglot-flush-changes ()
  "Send pending changes of the current buffer to the server now.

Eglot delays `textDocument/didChange' by `eglot-send-changes-idle-time'.
A request about a position must not overtake the edits before it, or the
server answers for text it has not seen yet."
  (nael-eglot-compat-flush))

(defun nael-eglot-server-or-lose ()
  "Return the Eglot server of the current buffer or signal an error."
  (or (eglot-current-server)
      (jsonrpc-error "No current JSON-RPC connection")))

(declare-function nael-infoview-shows-goals-p "nael-infoview" ())

(defun nael-eglot-eldoc-goals-elsewhere-p ()
  "Return non-nil if goals are displayed elsewhere than by ElDoc.
That is the case while the infoview is visible and works."
  (and (fboundp 'nael-infoview-shows-goals-p)
       (nael-infoview-shows-goals-p)))

(defun nael-eglot-eldoc-goal-fn (cb get)
  "Construct ElDoc CB handler function for Lean LSP goal response with GET."
  (lambda (response)
    (apply
     cb
     (if-let*
         ((goals (funcall get response :goals))
          ((not (seq-empty-p goals)))
          (first-goal (seq-first goals))
          (first-goal (nael-eglot-eldoc-fontify first-goal)))
         (list (concat
                ;; Propertize `\n' so that `:extend' works.
                (propertize "Tactic state:\n"
                            'face 'nael-eglot-eldoc-header)
                "\n"
                ;; Avoid rendering `first-goal' twice.
                (replace-regexp-in-string "^" "  " first-goal)
                (seq-mapcat
                 (lambda (goal)
                   (concat "\n\n" (replace-regexp-in-string
                                   "^" "  "
                                   (nael-eglot-eldoc-fontify goal))))
                 (seq-drop goals 1) 'string))
               :echo (if nael-eglot-echo-goal first-goal 'skip))
       (list nil)))))

(defun nael-eglot-eldoc-goal (cb &rest _)
  "ElDoc documentation function for plain goal.

Callback CB is provided to any member of
`eldoc-documentation-functions'.

The request target path is `$/lean/plainGoal' as documented here:
https://leanprover-community.github.io/mathlib4_docs/Lean/Data/Lsp/\
Extra.html#Lean.Lsp.PlainGoal"
  (unless (nael-eglot-eldoc-goals-elsewhere-p)
    (nael-eglot-flush-changes)
    (jsonrpc-async-request
     (nael-eglot-server-or-lose)
     :$/lean/plainGoal
     (nael-eglot-compat-position-params)
     :success-fn (nael-eglot-eldoc-goal-fn cb #'plist-get))
    t))

(defun nael-eglot-eldoc-term-goal-fn (cb get format)
  "Construct ElDoc CB handler function for Lean LSP term-goal response.

Use callback CB, GET to access a slot of the response, and FORMAT as
function to format / render a string, possibly with markup."
  (lambda (response)
    (apply
     cb
     (if-let*
         ((goal (funcall get response :goal))
          ((not (string= "" goal)))
          (doc (funcall format goal)))
         (list (concat
                ;; Propertize `\n' so that `:extend' works.
                (propertize "Expected type:\n"
                            'face 'nael-eglot-eldoc-header)
                "\n" (replace-regexp-in-string "^" "  " doc))
               ;; Don't echo any docstring at all.
               :echo 'skip)
       (list nil)))))

(defun nael-eglot-eldoc-term-goal (cb &rest _)
  "ElDoc documentation function for plain goal.

Callback CB is provided to any member of
`eldoc-documentation-functions'.

The request target path is `$/lean/plainTermGoal' as documented here:
https://leanprover-community.github.io/mathlib4_docs/Lean/Data/Lsp/\
Extra.html#Lean.Lsp.PlainTermGoal"
  (unless (nael-eglot-eldoc-goals-elsewhere-p)
    (nael-eglot-flush-changes)
    (jsonrpc-async-request
     (nael-eglot-server-or-lose)
     :$/lean/plainTermGoal
     (nael-eglot-compat-position-params)
     :success-fn (nael-eglot-eldoc-term-goal-fn
                  cb #'plist-get #'nael-eglot-compat-format-markup))
    t))

;;;###autoload
(defun nael-eglot-configure-when-managed ()
  "Buffer-locally set up ElDoc and Eglot for Nael.

Use ElDoc documentation strategy `compose', apply
`nael-eldoc-idle-delay' and add ElDoc documentation functions for
goal and term goal."
  (interactive)
  (setq-local eldoc-documentation-strategy
              #'eldoc-documentation-compose)
  (when nael-eldoc-idle-delay
    (setq-local eldoc-idle-delay nael-eldoc-idle-delay))
  (add-hook 'eldoc-documentation-functions
            #'nael-eglot-eldoc-goal -90 'local)
  (add-hook 'eldoc-documentation-functions
            #'nael-eglot-eldoc-term-goal -80 'local)
  (add-hook 'change-major-mode-hook
            #'nael-eglot-close-before-mode-change -90 'local))

;; Setting up the major mode of a managed buffer again, as `M-x
;; nael-mode' in a Nael buffer or `normal-mode' do, makes Eglot open
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

(defun nael-eglot-close-before-mode-change ()
  "Tell the server that the document is closed, if Eglot manages it.
For `change-major-mode-hook', where Eglot does not do so itself."
  (when (eglot-managed-p)
    (nael-eglot-compat-did-close)))

;;;###autoload
(defun nael-eglot-configure-when-initialized (_)
  "Buffer-locally correct Eglot's expectations on Lean LSP server.

Since `lake serve' does not output anything, instruct Eglot to not wait
for any output."
  (setq-local eglot-sync-connect
              nil))

(defcustom nael-eglot-contact (list "lake" "serve")
  "Contact for Eglot server program for `nael-mode'.

This is a list (PROGRAM [ARGS...]) or (HOST PORT [TCP-ARGS...]) as
described for CONTACT in `eglot-server-programs'.  Eglot uses it to
start a server of class `nael-eglot-server'."
  :type '(choice (repeat :tag "(PROGRAM [ARGS...])" string)
                 (sexp :tag "Other"))
  :group 'nael-eglot)

(defclass nael-eglot-server (eglot-lsp-server) ()
  :documentation "Eglot server class for the Lean language server.")

(add-to-list 'eglot-server-programs
             (cons 'nael-mode
                   (lambda (&optional _interactive _project)
                     (cons 'nael-eglot-server nael-eglot-contact))))

;;;; Lean extensions of LSP:

(defconst nael-eglot-rpc-wire-format "v1"
  "Encoding of RPC references that Nael asks the server for.
Wire format \"v1\" encodes a reference as {\"__rpcref\": ID}.  Servers
of Lean versions before 4.30 do not know it and use \"v0\", {\"p\": ID}.")

(cl-defmethod eglot-client-capabilities :around ((_server nael-eglot-server))
  "Add the Lean-specific client capabilities to those of Eglot.
Besides the RPC wire format, ask for silent diagnostics, such as the
one that says that a proof is complete; see `nael-eglot-diagnostics'."
  (append (cl-call-next-method)
          (list :lean (list :rpcWireFormat nael-eglot-rpc-wire-format
                            :silentDiagnosticSupport t))))

(defun nael-eglot-uri-buffer (uri)
  "Return the live buffer visiting the file of URI, or nil."
  (find-buffer-visiting (eglot-uri-to-path uri)))

(defvar-local nael-eglot-file-progress nil
  "Regions of the current buffer that the server is still processing.
This is the latest `processing' vector of `$/lean/fileProgress': each
element is a plist with an LSP `:range' and a `:kind', where 1 means
processing and 2 means a fatal error.  An empty vector means done.")

(defvar nael-eglot-file-progress-functions nil
  "Abnormal hook run when the server reports progress on a buffer.
Each function is called in that buffer with the new value of
`nael-eglot-file-progress' as argument.")

(defvar-local nael-eglot-diagnostics nil
  "The diagnostics that the server published last for the current buffer.
This is a vector of LSP diagnostics with Lean's extensions: a diagnostic
may have a `:fullRange' besides its `:range', `:leanTags' (see
`nael-eglot-diagnostic-tag-p'), and `:isSilent'.  Silent diagnostics are
kept here, but Eglot, and so Flymake, never sees them.")

(defvar nael-eglot-diagnostics-functions nil
  "Abnormal hook run when the server publishes diagnostics for a buffer.
Each function is called in that buffer with the vector of LSP
diagnostics as argument, after Eglot has handled them.  The vector
includes the silent diagnostics; see `nael-eglot-diagnostics'.")

(cl-defmethod eglot-handle-notification
  ((_server nael-eglot-server) (_method (eql $/lean/fileProgress))
   &key textDocument processing &allow-other-keys)
  "Record PROCESSING for the buffer of TEXTDOCUMENT and run hooks."
  (when-let* ((buffer (nael-eglot-uri-buffer
                       (plist-get textDocument :uri))))
    (with-current-buffer buffer
      (setq nael-eglot-file-progress processing)
      (run-hook-with-args 'nael-eglot-file-progress-functions
                          processing))))

(cl-defmethod eglot-handle-notification :around
  ((server nael-eglot-server)
   (method (eql textDocument/publishDiagnostics))
   &rest args &key uri diagnostics &allow-other-keys)
  "Hand the DIAGNOSTICS for URI to Eglot without the silent ones.
Silent diagnostics would be shown as notes otherwise.  Keep all in
`nael-eglot-diagnostics' and run `nael-eglot-diagnostics-functions'.
SERVER, METHOD and ARGS are passed on."
  (apply #'cl-call-next-method server method
         (plist-put (copy-sequence args) :diagnostics
                    (seq-into (seq-remove (lambda (diagnostic)
                                            (eq (plist-get diagnostic :isSilent)
                                                t))
                                          diagnostics)
                              'vector)))
  (when-let* ((buffer (nael-eglot-uri-buffer uri)))
    (with-current-buffer buffer
      (setq nael-eglot-diagnostics diagnostics)
      (run-hook-with-args 'nael-eglot-diagnostics-functions
                          diagnostics))))

(defconst nael-eglot-diagnostic-tags
  '((unsolved-goals . 1) (goals-accomplished . 2))
  "Lean's tags of diagnostics, as in the `:leanTags' of a diagnostic.")

(defun nael-eglot-diagnostic-tag-p (diagnostic tag)
  "Return non-nil if DIAGNOSTIC has TAG, a key of `nael-eglot-diagnostic-tags'."
  (seq-contains-p (plist-get diagnostic :leanTags)
                  (alist-get tag nael-eglot-diagnostic-tags)))

(defun nael-eglot-diagnostic-lines (diagnostic &optional full)
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

(defun nael-eglot-goals-accomplished-p (&optional line)
  "Return non-nil if LINE lies in a proof that the server found complete.
LINE is 0-based and defaults to the line of point.  The answer comes
from the silent diagnostics in `nael-eglot-diagnostics'."
  (let ((line (or line (1- (line-number-at-pos nil t)))))
    (seq-some (lambda (diagnostic)
                (and (nael-eglot-diagnostic-tag-p diagnostic
                                                  'goals-accomplished)
                     (pcase-let ((`(,first . ,last)
                                  (nael-eglot-diagnostic-lines diagnostic t)))
                       (<= first line last))))
              nael-eglot-diagnostics)))

(defun nael-eglot-processing-line-p (&optional line)
  "Return non-nil if the server is still processing LINE.
LINE is 0-based as in LSP and defaults to the line of point.  The
answer comes from the latest `nael-eglot-file-progress'; regions where
processing stopped with a fatal error do not count."
  (let ((line (or line (1- (line-number-at-pos nil t)))))
    (seq-some (lambda (item)
                (let ((range (plist-get item :range)))
                  (and (memql (plist-get item :kind) '(nil 1))
                       (<= (plist-get (plist-get range :start) :line)
                           line
                           (plist-get (plist-get range :end) :line)))))
              nael-eglot-file-progress)))

;;;; Building dependencies:

;; Adapted from `lean4-eglot.el' of lean4-mode
;; <https://github.com/d-torrance/lean4-mode> at commit 0c2216dd43,
;; Copyright © 2026 Doug Torrance, licensed under the Apache License,
;; Version 2.0.  Changed: names and docstrings; the server class; the
;; restart command checks `eglot-managed-p' instead of
;; `eglot-current-server'.

(defcustom nael-eglot-build-dependencies nil
  "Whether opening a file makes the server build its imports.

If nil, opening a file never starts a build; when imports are out of
date, the server says so, and `nael-eglot-restart-file' builds them.
If non-nil, every opening builds whatever is out of date, which may
take long.  This corresponds to the setting
`lean4.automaticallyBuildDependencies' of the Lean extension for
VS Code, which is off by default as well."
  :type 'boolean
  :group 'nael-eglot)

(defvar nael-eglot--build-dependencies-once nil
  "Non-nil while `nael-eglot-restart-file' reopens a file.")

(defun nael-eglot-dependency-build-mode ()
  "Return the `dependencyBuildMode' for a `textDocument/didOpen'.

The server treats a missing mode as \"always\", so a mode is sent with
every opening.  Unlike \"always\", \"once\" does not build again when
the file worker restarts after a crash or an import change."
  (cond (nael-eglot-build-dependencies "always")
        (nael-eglot--build-dependencies-once "once")
        (t "never")))

(cl-defmethod jsonrpc-connection-send :around
  ((server nael-eglot-server) &rest args &key method params
   &allow-other-keys)
  "Add `dependencyBuildMode' to `textDocument/didOpen' sent to SERVER.

Eglot builds the parameters of `textDocument/didOpen' itself, so the
field is added here.  Pass ARGS with METHOD and PARAMS on."
  (if (eq method :textDocument/didOpen)
      (apply #'cl-call-next-method server
             (plist-put (copy-sequence args) :params
                        (append params
                                (list :dependencyBuildMode
                                      (nael-eglot-dependency-build-mode)))))
    (apply #'cl-call-next-method server args)))

;;;###autoload
(defun nael-eglot-restart-file ()
  "Restart the server's processing of the current file.

Close and reopen the file on the server, which builds imports that are
out of date once.  Use this after changing a file that the current file
imports."
  (interactive)
  (unless (eglot-managed-p)
    (user-error "Buffer is not managed by Eglot"))
  (nael-eglot-compat-did-close)
  (let ((nael-eglot--build-dependencies-once t))
    (nael-eglot-compat-did-open)))

(provide 'nael-eglot)

;;; nael-eglot.el ends here
