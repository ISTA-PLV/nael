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

(require 'eglot)

(require 'nael)

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
  (eglot--signal-textDocument/didChange))

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
  (nael-eglot-flush-changes)
  (jsonrpc-async-request
   (eglot--current-server-or-lose)
   :$/lean/plainGoal
   (eglot--TextDocumentPositionParams)
   :success-fn (nael-eglot-eldoc-goal-fn cb #'plist-get))
  t)

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
  (nael-eglot-flush-changes)
  (jsonrpc-async-request
   (eglot--current-server-or-lose)
   :$/lean/plainTermGoal
   (eglot--TextDocumentPositionParams)
   :success-fn (nael-eglot-eldoc-term-goal-fn
                cb #'plist-get #'eglot--format-markup))
  t)

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
            #'nael-eglot-eldoc-term-goal -80 'local))

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
  (eglot--signal-textDocument/didClose)
  (let ((nael-eglot--build-dependencies-once t))
    (eglot--signal-textDocument/didOpen)))

(provide 'nael-eglot)

;;; nael-eglot.el ends here
