;;; lena-eglot-compat.el --- Eglot internals used by Lena  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see LICENSE.GPL3.

;;; Commentary:

;; Lena needs a few functions that Eglot does not export.  Every use of
;; such an internal goes through this file, so that a change in Eglot
;; breaks one place only, and so that a missing internal is reported
;; when Lena is loaded rather than when it is first called.

;;; Code:

(require 'eglot)

(defconst lena-eglot-compat-internals
  '(eglot--capabilities
    eglot--format-markup
    eglot--signal-textDocument/didChange
    eglot--signal-textDocument/didClose
    eglot--signal-textDocument/didOpen
    eglot--TextDocumentPositionParams
    eglot--update-hints-1
    jsonrpc--next-request-id)
  "Internal functions of Eglot and jsonrpc that Lena calls.")

(when-let* ((missing (seq-remove #'fboundp lena-eglot-compat-internals)))
  (display-warning
   'lena
   (format "This version of Eglot lacks functions that Lena uses: %s"
           (mapconcat #'symbol-name missing ", "))
   :warning))

(defun lena-eglot-compat-flush ()
  "Send pending changes of the current buffer to its server."
  (eglot--signal-textDocument/didChange))

(defun lena-eglot-compat-did-close ()
  "Send `textDocument/didClose' for the current buffer."
  (eglot--signal-textDocument/didClose))

(defun lena-eglot-compat-did-open ()
  "Send `textDocument/didOpen' for the current buffer."
  (eglot--signal-textDocument/didOpen))

(defun lena-eglot-compat-position-params (&optional pos)
  "Return LSP `TextDocumentPositionParams' for POS, or point.
The column is counted in UTF-16 code units, as LSP requires."
  (save-excursion
    (when pos (goto-char pos))
    (eglot--TextDocumentPositionParams)))

(defun lena-eglot-compat-format-markup (markup)
  "Render MARKUP, an LSP `MarkupContent' or string, as Eglot does."
  (eglot--format-markup markup))

(defun lena-eglot-compat-capabilities (server)
  "Return the capabilities that SERVER announced on initialization."
  (eglot--capabilities server))

(defun lena-eglot-compat-last-request-id (connection)
  "Return the id of the request just sent through CONNECTION.
Only for jsonrpc 1.0.25 (Emacs 30), whose `jsonrpc-async-request'
returns nil instead of the id; remove this once Lena needs a later
jsonrpc."
  (jsonrpc--next-request-id connection))

(defvar eglot--semtok-state)

(defun lena-eglot-compat-forget-semantic-tokens ()
  "Make Eglot ask for the semantic tokens of the current buffer again.
Eglot keeps them for a document version.  It has them since 1.19."
  (setq eglot--semtok-state nil))

(defun lena-eglot-compat-update-hints (from to)
  "Ask for the inlay hints between FROM and TO, and show them.
Eglot asks for them only when it fontifies the text."
  (eglot--update-hints-1 from to))

(provide 'lena-eglot-compat)

;;; lena-eglot-compat.el ends here
