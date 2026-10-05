;;; nael-eglot-compat.el --- Eglot internals used by Nael  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see LICENSE.GPL3.

;;; Commentary:

;; Nael needs a few functions that Eglot does not export.  Every use of
;; such an internal goes through this file, so that a change in Eglot
;; breaks one place only, and so that a missing internal is reported
;; when Nael is loaded rather than when it is first called.

;;; Code:

(require 'eglot)

(defconst nael-eglot-compat-internals
  '(eglot--capabilities
    eglot--format-markup
    eglot--signal-textDocument/didChange
    eglot--signal-textDocument/didClose
    eglot--signal-textDocument/didOpen
    eglot--TextDocumentPositionParams)
  "Internal functions of Eglot that Nael calls.")

(when-let* ((missing (seq-remove #'fboundp nael-eglot-compat-internals)))
  (display-warning
   'nael
   (format "This version of Eglot lacks functions that Nael uses: %s"
           (mapconcat #'symbol-name missing ", "))
   :warning))

(defun nael-eglot-compat-flush ()
  "Send pending changes of the current buffer to its server."
  (eglot--signal-textDocument/didChange))

(defun nael-eglot-compat-did-close ()
  "Send `textDocument/didClose' for the current buffer."
  (eglot--signal-textDocument/didClose))

(defun nael-eglot-compat-did-open ()
  "Send `textDocument/didOpen' for the current buffer."
  (eglot--signal-textDocument/didOpen))

(defun nael-eglot-compat-position-params (&optional pos)
  "Return LSP `TextDocumentPositionParams' for POS, or point.
The column is counted in UTF-16 code units, as LSP requires."
  (save-excursion
    (when pos (goto-char pos))
    (eglot--TextDocumentPositionParams)))

(defun nael-eglot-compat-format-markup (markup)
  "Render MARKUP, an LSP `MarkupContent' or string, as Eglot does."
  (eglot--format-markup markup))

(defun nael-eglot-compat-capabilities (server)
  "Return the capabilities that SERVER announced on initialization."
  (eglot--capabilities server))

(defvar eglot--semtok-state)

(defun nael-eglot-compat-forget-semantic-tokens ()
  "Make Eglot ask for the semantic tokens of the current buffer again.
Eglot keeps them for a document version.  It has them since 1.19."
  (setq eglot--semtok-state nil))

(provide 'nael-eglot-compat)

;;; nael-eglot-compat.el ends here
