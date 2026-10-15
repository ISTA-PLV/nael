;;; lena-eglot-live.el --- Tests for lena-eglot against Lean  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see lena/LICENSE.GPL3.

;;; Commentary:

;; Tests for Lena's Eglot setup against a real Lean server on the Lake
;; project in `test/lean/'.  They reuse the setup of `lena-rpc-live'.
;;
;; Run with: emacs -Q --batch -l build.el test-live

;;; Code:

(require 'ert)
(require 'lena-rpc-live)

(defun lena-eglot-live-reopen (function)
  "Call FUNCTION in the live buffer and check that it reopens the document.
The document must be closed once and opened once, in that order, and
the server must work on it again."
  (with-current-buffer (lena-rpc-live-buffer)
    (let (sent)
      (cl-letf* ((notify (symbol-function 'jsonrpc-notify))
                 ((symbol-function 'jsonrpc-notify)
                  (lambda (connection method params)
                    (when (memq method '(:textDocument/didOpen
                                         :textDocument/didClose))
                      (push method sent))
                    (funcall notify connection method params))))
        (funcall function)
        (should (lena-rpc-live-wait #'eglot-managed-p 30)))
      (should (equal (reverse sent)
                     '(:textDocument/didClose :textDocument/didOpen))))
    (setq lena-eglot-file-progress nil)
    (should (lena-rpc-live-wait #'lena-rpc-live-processed-p 60))))

(ert-deftest lena-eglot-live-mode-change ()
  "Setting up the major mode again closes the document before reopening it."
  (lena-eglot-live-reopen #'lena-mode))

(ert-deftest lena-eglot-live-revert ()
  "Reverting the buffer closes the document only once."
  (lena-eglot-live-reopen (lambda () (revert-buffer t t))))

(defun lena-eglot-live-semantic-lines ()
  "Return the lines of the current buffer with faces of semantic tokens."
  (font-lock-ensure)
  (lena-rpc-live-wait #'ignore 1)
  (font-lock-ensure)
  (let (lines)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (seq-some (lambda (face)
                          (and (symbolp face)
                               (string-prefix-p "eglot-semantic"
                                                (symbol-name face))))
                        (ensure-list (get-text-property (point) 'face)))
          (cl-pushnew (line-number-at-pos) lines))
        (forward-char)))
    (nreverse lines)))

(ert-deftest lena-eglot-live-semantic-tokens-restart ()
  "After restarting the file, Eglot shows its semantic tokens again.
Eglot keeps the tokens of a document version, and the file is reopened
with the same version; here the tokens it has are empty, as when the
file was opened with imports out of date."
  (unless (fboundp 'eglot-semantic-tokens-mode)
    (ert-skip "This Eglot does not show semantic tokens"))
  (with-current-buffer (lena-rpc-live-buffer)
    (unwind-protect
        (progn
          (eglot-semantic-tokens-mode 1)
          (should (lena-rpc-live-wait
                   (lambda () (lena-eglot-live-semantic-lines)) 10))
          ;; As if Lean had answered while it could not elaborate.
          (setf (plist-get eglot--semtok-state :data) [])
          (font-lock-flush)
          (should-not (lena-eglot-live-semantic-lines))
          (setq lena-eglot-file-progress nil)
          (lena-eglot-restart-file)
          (should (lena-rpc-live-wait #'lena-rpc-live-processed-p 60))
          (should (lena-rpc-live-wait
                   (lambda () (lena-eglot-live-semantic-lines)) 10)))
      (eglot-semantic-tokens-mode -1))))

(provide 'lena-eglot-live)

;;; lena-eglot-live.el ends here
