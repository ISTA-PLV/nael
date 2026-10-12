;;; nael-eglot-live.el --- Tests for nael-eglot against Lean  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see nael/LICENSE.GPL3.

;;; Commentary:

;; Tests for Nael's Eglot setup against a real Lean server on the Lake
;; project in `test/lean/'.  They reuse the setup of `nael-rpc-live'.
;;
;; Run with: emacs -Q --batch -l build.el test-live

;;; Code:

(require 'ert)
(require 'nael-rpc-live)

(defun nael-eglot-live-reopen (function)
  "Call FUNCTION in the live buffer and check that it reopens the document.
The document must be closed once and opened once, in that order, and
the server must work on it again."
  (with-current-buffer (nael-rpc-live-buffer)
    (let (sent)
      (cl-letf* ((notify (symbol-function 'jsonrpc-notify))
                 ((symbol-function 'jsonrpc-notify)
                  (lambda (connection method params)
                    (when (memq method '(:textDocument/didOpen
                                         :textDocument/didClose))
                      (push method sent))
                    (funcall notify connection method params))))
        (funcall function)
        (should (nael-rpc-live-wait #'eglot-managed-p 30)))
      (should (equal (reverse sent)
                     '(:textDocument/didClose :textDocument/didOpen))))
    (setq nael-eglot-file-progress nil)
    (should (nael-rpc-live-wait #'nael-rpc-live-processed-p 60))))

(ert-deftest nael-eglot-live-mode-change ()
  "Setting up the major mode again closes the document before reopening it."
  (nael-eglot-live-reopen #'nael-mode))

(ert-deftest nael-eglot-live-revert ()
  "Reverting the buffer closes the document only once."
  (nael-eglot-live-reopen (lambda () (revert-buffer t t))))

(defun nael-eglot-live-semantic-lines ()
  "Return the lines of the current buffer with faces of semantic tokens."
  (font-lock-ensure)
  (nael-rpc-live-wait #'ignore 1)
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

(ert-deftest nael-eglot-live-semantic-tokens-restart ()
  "After restarting the file, Eglot shows its semantic tokens again.
Eglot keeps the tokens of a document version, and the file is reopened
with the same version; here the tokens it has are empty, as when the
file was opened with imports out of date."
  (unless (fboundp 'eglot-semantic-tokens-mode)
    (ert-skip "This Eglot does not show semantic tokens"))
  (with-current-buffer (nael-rpc-live-buffer)
    (unwind-protect
        (progn
          (eglot-semantic-tokens-mode 1)
          (should (nael-rpc-live-wait
                   (lambda () (nael-eglot-live-semantic-lines)) 10))
          ;; As if Lean had answered while it could not elaborate.
          (setf (plist-get eglot--semtok-state :data) [])
          (font-lock-flush)
          (should-not (nael-eglot-live-semantic-lines))
          (setq nael-eglot-file-progress nil)
          (nael-eglot-restart-file)
          (should (nael-rpc-live-wait #'nael-rpc-live-processed-p 60))
          (should (nael-rpc-live-wait
                   (lambda () (nael-eglot-live-semantic-lines)) 10)))
      (eglot-semantic-tokens-mode -1))))

(provide 'nael-eglot-live)

;;; nael-eglot-live.el ends here
