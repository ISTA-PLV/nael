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

(ert-deftest nael-eglot-live-mode-change ()
  "Setting up the major mode again closes the document before reopening it."
  (with-current-buffer (nael-rpc-live-buffer)
    (let (sent)
      (cl-letf* ((notify (symbol-function 'jsonrpc-notify))
                 ((symbol-function 'jsonrpc-notify)
                  (lambda (connection method params)
                    (when (memq method '(:textDocument/didOpen
                                         :textDocument/didClose))
                      (push method sent))
                    (funcall notify connection method params))))
        (nael-mode)
        (should (nael-rpc-live-wait #'eglot-managed-p 30)))
      (should (equal (reverse sent)
                     '(:textDocument/didClose :textDocument/didOpen))))
    ;; The server works on the document again.
    (setq nael-eglot-file-progress nil)
    (should (nael-rpc-live-wait #'nael-rpc-live-processed-p 60))))

(provide 'nael-eglot-live)

;;; nael-eglot-live.el ends here
