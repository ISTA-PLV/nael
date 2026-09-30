;;; nael-lsp.el --- Nael and lsp-mode  -*- lexical-binding: t; -*-

;; Copyright © 2025 Mekeor Melire

;; Author: Mekeor Melire <mekeor@posteo.de>
;; Keywords: languages
;; Maintainer: Mekeor Melire <mekeor@posteo.de>
;; Package-Requires: ((emacs "29.1") (lsp-mode "9") (nael "0.8.3"))
;; SPDX-License-Identifier: GPL-3.0-only
;; URL: https://codeberg.org/mekeor/nael
;; Version: 0.8.3

;; This code is licensed under GNU General Public License version 3,
;; see LICENSE.GPL3.

;;; Commentary:

;; This file configures `lsp-mode' for `nael-mode'.

;;; Code:

(require 'cl-lib)

(require 'lsp-mode)

(require 'nael)
(require 'nael-eglot)

(defgroup nael-lsp nil
  "`lsp-mode' configured for `nael-mode'."
  :group 'nael
  :group 'lsp
  :link '(emacs-library-link :tag "Source Lisp File" "nael-lsp.el")
  :prefix "nael-lsp-")

;;;###autoload
(keymap-set nael-mode-map
            "C-c C-l"
            #'lsp)

(add-to-list 'lsp-language-id-configuration
             (cons 'nael-mode "nael"))

(lsp-register-client
 (make-lsp-client
  :language-id "nael"
  :major-modes '(nael-mode)
  :new-connection (lsp-stdio-connection '("lake" "serve"))
  :semantic-tokens-faces-overrides
  '(:types (("leanSorryLike" . font-lock-warning-face)))
  :server-id 'nael))

;; We could introduce the following interfaces so that we can use
;; destructuring features of Dash like (-lambda ((&nael:Goal :goals
;; :rendered)) (something goals rendered)) but in the context of
;; `nael-lsp-eldoc(-term)-goal', I wasn't yet able to use it like that
;; because sometimes the response will be nil and the destructuring
;; will fail.
;;
;; (lsp-interface
;;  (nael:Goal (:goals :rendered) nil)
;;  (nael:TermGoal (:goal :range) nil))

(defun nael-lsp-eldoc-goal (cb &rest _)
  "`PlainGoal' for `eldoc-documentation-functions'.

CB is the callback provided to members of ElDoc documentation
functions.

https://leanprover-community.github.io/mathlib4_docs/Lean/Data/Lsp/\
Extra.html#Lean.Lsp.PlainGoal"
  (lsp-request-async
   "$/lean/plainGoal"
   (lsp--text-document-position-params)
   (nael-eglot-eldoc-goal-fn cb #'lsp-get)
   :error-handler #'ignore
   :mode 'tick))

(defun nael-lsp-eldoc-term-goal (cb &rest _)
  "`PlainTermGoal' for `eldoc-documentation-functions', using callback CB."
  (lsp-request-async
   "$/lean/plainTermGoal"
   (lsp--text-document-position-params)
   (nael-eglot-eldoc-term-goal-fn cb #'lsp-get #'lsp--render-element)
   :error-handler #'ignore
   :mode 'tick))

;;;###autoload
(defun nael-lsp-configure-when-managed ()
  "Buffer-locally set up ElDoc and lsp-mode for Nael.

Use ElDoc documentation strategy `compose', apply
`nael-eglot-eldoc-idle-delay' and add ElDoc documentation functions for
proof goal."
  (interactive)
  (setq-local eldoc-documentation-strategy
              #'eldoc-documentation-compose)
  (when nael-eglot-eldoc-idle-delay
    (setq-local eldoc-idle-delay nael-eglot-eldoc-idle-delay))
  (add-hook 'eldoc-documentation-functions
            #'nael-lsp-eldoc-goal -90 'local)
  (add-hook 'eldoc-documentation-functions
            #'nael-lsp-eldoc-term-goal -80 'local))

(provide 'nael-lsp)

;;; nael-lsp.el ends here
