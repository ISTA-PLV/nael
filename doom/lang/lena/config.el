;;; lang/lena/config.el -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see lena/LICENSE.GPL3 in the Lena repository.

;; This module replaces Doom Emacs' `:lang lean' module for Lena.  Its
;; setup of abbreviations, Markdown code blocks, smartparens pairs and
;; LSP follows that module (Doom Emacs at commit f8329b4, Copyright (c)
;; 2014-2026 Henrik Lissner, MIT License; see
;; <https://github.com/doomemacs/doomemacs/blob/master/LICENSE>), as does
;; the popup rule of the help buffer, which matches Doom's rule for *Help*.

(use-package! lena
  :defer t
  :init
  ;; Lean's Unicode input, such as \to for →.
  (add-hook 'lena-mode-hook #'abbrev-mode)
  (after! markdown-mode
    (add-to-list 'markdown-code-lang-modes '("lean" . lena-mode)))
  :config
  (after! smartparens
    (sp-with-modes 'lena-mode
      (sp-local-pair "/-" "-/")
      (sp-local-pair "`" "`")
      (sp-local-pair "{" "}")
      (sp-local-pair "«" "»")
      (sp-local-pair "⟨" "⟩")
      (sp-local-pair "⟪" "⟫")))
  ;; Lean always uses Eglot, also when `:tools lsp' uses lsp-mode for
  ;; other languages.
  (when (modulep! +lsp)
    (add-hook 'lena-mode-local-vars-hook #'eglot-ensure 'append))
  (map! :map lena-mode-map
        :localleader
        "a" #'lena-abbrev-help
        "b" #'project-compile
        "e" #'eldoc-doc-buffer
        "i" #'lena-infoview
        "r" #'lena-eglot-restart-file))

;;; Infoview

;; The infoview places itself, in a side window by default; see
;; `lena-infoview-display-action'.
(set-popup-rule! "^\\*Lena Infoview\\*" :ignore t)
(set-popup-rule! "^\\*Lena Help\\*" :slot 2 :vslot -8 :size 0.42 :select t)

(after! lena-infoview-buffer
  (map! :map lena-infoview-mode-map
        :localleader
        "r" #'lena-infoview-refresh
        "v" #'lena-infoview-view-options
        "y" #'lena-infoview-copy-goal))

(when (modulep! :editor evil)
  (after! (evil lena-infoview-buffer)
    (require 'lena-evil)
    (lena-evil-setup)))
