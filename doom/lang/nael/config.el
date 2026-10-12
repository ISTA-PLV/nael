;;; lang/nael/config.el -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see nael/LICENSE.GPL3 in the Nael repository.

;; This module replaces Doom Emacs' `:lang lean' module for Nael.  Its
;; setup of abbreviations, Markdown code blocks, smartparens pairs and
;; LSP follows that module (Doom Emacs at commit f8329b4, Copyright (c)
;; 2014-2026 Henrik Lissner, MIT License; see
;; <https://github.com/doomemacs/doomemacs/blob/master/LICENSE>), as does
;; the popup rule of the help buffer, which matches Doom's rule for *Help*.

(use-package! nael
  :defer t
  :init
  ;; Lean's Unicode input, such as \to for →.
  (add-hook 'nael-mode-hook #'abbrev-mode)
  (after! markdown-mode
    (add-to-list 'markdown-code-lang-modes '("lean" . nael-mode)))
  :config
  (after! smartparens
    (sp-with-modes 'nael-mode
      (sp-local-pair "/-" "-/")
      (sp-local-pair "`" "`")
      (sp-local-pair "{" "}")
      (sp-local-pair "«" "»")
      (sp-local-pair "⟨" "⟩")
      (sp-local-pair "⟪" "⟫")))
  ;; Lean always uses Eglot, also when `:tools lsp' uses lsp-mode for
  ;; other languages.
  (when (modulep! +lsp)
    (add-hook 'nael-mode-local-vars-hook #'eglot-ensure 'append))
  (map! :map nael-mode-map
        :localleader
        "a" #'nael-abbrev-help
        "b" #'project-compile
        "e" #'eldoc-doc-buffer
        "i" #'nael-infoview
        "r" #'nael-eglot-restart-file))

;;; Infoview

;; The infoview places itself, in a side window by default; see
;; `nael-infoview-display-action'.
(set-popup-rule! "^\\*Nael Infoview\\*" :ignore t)
(set-popup-rule! "^\\*Nael Help\\*" :slot 2 :vslot -8 :size 0.42 :select t)

(after! nael-infoview-buffer
  (map! :map nael-infoview-mode-map
        :localleader
        "r" #'nael-infoview-refresh
        "v" #'nael-infoview-view-options
        "y" #'nael-infoview-copy-goal))

(when (modulep! :editor evil)
  (after! (evil nael-infoview-buffer)
    (require 'nael-evil)
    (nael-evil-setup)))
