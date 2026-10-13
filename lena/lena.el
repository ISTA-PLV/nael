;;; lena.el --- Major mode for Lean  -*- lexical-binding: t; -*-

;; Copyright © 2013-2014 Microsoft Corp.
;; Copyright © 2014-2015 Soonho Kong
;; Copyright © 2024 Free Software Foundation, Inc.
;; Copyright © 2025 Mekeor Melire

;; Author: Adam Topaz <topaz@ualberta.ca>
;;         Akira Komamura <akira.komamura@gmail.com>
;;         Bao Zhiyuan <bzy_sustech@foxmail.com>
;;         Daniel Selsam <daniel.selsam@protonmail.com>
;;         Gabriel Ebner <gebner@gebner.org>
;;         Henrik Böving <hargonix@gmail.com>
;;         Hongyu Ouyang <oyhy0214@163.com>
;;         Jakub Bartczuk <bartczukkuba@gmail.com>
;;         Leonardo de Moura <leonardo@microsoft.com>
;;         Mauricio Collares <mauricio@collares.org>
;;         Mekeor Melire <mekeor@posteo.de>
;;         Philip Kaludercic <philipk@posteo.net>
;;         Richard Copley <buster@buster.me.uk>
;;         Sebastian Ullrich <sebasti@nullri.ch>
;;         Siddharth Bhat <siddu.druid@gmail.com>
;;         Simon Hudon <simon.hudon@gmail.com>
;;         Soonho Kong <soonhok@cs.cmu.edu>
;;         Tomáš Skřivan <skrivantomas@seznam.cz>
;;         Wojciech Nawrocki <wjnawrocki@protonmail.com>
;;         Yael Dillies <yael.dillies@gmail.com>
;;         Yury G. Kudryashov <urkud@urkud.name>
;; Keywords: languages
;; Maintainer: Klaus Kraßnitzer <klaus.krassnitzer@ista.ac.at>
;;             Alex Loitzl <alexander.loitzl@gmail.com>
;; Package-Requires: ((emacs "30.1"))
;; SPDX-License-Identifier: Apache-2.0 AND GPL-3.0-only AND MIT
;; URL: https://github.com/ISTA-PLV/nael
;; Version: 0.8.3

;; This is licensed under GNU General Public License (version 3 only),
;; see LICENSE.GPL3.  To be precise, it is licensed under Apache-2.0,
;; see LICENSE.APACHE2, and sublicensed under GPL3.

;;; Commentary:

;; `lena-mode' is a major mode for the Lean language.  For more
;; information, read the manual:
;;
;; - As Org file `README.org' in the `lena' folder or repository.
;;
;; - After installation, as local Info manual per C-h i d lena RET or
;;   (info "(lena) Top")
;;
;; - As HTML webpage at <https://github.com/ISTA-PLV/nael>.

;;; Code:

(require 'jsonrpc)
(require 'project)
(require 'rx)
(require 'seq)

;; Avoid strict loading of optional features, files or dependencies.
(declare-function flymake-goto-next-error "flymake"
                  (&optional n filter interactive))
(declare-function lena-eglot-configure-when-initialized "lena-eglot"
                  (_))
(declare-function lena-eglot-configure-when-managed "lena-eglot" ())
(declare-function lena-eglot-restart-file "lena-eglot" ())
(declare-function lena-infoview "lena-infoview" ())
(declare-function sp-local-pair "smartparens" (modes open close &rest args))

(defgroup lena nil
  "Major mode for Lean."
  :group 'languages
  :link '(emacs-library-link :tag "Source Lisp File" "lena.el")
  :link '(url-link :tag "Website" "https://github.com/ISTA-PLV/nael")
  :prefix "lena-"
  :tag "Lena")

(defvar lena-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Parentheses:
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)

    ;; Comments:
    (modify-syntax-entry ?/  ". 14nb" table)
    (modify-syntax-entry ?-  ". 123"  table)
    (modify-syntax-entry ?\n ">"      table)
    ;; « » registered as comments, content fixed in `lena-font-lock-defaults`
    (modify-syntax-entry ?«  "< c"      table)
    (modify-syntax-entry ?»  "> c"      table)

    ;; Words:
    (mapc
     (lambda (character) (modify-syntax-entry character "w" table))
     '(;; Latin characters:
       ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u
       ?v ?w ?x ?y ?z ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P
       ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
       ;; Digits
       ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
       ;; Greek (and Coptic) characters:
       ?α ?β ?γ ?δ ?ε ?ζ ?η ?θ ?ι ?κ ;; ?λ
       ?μ ?ν ?ξ ?ο ?π ?ρ ?ς ?σ ?τ ?υ ?φ ?χ ?ψ ?ω ?ϊ ?ϋ ?ό ?ύ ?ώ ?Ϗ ?ϐ
       ?ϑ ?ϒ ?ϓ ?ϔ ?ϕ ?ϖ ?ϗ ?Ϙ ?ϙ ?Ϛ ?ϛ ?Ϝ ?ϝ ?Ϟ ?ϟ ?Ϡ ?ϡ ?Ϣ ?ϣ ?Ϥ ?ϥ
       ?Ϧ ?ϧ ?Ϩ ?ϩ ?Ϫ ?ϫ ?Ϭ ?ϭ ?Ϯ ?ϯ ?ϰ ?ϱ ?ϲ ?ϳ ?ϴ ?ϵ ?϶ ?Ϸ ?ϸ ?Ϲ ?Ϻ
       ?ϻ ?ἀ ?ἁ ?ἂ ?ἃ ?ἄ ?ἅ ?ἆ ?ἇ ?Ἀ ?Ἁ ?Ἂ ?Ἃ ?Ἄ ?Ἅ ?Ἆ ?Ἇ ?ἐ ?ἑ ?ἒ ?ἓ
       ?ἔ ?ἕ ?἖ ?἗ ?Ἐ ?Ἑ ?Ἒ ?Ἓ ?Ἔ ?Ἕ ?἞ ?἟ ?ἠ ?ἡ ?ἢ ?ἣ ?ἤ ?ἥ ?ἦ ?ἧ ?Ἠ
       ?Ἡ ?Ἢ ?Ἣ ?Ἤ ?Ἥ ?Ἦ ?Ἧ ?ἰ ?ἱ ?ἲ ?ἳ ?ἴ ?ἵ ?ἶ ?ἷ ?Ἰ ?Ἱ ?Ἲ ?Ἳ ?Ἴ ?Ἵ
       ?Ἶ ?Ἷ ?ὀ ?ὁ ?ὂ ?ὃ ?ὄ ?ὅ ?὆ ?὇ ?Ὀ ?Ὁ ?Ὂ ?Ὃ ?Ὄ ?Ὅ ?὎ ?὏ ?ὐ ?ὑ ?ὒ
       ?ὓ ?ὔ ?ὕ ?ὖ ?ὗ ?὘ ?Ὑ ?὚ ?Ὓ ?὜ ?Ὕ ?὞ ?Ὗ ?ὠ ?ὡ ?ὢ ?ὣ ?ὤ ?ὥ ?ὦ ?ὧ
       ?Ὠ ?Ὡ ?Ὢ ?Ὣ ?Ὤ ?Ὥ ?Ὦ ?Ὧ ?ὰ ?ά ?ὲ ?έ ?ὴ ?ή ?ὶ ?ί ?ὸ ?ό ?ὺ ?ύ ?ὼ
       ?ώ ?὾ ?὿ ?ᾀ ?ᾁ ?ᾂ ?ᾃ ?ᾄ ?ᾅ ?ᾆ ?ᾇ ?ᾈ ?ᾉ ?ᾊ ?ᾋ ?ᾌ ?ᾍ ?ᾎ ?ᾏ ?ᾐ ?ᾑ
       ?ᾒ ?ᾓ ?ᾔ ?ᾕ ?ᾖ ?ᾗ ?ᾘ ?ᾙ ?ᾚ ?ᾛ ?ᾜ ?ᾝ ?ᾞ ?ᾟ ?ᾠ ?ᾡ ?ᾢ ?ᾣ ?ᾤ ?ᾥ ?ᾦ
       ?ᾧ ?ᾨ ?ᾩ ?ᾪ ?ᾫ ?ᾬ ?ᾭ ?ᾮ ?ᾯ ?ᾰ ?ᾱ ?ᾲ ?ᾳ ?ᾴ ?᾵ ?ᾶ ?ᾷ ?Ᾰ ?Ᾱ ?Ὰ ?Ά
       ?ᾼ ?᾽ ?ι ?᾿ ?῀ ?῁ ?ῂ ?ῃ ?ῄ ?῅ ?ῆ ?ῇ ?Ὲ ?Έ ?Ὴ ?Ή ?ῌ ?῍ ?῎ ?῏ ?ῐ
       ?ῑ ?ῒ ?ΐ ?῔ ?῕ ?ῖ ?ῗ ?Ῐ ?Ῑ ?Ὶ ?Ί ?῜ ?῝ ?῞ ?῟ ?ῠ ?ῡ ?ῢ ?ΰ ?ῤ ?ῥ
       ?ῦ ?ῧ ?Ῠ ?Ῡ ?Ὺ ?Ύ ?Ῥ ?῭ ?΅ ?` ?῰ ?῱ ?ῲ ?ῳ ?ῴ ?῵ ?ῶ ?ῷ ?Ὸ ?Ό ?Ὼ
       ?Ώ ?ῼ ?´ ?῾
       ;; Mathematical characters:
       ?℀ ?℁ ?ℂ ?℃ ?℄ ?℅ ?℆ ?ℇ ?℈ ?℉ ?ℊ ?ℋ ?ℌ ?ℍ ?ℎ ?ℏ ?ℐ ?ℑ ?ℒ ?ℓ ?℔
       ?ℕ ?№ ?℗ ?℘ ?ℙ ?ℚ ?ℛ ?ℜ ?ℝ ?℞ ?℟ ?℠ ?℡ ?™ ?℣ ?ℤ ?℥ ?Ω ?℧ ?ℨ ?℩
       ?K ?Å ?ℬ ?ℭ ?℮ ?ℯ ?ℰ ?ℱ ?Ⅎ ?ℳ ?ℴ ?ℵ ?ℶ ?ℷ ?ℸ ?ℹ ?℺ ?℻ ?ℼ ?ℽ ?ℾ
       ?ℿ ?⅀ ?⅁ ?⅂ ?⅃ ?⅄ ?ⅅ ?ⅆ ?ⅇ ?ⅈ ?ⅉ ?⅊ ?⅋ ?⅌ ?⅍ ?ⅎ ?⅏
       ;; Subscripts:
       ?₁ ?₂ ?₃ ?₄ ?₅ ?₆ ?₇ ?₈ ?₉ ?₀ ?ₐ ?ₑ ?ₒ ?ₓ ?ₔ ?ₕ ?ₖ ?ₗ ?ₘ ?ₙ ?ₚ
       ?ₛ ?ₜ))

    ;; Symbol constituents
    (mapc
     (lambda (character) (modify-syntax-entry character "_" table))
     '(?_ ?! ?? ?'))

    ;; Operators:
    (mapc
     (lambda (character) (modify-syntax-entry character "." table))
     '(?# ?$ ?% ?& ?* ?+ ?< ?= ?> ?@ ?^ ?| ?~ ?:))

    ;; Whitespace:
    (modify-syntax-entry ?\  " " table)
    (modify-syntax-entry ?\t " " table)

    ;; Strings:
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "/"  table)

    table)
  "Syntax table used in `lena-mode'.")

(defconst lena-syntax-definition-pre
  (rx (zero-or-more (or "noncomputable" "partial" "unsafe"
                        "private" "protected" "public" space))))

(defconst lena-syntax-definition
  (rx
   ;; Use `line-start' rather than `word-start' for speed.
   (zero-or-more line-start (regexp lena-syntax-definition-pre))
   line-start
   (group
    (regexp lena-syntax-definition-pre)
    (or "axiom" "class" "constant" "def" "definition" "inductive"
        "instance" "lemma" "opaque" "structure" "theorem"
        (group "class" (zero-or-more space) "inductive")))
   symbol-end
   (zero-or-more space)
   (group (zero-or-more
           "{" (zero-or-more (not (any "}"))) "}"
           (zero-or-more space)))
   (zero-or-more space)
   (group-n 99 (zero-or-more (not (any " \t\n\r{([:")))))
  "Regular expression matching definitions.")

(defvar lena-font-lock-defaults
  (list
   (list
    (list (rx symbol-start "attribute" symbol-end
              (zero-or-more space)
              (group
               (one-or-more "[" (zero-or-more (not (any "]"))) "]"
                            (zero-or-more space))))
          '(1 'font-lock-preprocessor-face))
    (list (rx (group "@[" (zero-or-more (not (any "]"))) "]"))
          '(1 'font-lock-preprocessor-face))
    (list (rx (group "#"
                     (or "eval" "print" "reduce" "help" "check" "lang"
                         "check_failure" "synth")))
          '(1 'font-lock-keyword-face))

    ;; Mutual definitions:
    (list (rx symbol-start "mutual" symbol-end
              (zero-or-more space)
              symbol-start (or "inductive" "definition" "def") symbol-end
              (group (zero-or-more (not (any " \t\n\r{([,")))
                     (zero-or-more (zero-or-more space) ","
                                   (zero-or-more space)
                                   (not (any " \t\n\r{([,")))))
          '(1 'font-lock-function-name-face))

    ;; Definitions:
    (list lena-syntax-definition
          '(1 'font-lock-keyword-face)
          '(99 'font-lock-function-name-face))

    ;; Constants which have a keyword as subterm:
    (cons "∘if"
          'font-lock-constant-face)

    ;; Keywords:
    (list "\\(set_option\\)[ \t]*\\([^ \t\n]*\\)"
          '(2 'font-lock-constant-face))
    (cons (rx symbol-start
              (or
               "abbrev" "apply" "assert!" "at" "attribute" "attributes"
               "axiom" "begin" "break" "builtin_initialize" "by" "cases"
               "catch" "class" "constant" "continue" "dbg_trace"
               "declare_syntax_cat" "def" "deriving" "do" "elab" "else"
               "end" "exact" "example" "exists" "export" "extends"
               "finally" "for" "forall" "from" "fun" "generalizing"
               "have" "hide" "hiding" "if" "import" "in" "include"
               "induction" "inductive" "infix" "infixl" "infixr"
               "init_quot" "initialize" "instance" "lemma" "let" "local"
               "macro" "macro_rules" "match" "match_syntax" "module"
               "mut" "mutual" "namespace" "nomatch" "noncomputable"
               "notation" "open" "opaque" "panic!" "partial" "postfix"
               "precedence" "prefix" "prelude" "private" "protected"
               "public" "raw" "rec" "register_builtin_option" "renaming"
               "return" "run_cmd" "scoped" "section" "set_option" "show"
               "structure" "suffices" "syntax" "then" "theorem" "this"
               "try" "unif_hint" "universe" "universes" "unless"
               "unreachable!" "unsafe" "using" "using_well_founded"
               "variable" "variables" "where" "with")
              symbol-end)
          'font-lock-keyword-face)
    (list (rx word-start (group "example") ".")
          '(1 'font-lock-keyword-face))
    (cons (rx (or "∎"))
          'font-lock-keyword-face)

    ;; Types:
    (cons (rx symbol-start
              (or (seq (or "Type" "Sort") "*")
                  (seq (or "Prop" "Type" "Sort") symbol-end)))
          'font-lock-type-face)
    (list (rx word-start (group (or "Prop" "Type" "Sort")) ".")
          '(1 'font-lock-type-face))

    ;; Strings:
    (cons "\"[^\"]*\"" 'font-lock-string-face)

    ;; Constants:
    (cons (regexp-opt
           '("!" "#" "$" "&&" "*" "+" "+c" "+f" "+n" "-" "->" "/" "/"
             "/\\" ":=" "<" "<->" "<=" "=" "==" ">" ">=" "@" "\\/"
             "^c" "||" "~" "¬" "×" "×c" "×f" "×n" "Π" "Σ" "λ" "⁻¹" "ℂ"
             "ℕ" "ℕ₋₂" "ℚ" "ℝ" "ℤ" "→" "↔" "∀" "∃" "∘" "∘1nf" "∘f"
             "∘f1n" "∘fi" "∘fn" "∘fn1" "∘n" "∘n1f" "∘nf" "∧" "∨" "∼"
             "≃" "≃c" "≅" "≅c" "≠" "≡" "≤" "≥" "▸" "◾" "◾o" "⬝" "⬝e"
             "⬝h" "⬝hp" "⬝i" "⬝o" "⬝op" "⬝ph" "⬝po" "⬝pv" "⬝r" "⬝v"
             "⬝vp" "𝔸"))
          'font-lock-constant-face)
    (cons (rx symbol-start
              (one-or-more digit)
              (optional (and "." (zero-or-more digit)))
              symbol-end)
          'font-lock-constant-face)

    ;; Place holder:
    (cons (rx symbol-start "_" symbol-end)
          'font-lock-preprocessor-face)

    ;; Warnings:
    (cons (rx symbol-start "sorry" symbol-end)
          'font-lock-warning-face)

    ;; Escaped identifiers:
    (list (rx (group "«")
              (group (one-or-more (not (any "»"))))
              (group "»"))
          '(1 font-lock-comment-face t)
          '(2 nil t)
          '(3 font-lock-comment-face t))))
  "Defaults for `font-lock-mode' used by `lena-mode'.")

;;;; Auxiliary Functions and Commands:

(defun lena-comment-insert ()
  "`comment-insert-comment-function' for `lena-mode'."
  (interactive)
  (if (save-excursion (beginning-of-line)
                      (looking-at-p "[[:blank:]]*$"))
      (progn
        ;; Respect users who set `comment-start' to "--".
        (insert comment-start " ")
        ;; Respect users who set `comment-end' to "".
        (unless (length= comment-end 0)
          (save-excursion
            (insert " " comment-end))))
    (end-of-line)
    (unless (looking-back "[[:blank:]]" (1- (point)))
      (insert " "))
    (insert "-- ")))

(defun lena-fill-paragraph (&optional justify)
  "Fill comment paragraph at point.  Maybe JUSTIFY."
  (interactive)
  (when (save-excursion (nth 4 (syntax-ppss (point))))
    (let* ((com-beg (save-excursion
                      (re-search-backward "[/-]-" nil t)
                      (match-beginning 0)))
           (multi (eq (char-after com-beg) ?/)))
      (if multi
          (let* ((par-beg (save-excursion
                            (re-search-backward paragraph-start nil t)
                            (match-beginning 0)))
                 (beg (max com-beg par-beg))
                 (com-end (if multi "-/" "$"))
                 (com-end (save-excursion
                            ;; If cursor is at -|/, then move to |-/,
                            ;; so that `re-search-forward' can locate
                            ;; comment ending.
                            (and (eq (char-before) ?-)
                                 (eq (char-after) ?/)
                                 (backward-char))
                            (re-search-forward com-end nil t)
                            (match-end 0)))
                 (par-end (save-excursion
                            (search-forward paragraph-separate nil t)
                            (match-end 0)))
                 (end (min com-end par-end)))
            (fill-region beg end justify))
        ;; `fill-comment-paragraph' fills prefixed comments well, when
        ;; configured correctly.
        (let ((comment-start "--") (comment-end ""))
          ;; For some reason, "" is used as fill-prefix by
          ;; `fill-comment-paragraph' when point is at --|.  Avoid
          ;; this misbehavior by moving point forward one char.
          (and (not (eolp))
               (looking-back "--" (max (- (point) 2) (point-min)))
               (forward-char))
          (fill-comment-paragraph justify))))))

;; TODO: Both `lena-navigation-defun-beginning' and
;; `lena-navigation-defun-name' currently lack support for `mutual'
;; blocks, i.e. mutually recursive definitions.

(defun lena-navigation-defun-end ()
  "`end-of-defun-function' for `lena-mode'."
  (interactive)
  (when (re-search-forward lena-syntax-definition nil t)
    (goto-char (match-beginning 0))))

(defun lena-navigation-defun-beginning ()
  "`beginning-of-defun-function' for `lena-mode'."
  (interactive)
  (re-search-backward lena-syntax-definition nil t))

(defun lena-navigation-defun-name ()
  "`add-log-current-defun-function' for `lena-mode'."
  (save-excursion
    (when (lena-navigation-defun-beginning)
      (forward-symbol 1)
      (forward-whitespace 1)
      (symbol-at-point))))

(defvar lena-imenu-generic-expression
  (list (list nil lena-syntax-definition 99))
  "`imenu-generic-expression' for `lena-mode'.")

;;;; Preparation:

;; Our goal is to avoid loading `lena-eglot' / `eglot' and
;; `lena-quail' / `quail', until the user calls one of their autoloaded
;; commands or activates the input method.  We are lucky that
;; `eglot-server-initialized-hook' as well as `eglot-managed-mode-hook'
;; are initialized to nil, in usual Emacs manner.  Thus, it's fine to
;; call `add-hook' on them, even if they have not been defined as
;; variables yet.

;; We could introduce a hook, with both of
;; `lena-prepare-{eglot,quail}' being default members of it,
;; which we could run in the beginning of the definition-body of
;; `lena-mode'.  As mentioned, though, it's uncommon in Emacs to have
;; hooks initialized with non-nil values and some common functions
;; like `add-hook' rely on this practice.  Thus, we use boolean flags
;; instead.

(defcustom lena-prepare-eglot t
  "Whether `eglot' should be prepared for `lena-mode'."
  :type 'boolean
  :group 'lena)

(defun lena-prepare-eglot ()
  "Prepare `eglot' for `lena-mode'."
  (interactive)
  (when lena-prepare-eglot
    ;; We want to add an entry to `eglot-server-programs' but we want
    ;; to avoid stricly loading `eglot' here.  Unfortunately, Eglot
    ;; doesn't offer any hook that'd be run before it accesses
    ;; `eglot-server-programs'.  We have no choice but
    ;; `with-eval-after-load'.
    (with-eval-after-load 'eglot
      (require 'lena-eglot))
    (add-hook 'eglot-server-initialized-hook
              #'lena-eglot-configure-when-initialized nil 'local)
    (add-hook 'eglot-managed-mode-hook
              #'lena-eglot-configure-when-managed nil 'local)))

;; Let's use the same interface (a configure-function, a
;; prepare-option and -function) for Flymake too because we don't load
;; or invoke it in `lena-mode' itself.

(defun lena-flymake-configure ()
  "Use Flymake to jump to errors."
  (interactive)
  (setq-local next-error-function
              #'flymake-goto-next-error))

(defcustom lena-prepare-flymake t
  "Whether `flymake-mode' should be prepared for `lena-mode'."
  :type 'boolean
  :group 'lena)

(defun lena-prepare-flymake ()
  "Prepare `flymake-mode' for `lena-mode'."
  (interactive)
  (when lena-prepare-flymake
    (add-hook 'flymake-mode-hook
              #'lena-flymake-configure nil 'local)))

;; Register the Lena input method without loading `lena-quail'.  Like
;; the input methods in Emacs' `leim-list', `quail-use-package' loads
;; the library only once the input method is activated.
(register-input-method
 "Lena" "UTF-8" #'quail-use-package "∀"
 "TeX-like input method for Lean language and `lena-mode'."
 "lena-quail")

(defcustom lena-prepare-quail t
  "Whether the Lena input method should be prepared for `lena-mode'.

When non-nil, `lena-mode' makes Lena the buffer's default input method
so that \\[toggle-input-method] activates it."
  :type 'boolean
  :group 'lena)

(defun lena-prepare-quail ()
  "Prepare the Lena input method for `lena-mode'."
  (interactive)
  (when lena-prepare-quail
    (setq-local default-input-method
                "Lena")))

;;;; Mode:

(defcustom lena-mode-hook nil
  "Hook run when entering `lena-mode'."
  :options '(eglot-ensure imenu-add-menubar-index)
  :type 'hook
  :group 'lena)

(defvar-keymap lena-mode-map
  "<remap> <display-local-help>" #'eldoc-doc-buffer
  "C-c C-c" #'project-compile
  "C-c C-e" #'eglot
  "C-c C-i" #'lena-infoview
  "C-c C-r" #'lena-eglot-restart-file)

;;;###autoload
(define-derived-mode lena-mode prog-mode "Lena"
  "Major mode for Lean.

\\{lena-mode-map}"
  ;; Preparations:
  (lena-prepare-eglot)
  (lena-prepare-quail)
  ;; Navigation:
  (setq-local add-log-current-defun-function
              #'lena-navigation-defun-name)
  (setq-local beginning-of-defun-function
              #'lena-navigation-defun-beginning)
  (setq-local end-of-defun-function
              #'lena-navigation-defun-end)
  ;; Paragraphs and filling:
  (setq-local paragraph-start
              "[[:blank:]]*$")
  (setq-local paragraph-separate
              "[[:blank:]]*$")
  (setq-local fill-paragraph-function
              #'lena-fill-paragraph)
  ;; Comments:
  (setq-local comment-end
              "-/")
  (setq-local comment-end-skip
              "[[:space:]]*-/")
  (setq-local comment-insert-comment-function
              #'lena-comment-insert)
  (setq-local comment-padding
              1)
  (setq-local comment-quote-nested ;; Comments may be nested.
              nil)
  (setq-local comment-start
              "/-")
  (setq-local comment-start-skip
              "/-[[:space:]]*")
  (setq-local comment-style
              'multi-line)
  (setq-local comment-use-syntax
              t)
  (setq-local parse-sexp-ignore-comments
              t)
  ;; Font-lock:
  (setq-local font-lock-defaults
              lena-font-lock-defaults)
  ;; Compile:
  (setq-local compilation-mode-font-lock-keywords
              nil)
  (setq-local compile-command
              "lake build ")
  ;; Imenu:
  (setq-local imenu-generic-expression
              lena-imenu-generic-expression)
  ;; Flymake:
  (lena-flymake-configure))

;; Lean language specification requires UTF-8 encoding.
(modify-coding-system-alist 'file "\\.lean\\'" 'utf-8)

;;;; Association:

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons "\\.lean\\'" 'lena-mode))

(add-to-list 'project-vc-extra-root-markers
             "lakefile.lean")

(add-to-list 'project-vc-extra-root-markers
             "lakefile.toml")

;; Projectile, which Doom Emacs uses to find projects, takes the nearest
;; directory with one of these files as the root, so that a Lake project
;; inside a larger repository gets its own Lean server.
(with-eval-after-load 'projectile
  (defvar projectile-project-root-files-bottom-up)
  (add-to-list 'projectile-project-root-files-bottom-up "lakefile.lean")
  (add-to-list 'projectile-project-root-files-bottom-up "lakefile.toml"))

(with-eval-after-load 'org-src
  (defvar org-src-lang-modes)
  (add-to-list 'org-src-lang-modes
               (cons "lean" 'lena)))

;; If the code that requires `markdown-mode' grows, we will extract it
;; into a new package that depends on it.  But a single expression is
;; not worth a package.
(with-eval-after-load 'markdown-mode
  (defvar markdown-code-lang-modes)
  (add-to-list 'markdown-code-lang-modes
               (cons "lean" 'lena-mode)))

;; These pairs follow Doom Emacs' `:lang lean' module (Doom Emacs at
;; commit f8329b4, Copyright (c) 2014-2026 Henrik Lissner, MIT License;
;; see <https://github.com/doomemacs/doomemacs/blob/master/LICENSE>).
(with-eval-after-load 'smartparens
  (sp-local-pair 'lena-mode "/-" "-/")
  (sp-local-pair 'lena-mode "`" "`")
  (sp-local-pair 'lena-mode "{" "}")
  (sp-local-pair 'lena-mode "«" "»")
  (sp-local-pair 'lena-mode "⟨" "⟩")
  (sp-local-pair 'lena-mode "⟪" "⟫"))

(provide 'lena)

;;; lena.el ends here
