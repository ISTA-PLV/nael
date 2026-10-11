#!/usr/bin/env -S emacs -x
;; -*- lexical-binding: t; -*-

;; Copyright © 2026 Mekeor Melire

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see LICENSE.

;;; Commentary:

;; This file is supposed to be run as Elisp script, e.g. from shell
;; via `./build.el'.  Its purpose is to generate other files,
;; depending on passed command-line arguments:

;; - Pass `abbreviations' to generate the Emacs-Lisp source file
;;   `lena/lena-quail.el' based on data from the official Lean plugin
;;   for V. S. Code.
;; - Pass `autoloads' to generate `*/*-autoloads.el' files.
;; - Pass `compile' to generate `*/*.elc' files.  It fails on any
;;   warning.
;; - Pass `lint' to check the packages with `checkdoc' and, if it is in
;;   `load-path', `package-lint'.  It fails on any `checkdoc' complaint
;;   and on errors of `package-lint'; its warnings are only shown.
;; - Pass `info' to generate `lena/lena.info' and `lena/lena.texi'.
;; - Pass `root-readme' to copy `lena/README.org' to `./README.org'.
;; - Pass `test' to run the unit tests in `test/*-tests.el'.
;; - Pass `test-live' to run them together with the tests in
;;   `test/*-live.el', which need Lean (`lake' in `exec-path') and take
;;   a few minutes.

;; If no command-line arguments are passed, the following defaults
;; are targeted: `abbreviations', `info' and `root-readme'.

;;; Code:

(defconst build-targets
  (or command-line-args-left
      (list
       "abbreviations"
       ;; "autoloads"
       ;; "compile"
       "info"
       "root-readme")))

;;;; Build target `autoloads':

(when (member "autoloads" build-targets)
  (dolist (dir (file-expand-wildcards "lena*"))
    (loaddefs-generate
     dir
     (file-name-concat dir (concat dir "-autoloads.el")))))

;;;; Build target `compile':

;; TODO: Make dependency `markdown-mode' available.
(when (member "compile" build-targets)
  (require 'bytecomp)
  (let ((failed nil))
    (dolist (dir (file-expand-wildcards "lena*"))
      (let ((load-path
             ;; Each package needs itself in `load-path'.  `lena-*'
             ;; packages each need `lena' in `load-path' too.
             (append (mapcar #'file-truename (list dir "lena"))
                     load-path))
            (byte-compile-error-on-warn t))
        (dolist (file (directory-files dir t (rx ".el" eos)))
          (unless (or (string-suffix-p "-autoloads.el" file)
                      (string-suffix-p "-pkg.el" file)
                      (byte-compile-file file))
            (setq failed t)))))
    (when failed
      (kill-emacs 1))))

;;;; Build target `lint':

(when (member "lint" build-targets)
  (require 'checkdoc)
  (let ((failed nil)
        (root default-directory)
        ;; Generated from data of the Lean plugin for V. S. Code.
        (generated '("lena-abbrev.el" "lena-skeleton.el")))
    (dolist (dir (file-expand-wildcards "lena*"))
      (dolist (file (directory-files dir t (rx ".el" eos)))
        (unless (or (string-suffix-p "-autoloads.el" file)
                    (string-suffix-p "-pkg.el" file)
                    (member (file-name-nondirectory file) generated))
          (with-current-buffer (find-file-noselect file)
            (let ((sentence-end-double-space t)
                  ;; Its guesses at imperative verbs misfire on
                  ;; docstrings of variables and predicates.
                  (checkdoc-verb-check-experimental-flag nil)
                  (checkdoc-autofix-flag 'never)
                  (checkdoc-create-error-function
                   (lambda (text start _end &optional _unfixable)
                     (setq failed t)
                     (message "%s:%d: %s" (file-relative-name file root)
                              (line-number-at-pos start) text)
                     nil)))
              (checkdoc-current-buffer t))))))
    (if (not (require 'package-lint nil t))
        (message "package-lint is not in `load-path'; skipping it")
      (package-initialize)
      (dolist (dir (file-expand-wildcards "lena*"))
        (let ((package-lint-main-file
               (file-truename (file-name-concat dir (concat dir ".el")))))
          (dolist (file (directory-files dir t (rx ".el" eos)))
            (unless (or (string-suffix-p "-autoloads.el" file)
                        (string-suffix-p "-pkg.el" file))
              (with-current-buffer (find-file-noselect file)
                (pcase-dolist (`(,line ,column ,type ,text)
                               (package-lint-buffer))
                  ;; Lena is not in any package archive yet, and without
                  ;; archives no dependency is installable.  Newer
                  ;; package-lint takes `any' in `rx' forms for the
                  ;; function of Emacs 31.
                  (unless (or (and (string-match-p "is not installable" text)
                                   (or (string-match-p "\\_<lena\\_>" text)
                                       (null package-archive-contents)))
                              (string-match-p "if you need `any'" text))
                    (when (eq type 'error)
                      (setq failed t))
                    (message "%s:%d:%d: %s: %s" (file-relative-name file root)
                             line column type text)))))))))
    (when failed
      (kill-emacs 1))))

;;;; Build targets `info' and `root-readme':

(when (or (member "info" build-targets)
          (member "root-readme" build-targets))
  (require 'ox-texinfo)

  ;; Avoid README.org~ file.
  (setq backup-inhibited t)
  (setq create-lockfiles nil)

  (with-temp-buffer
    (find-file "lena/README.org")
    ;; On the Codeberg website <https://codeberg.org/mekeor/nael> the
    ;; table of contents doesn't look nice.  Gladly, Codeberg/Forgejo
    ;; respects the file-local setting “toc:nil”.  But for Texinfo
    ;; exports we do want a TOC, so here we remove that setting.
    (with-undo-amalgamate
      (replace-regexp (rx line-start "#+options: toc:nil" line-end) "")
      (when (member "info" build-targets)
        (org-texinfo-export-to-info)))
    (ignore-errors (undo))
    ;; We want a readme to be displayed on the website.  Unfortunately,
    ;; Codeberg/Forgejo does not render the README.org if it's a
    ;; symbolic link.  Hence, we just create a copy.  To avoid
    ;; misunderstandings, we insert a comment at top and force
    ;; read-only file-locally.
    (insert "# DO NOT EDIT THIS FILE!\n"
            "# IT IS JUST A BUILD-TIME COPY OF lena/README.org!\n"
            "# EDIT THE ORIGINAL FILE INSTEAD!\n\n")
    (add-file-local-variable 'buffer-read-only t)
    (when (member "root-readme" build-targets)
      (write-file "../README.org"))))

;;;; Build target `abbreviations':

(defconst build-abbrev-alist
  (when (member "abbreviations" build-targets)
    ;; Drop abbreviations that contain a space or a backslash
    ;; character.  Lena's former Abbrev-based implementation could not
    ;; handle them, and the input method has kept the same set of
    ;; abbreviations.  As of writing this, ("+ " . "⊹") is the only
    ;; pair affected by the space character.
    (seq-remove
     (lambda (pair) (string-match-p "[ \\]" (car pair)))
     (with-temp-buffer
       (url-insert-file-contents
        (concat "https://raw.githubusercontent.com"
                "/leanprover/vscode-lean4/refs/heads/master"
                "/lean4-unicode-input/src/abbreviations.json"))
       (goto-char (point-min))
       (let ((json-key-type 'string))
         (json-read)))))
  "Raw abbreviations alist mapping strings to strings.

Values may include `$CURSOR'.")

(defconst build-abbrev-separator
  (regexp-quote "$CURSOR"))

;; Drop abbreviations whose expansion includes `$CURSOR', such as
;; \<> for ⟨⟩, because Quail cannot place point within an expansion.
(defconst build-abbrev-singletons
  (seq-remove (lambda (pair)
                (string-match-p build-abbrev-separator (cdr pair)))
              build-abbrev-alist))

;; For editing the strings that include Elisp code, it's best to
;; install the package `separedit', then type C-u M-x separedit-dwim
;; RET emacs-lisp-mode RET.

(when (member "abbreviations" build-targets)
  (with-temp-file "lena/lena-quail.el"
    (insert
     (format
      ";;; lena-quail.el --- Lena Input Method  -*- lexical-binding: t; -*-

;; Copyright © 2026 Mekeor Melire

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see LICENSE.GPL3.

;;; Commentary:

;; This file has been generated by `build.el' by relying on data from
;; the official Lean plugin for another editor.  Please do not edit
;; this file directly; edit `build.el' instead!

;; This file uses Emacs' built-in Quail library to define the input
;; method `Lena' for conveniently inserting Unicode characters that
;; are common in Lean code.

;; For more information on Emacs' input methods, consult the
;; corresponding Info manual:
;;
;;   (info \"(emacs) Input Methods\")

;;; Code:

(require 'quail)

(quail-define-package
 \"Lena\" ;; name
 \"UTF-8\" ;; language
 \"Lena\" ;; title
 nil ;; guidance
 \"TeX-like input method for Lean language and `lena-mode'.\" ;; doc
 nil ;; translation-keys
 nil ;; forget-last-selection
 nil ;; deterministic
 nil ;; kbd-translate
 nil ;; show-layout
 nil ;; create-decode-map
 nil ;; maximum-shortest
 nil ;; overlay-plist
 nil ;; update-translation-function
 nil ;; conversion-keys
 nil ;; simple
 )

(quail-define-rules
  %s)

(provide 'lena-quail)

;;; lena-quail.el ends here

;; %s:
;; buffer-read-only: t
;; End:
"
      (let ((max (1- (length build-abbrev-singletons))))
        (string-join
         (seq-map-indexed
          (lambda (pair index)
            (format "%s(%S [%S])"
                    (if (eq 0 index) "" "  ")
                    (concat "\\" (car pair))
                    (cdr pair)))
          build-abbrev-singletons)
         "\n"))
      "Local Variables"))))


;;;; Build targets `test' and `test-live':

(when (or (member "test" build-targets)
          (member "test-live" build-targets))
  (require 'ert)
  (add-to-list 'load-path (file-truename "lena"))
  (add-to-list 'load-path (file-truename "lena-evil"))
  (add-to-list 'load-path (file-truename "test"))
  ;; Test files provide features named after them and may require
  ;; each other.
  (dolist (file (directory-files "test" nil (rx "-tests.el" eos)))
    (require (intern (file-name-base file))))
  (when (member "test-live" build-targets)
    (dolist (file (directory-files "test" nil (rx "-live.el" eos)))
      (require (intern (file-name-base file)))))
  (ert-run-tests-batch-and-exit))

;;; build ends here

;; Local Variables:
;; no-byte-compile: t
;; outline-regexp: "^;;;; Build target"
;; End:
