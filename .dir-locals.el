;; -*- no-byte-compile: t; -*-

((nil
  (bug-reference-url-format
   . "https://github.com/ISTA-PLV/nael/issues/%s")
  (compile-command
   . "./build.el")
  (sentence-end-double-space
   . t))
 ;; Lisp follows Emacs' convention of two spaces after a sentence; the
 ;; Org documents use one.
 (org-mode
  (sentence-end-double-space
   . nil)))
