;;; nael-infoview-hover-live.el --- Tests for nael-infoview-hover against Lean  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see nael/LICENSE.GPL3.

;;; Commentary:

;; Tests for information on subterms in the infoview against a real
;; Lean server on the Lake project in `test/lean/'.  They reuse the
;; setup of `nael-infoview-live'.
;;
;; Run with: emacs -Q --batch -l build.el test-live

;;; Code:

(require 'ert)
(require 'nael-infoview-live)
(require 'nael-infoview-tests)

(defvar nael-infoview-hover-live-elide-buffer nil)

(defun nael-infoview-hover-live-elide-buffer ()
  "Return the buffer of `Tiny/Elide.lean', with the infoview following it."
  (nael-infoview-live-open)
  (unless (buffer-live-p nael-infoview-hover-live-elide-buffer)
    (setq nael-infoview-hover-live-elide-buffer
          (find-file-noselect (concat nael-rpc-live-root "Tiny/Elide.lean")))
    (with-current-buffer nael-infoview-hover-live-elide-buffer
      (nael-mode)
      (eglot-ensure)
      (should (nael-rpc-live-wait #'eglot-managed-p 30))))
  (with-current-buffer nael-infoview-hover-live-elide-buffer
    (should (nael-rpc-live-wait #'nael-rpc-live-processed-p 60)))
  (set-buffer nael-infoview-hover-live-elide-buffer)
  (switch-to-buffer nael-infoview-hover-live-elide-buffer)
  nael-infoview-hover-live-elide-buffer)

(defun nael-infoview-hover-live-eldoc (string)
  "Return the echo of ElDoc on the first STRING in the infoview."
  (with-current-buffer (nael-infoview--buffer)
    (nael-infoview-test-goto string)
    (let (echo)
      (should (nael-infoview-eldoc-function
               (lambda (_doc &rest keys)
                 (setq echo (substring-no-properties
                             (plist-get keys :echo))))))
      (should (nael-rpc-live-wait (lambda () echo) 10))
      echo)))

(ert-deftest nael-infoview-hover-live-elided ()
  "ElDoc on an elided subterm, `⋯', shows the subterm."
  (with-current-buffer (nael-infoview-hover-live-elide-buffer)
    (pcase-dolist (`(,line ,goal ,hover)
                   '((5 "⊢ ⟨↑x, ⋯⟩ = x" "x.isLt : ↑x < 3")
                     (9 "⊢ p = ⟨p.val, ⋯⟩" "p.property : p.val > 0")
                     (15 "⊢ f (f ⋯) = f (f ⋯)" "f (f (f ⋯)) : Nat")))
      (nael-infoview-live-move line 2)
      (should (nael-infoview-live-wait-for goal))
      (nael-infoview-live-settle)
      (should (equal (nael-infoview-hover-live-eldoc "⋯") hover)))))

(ert-deftest nael-infoview-hover-live-eldoc-doc ()
  "ElDoc's documentation of a constant includes its docstring."
  (with-current-buffer (nael-infoview-live-open)
    (with-current-buffer (nael-infoview--buffer)
      (nael-infoview-test-goto "Nat")
      (let (doc)
        (nael-infoview-eldoc-function
         (lambda (string &rest _) (setq doc (substring-no-properties string))))
        (should (nael-rpc-live-wait (lambda () doc) 10))
        (should (string-prefix-p "Nat : Type\n\n" doc))
        (should (string-search "natural numbers" doc))))))

(provide 'nael-infoview-hover-live)

;;; nael-infoview-hover-live.el ends here
