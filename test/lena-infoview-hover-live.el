;;; lena-infoview-hover-live.el --- Tests for lena-infoview-hover against Lean  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see lena/LICENSE.GPL3.

;;; Commentary:

;; Tests for information on subterms in the infoview against a real
;; Lean server on the Lake project in `test/lean/'.  They reuse the
;; setup of `lena-infoview-live'.
;;
;; Run with: emacs -Q --batch -l build.el test-live

;;; Code:

(require 'ert)
(require 'lena-infoview-live)
(require 'lena-test-helpers)

(defvar lena-infoview-hover-live-elide-buffer nil)

(defun lena-infoview-hover-live-elide-buffer ()
  "Return the buffer of `Tiny/Elide.lean', with the infoview following it."
  (lena-infoview-live-open)
  (unless (buffer-live-p lena-infoview-hover-live-elide-buffer)
    (setq lena-infoview-hover-live-elide-buffer
          (find-file-noselect (concat lena-rpc-live-root "Tiny/Elide.lean")))
    (with-current-buffer lena-infoview-hover-live-elide-buffer
      (lena-mode)
      (eglot-ensure)
      (should (lena-rpc-live-wait #'eglot-managed-p 30))))
  (with-current-buffer lena-infoview-hover-live-elide-buffer
    (should (lena-rpc-live-wait #'lena-rpc-live-processed-p 60)))
  (set-buffer lena-infoview-hover-live-elide-buffer)
  (switch-to-buffer lena-infoview-hover-live-elide-buffer)
  lena-infoview-hover-live-elide-buffer)

(defun lena-infoview-hover-live-eldoc (string)
  "Return the echo of ElDoc on the first STRING in the infoview."
  (with-current-buffer (lena-infoview--buffer)
    (lena-test-goto string)
    (let (echo)
      (should (lena-infoview-eldoc-function
               (lambda (_doc &rest keys)
                 (setq echo (substring-no-properties
                             (plist-get keys :echo))))))
      (should (lena-rpc-live-wait (lambda () echo) 10))
      echo)))

(ert-deftest lena-infoview-hover-live-elided ()
  "ElDoc on an elided subterm, `⋯', shows the subterm."
  (with-current-buffer (lena-infoview-hover-live-elide-buffer)
    (pcase-dolist (`(,line ,goal ,hover)
                   '((5 "⊢ ⟨↑x, ⋯⟩ = x" "x.isLt : ↑x < 3")
                     (9 "⊢ p = ⟨p.val, ⋯⟩" "p.property : p.val > 0")
                     (15 "⊢ f (f ⋯) = f (f ⋯)" "f (f (f ⋯)) : Nat")))
      (lena-infoview-live-move line 2)
      (should (lena-infoview-live-wait-for goal))
      (lena-infoview-live-settle)
      (should (equal (lena-infoview-hover-live-eldoc "⋯") hover)))))

(ert-deftest lena-infoview-hover-live-eldoc-doc ()
  "ElDoc's documentation of a constant includes its docstring."
  (with-current-buffer (lena-infoview-live-open)
    (with-current-buffer (lena-infoview--buffer)
      (lena-test-goto "Nat")
      (let (doc)
        (lena-infoview-eldoc-function
         (lambda (string &rest _) (setq doc (substring-no-properties string))))
        (should (lena-rpc-live-wait (lambda () doc) 10))
        (should (string-prefix-p "Nat : Type\n\n" doc))
        (should (string-search "natural numbers" doc))))))

(defun lena-infoview-hover-live-help ()
  "Return the text of the help buffer."
  (with-current-buffer lena-infoview-help-buffer-name
    (buffer-substring-no-properties (point-min) (point-max))))

(defun lena-infoview-hover-live-describe (string)
  "Describe the first STRING in the current buffer; wait for the help buffer."
  (let ((before (and (get-buffer lena-infoview-help-buffer-name) (lena-infoview-hover-live-help))))
    (lena-test-goto string)
    (lena-infoview-describe)
    (should (lena-rpc-live-wait
             (lambda () (and (get-buffer lena-infoview-help-buffer-name)
                             (not (equal (lena-infoview-hover-live-help)
                                         before))))
             10))))

(ert-deftest lena-infoview-hover-live-describe ()
  "Subterms are described in the help buffer, and theirs in turn."
  (with-current-buffer (lena-infoview-live-open)
    (unwind-protect
        (progn
          (with-current-buffer (lena-infoview--buffer)
            (lena-infoview-hover-live-describe "b"))
          (should (string-prefix-p "b : Nat\n\nFrom the goals at A.lean:4:2."
                                   (lena-infoview-hover-live-help)))
          (with-current-buffer lena-infoview-help-buffer-name
            (lena-infoview-hover-live-describe "Nat")
            (should (string-prefix-p "Nat : Type\n"
                                     (lena-infoview-hover-live-help)))
            (should (string-search "natural numbers"
                                   (lena-infoview-hover-live-help)))
            (help-go-back)
            (should (string-prefix-p "b : Nat\n"
                                     (lena-infoview-hover-live-help)))))
      (kill-buffer lena-infoview-help-buffer-name))))

(ert-deftest lena-infoview-hover-live-describe-released ()
  "A subterm whose reference was released is reported as gone."
  (with-current-buffer (lena-infoview-live-open)
    (unwind-protect
        (progn
          (with-current-buffer (lena-infoview--buffer)
            (lena-infoview-hover-live-describe "b"))
          (let ((page (car lena-infoview--pages))
                messages)
            (lena-rpc-owner-release (lena-infoview--page-owner page))
            (lena-rpc--flush-garbage
             (lena-rpc-handle-session (lena-infoview--page-handle page)))
            (cl-letf (((symbol-function 'message)
                       (lambda (&rest args)
                         (push (apply #'format-message args) messages))))
              (with-current-buffer lena-infoview-help-buffer-name
                (lena-test-goto "Nat")
                (lena-infoview-describe))
              (should (lena-rpc-live-wait (lambda () messages) 10)))
            (should (string-prefix-p "Lean no longer knows this subterm"
                                     (car messages)))))
      (kill-buffer lena-infoview-help-buffer-name))))

(ert-deftest lena-infoview-hover-live-definition ()
  "`xref-find-definitions' on a subterm goes to its definition, and back."
  (with-current-buffer (lena-infoview-live-open)
    (let ((infoview (lena-infoview--buffer)))
      (with-current-buffer infoview
        (switch-to-buffer infoview)
        (lena-test-goto "b")
        ;; As the command loop does; otherwise xref asks for the identifier.
        (let ((this-command #'xref-find-definitions))
          (call-interactively #'xref-find-definitions))
        (unwind-protect
            (progn
              (should (equal (file-name-nondirectory buffer-file-name)
                             "B.lean"))
              (should (looking-at "b : Nat := 1"))
              (xref-go-back)
              (should (eq (current-buffer) infoview))
              (should (looking-at "b")))
          (kill-buffer (find-buffer-visiting
                        (concat lena-rpc-live-root "Tiny/B.lean"))))))))

(provide 'lena-infoview-hover-live)

;;; lena-infoview-hover-live.el ends here
