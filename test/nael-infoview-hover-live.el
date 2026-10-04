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

(defun nael-infoview-hover-live-help ()
  "Return the text of the *Help* buffer."
  (with-current-buffer "*Help*"
    (buffer-substring-no-properties (point-min) (point-max))))

(defun nael-infoview-hover-live-describe (string)
  "Describe the first STRING in the current buffer; wait for *Help*."
  (let ((before (and (get-buffer "*Help*") (nael-infoview-hover-live-help))))
    (nael-infoview-test-goto string)
    (nael-infoview-describe)
    (should (nael-rpc-live-wait
             (lambda () (and (get-buffer "*Help*")
                             (not (equal (nael-infoview-hover-live-help)
                                         before))))
             10))))

(ert-deftest nael-infoview-hover-live-describe ()
  "Subterms are described in the *Help* buffer, and theirs in turn."
  (with-current-buffer (nael-infoview-live-open)
    (unwind-protect
        (progn
          (with-current-buffer (nael-infoview--buffer)
            (nael-infoview-hover-live-describe "b"))
          (should (string-prefix-p "b : Nat\n\nFrom the goals at A.lean:4:2."
                                   (nael-infoview-hover-live-help)))
          (with-current-buffer "*Help*"
            (nael-infoview-hover-live-describe "Nat")
            (should (string-prefix-p "Nat : Type\n"
                                     (nael-infoview-hover-live-help)))
            (should (string-search "natural numbers"
                                   (nael-infoview-hover-live-help)))
            (help-go-back)
            (should (string-prefix-p "b : Nat\n"
                                     (nael-infoview-hover-live-help)))))
      (kill-buffer "*Help*"))))

(ert-deftest nael-infoview-hover-live-describe-released ()
  "A subterm whose reference was released is reported as gone."
  (with-current-buffer (nael-infoview-live-open)
    (unwind-protect
        (progn
          (with-current-buffer (nael-infoview--buffer)
            (nael-infoview-hover-live-describe "b"))
          (let ((page (car nael-infoview--pages))
                messages)
            (nael-rpc-owner-release (nael-infoview--page-owner page))
            (nael-rpc--flush-garbage
             (nael-rpc-handle-session (nael-infoview--page-handle page)))
            (cl-letf (((symbol-function 'message)
                       (lambda (&rest args)
                         (push (apply #'format-message args) messages))))
              (with-current-buffer "*Help*"
                (nael-infoview-test-goto "Nat")
                (nael-infoview-describe))
              (should (nael-rpc-live-wait (lambda () messages) 10)))
            (should (string-prefix-p "Lean no longer knows this subterm"
                                     (car messages)))))
      (kill-buffer "*Help*"))))

(ert-deftest nael-infoview-hover-live-definition ()
  "`xref-find-definitions' on a subterm goes to its definition, and back."
  (with-current-buffer (nael-infoview-live-open)
    (let ((infoview (nael-infoview--buffer)))
      (with-current-buffer infoview
        (switch-to-buffer infoview)
        (nael-infoview-test-goto "b")
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
                        (concat nael-rpc-live-root "Tiny/B.lean"))))))))

(provide 'nael-infoview-hover-live)

;;; nael-infoview-hover-live.el ends here
