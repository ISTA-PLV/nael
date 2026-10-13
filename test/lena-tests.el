;;; lena-tests.el --- Tests for lena  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see lena/LICENSE.GPL3.

;;; Commentary:

;; Tests for the major mode's setup that need no Lean server.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'lena)

(ert-deftest lena-test-project-markers ()
  "Lake files mark the root of a project, for project.el and Projectile."
  (should (member "lakefile.toml" project-vc-extra-root-markers))
  (should (member "lakefile.lean" project-vc-extra-root-markers))
  ;; Projectile is not installed here; pretend that it is loaded now.
  (defvar projectile-project-root-files-bottom-up)
  (let ((projectile-project-root-files-bottom-up (list ".git"))
        (features features)
        ;; Otherwise the setup waits for the file being loaded, which
        ;; is build.el when the tests run.
        (load-file-name nil))
    (provide 'projectile)
    (should (member "lakefile.toml" projectile-project-root-files-bottom-up))
    (should (member "lakefile.lean" projectile-project-root-files-bottom-up))))

;;;; Input method:

(ert-deftest lena-test-input-method ()
  "Lean buffers start with the Lena input method on, unless told otherwise."
  (with-temp-buffer
    (lena-mode)
    (should (equal current-input-method "Lena"))
    (should (equal default-input-method "Lena")))
  (let ((lena-activate-quail nil))
    (with-temp-buffer
      (lena-mode)
      (should-not current-input-method)
      (should (equal default-input-method "Lena"))))
  (let ((lena-prepare-quail nil))
    (with-temp-buffer
      (lena-mode)
      (should-not current-input-method)
      (should-not (local-variable-p 'default-input-method)))))

(ert-deftest lena-test-input-method-prompt ()
  "A prompt entered from a Lean buffer toggles Lena, starting off."
  (let ((lean (generate-new-buffer " *lena-test-lean*"))
        (other (generate-new-buffer " *lena-test-other*"))
        (window (selected-window))
        (input-method-history (list "TeX")))
    (unwind-protect
        (cl-letf (((symbol-function 'minibuffer-selected-window)
                   (lambda () window)))
          (with-current-buffer lean (lena-mode))
          (set-window-buffer window lean)
          (with-temp-buffer
            (lena-quail-minibuffer-setup)
            (should-not current-input-method)
            (should (equal default-input-method "Lena"))
            (toggle-input-method)
            (should (equal current-input-method "Lena")))
          ;; Prompts from other buffers keep the global default.
          (set-window-buffer window other)
          (with-temp-buffer
            (lena-quail-minibuffer-setup)
            (should-not (local-variable-p 'default-input-method))
            (should (equal (car input-method-history) "TeX"))))
      (kill-buffer lean)
      (kill-buffer other))))

(provide 'lena-tests)

;;; lena-tests.el ends here
