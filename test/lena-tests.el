;;; nael-tests.el --- Tests for nael  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see nael/LICENSE.GPL3.

;;; Commentary:

;; Tests for the major mode's setup that need no Lean server.

;;; Code:

(require 'ert)
(require 'nael)

(ert-deftest nael-test-project-markers ()
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

(provide 'nael-tests)

;;; nael-tests.el ends here
