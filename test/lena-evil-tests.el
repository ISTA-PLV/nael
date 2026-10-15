;;; lena-evil-tests.el --- Tests for lena-evil  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see lena/LICENSE.GPL3.

;;; Commentary:

;; Tests for the Evil bindings of the infoview.  They are skipped unless
;; Evil is in `load-path'.

;;; Code:

(require 'ert)
(require 'lena-evil)
(require 'lena-test-helpers)

(ert-deftest lena-evil-test-keys ()
  "In Evil's normal state, the infoview and its help buffer have their keys."
  (unless (require 'evil nil t)
    (ert-skip "Evil is not installed"))
  (lena-evil-setup)
  (evil-set-leader 'normal (kbd ",") 'localleader)
  (lena-test-with-goals
    (evil-local-mode)
    (evil-normal-state)
    (pcase-dolist (`(,key . ,command)
                   '(("]g" . lena-infoview-next-goal)
                     ("[g" . lena-infoview-previous-goal)
                     ("]h" . outline-next-visible-heading)
                     ("gr" . lena-infoview-refresh)
                     ("K" . lena-infoview-describe)
                     ("q" . quit-window)
                     (", v" . lena-infoview-view-options)
                     (", y" . lena-infoview-copy-goal)))
      (should (eq (key-binding (kbd key)) command)))
    ;; Subterms keep their own keys.
    (lena-test-goto "IProp GF")
    (should (eq (key-binding (kbd "RET")) #'lena-infoview-describe)))
  (with-current-buffer (lena-infoview--help-buffer)
    (unwind-protect
        (progn
          (evil-local-mode)
          (evil-normal-state)
          (should (eq (key-binding (kbd "K")) #'lena-infoview-describe)))
      (kill-buffer))))

(provide 'lena-evil-tests)

;;; lena-evil-tests.el ends here
