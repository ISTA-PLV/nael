;;; nael-evil-tests.el --- Tests for nael-evil  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see nael/LICENSE.GPL3.

;;; Commentary:

;; Tests for the Evil bindings of the infoview.  They are skipped unless
;; Evil is in `load-path'.

;;; Code:

(require 'ert)
(require 'nael-evil)
(require 'nael-infoview-tests)

(ert-deftest nael-evil-test-keys ()
  "In Evil's normal state, the infoview and its help buffer have their keys."
  (unless (require 'evil nil t)
    (ert-skip "Evil is not installed"))
  (nael-evil-setup)
  (evil-set-leader 'normal (kbd ",") 'localleader)
  (nael-infoview-test-with-goals
    (evil-local-mode)
    (evil-normal-state)
    (pcase-dolist (`(,key . ,command)
                   '(("]g" . nael-infoview-next-goal)
                     ("[g" . nael-infoview-previous-goal)
                     ("]h" . outline-next-visible-heading)
                     ("gr" . nael-infoview-refresh)
                     ("K" . nael-infoview-describe)
                     ("q" . quit-window)
                     (", v" . nael-infoview-view-options)
                     (", y" . nael-infoview-copy-goal)))
      (should (eq (key-binding (kbd key)) command)))
    ;; Subterms keep their own keys.
    (nael-infoview-test-goto "IProp GF")
    (should (eq (key-binding (kbd "RET")) #'nael-infoview-describe)))
  (with-current-buffer (nael-infoview--help-buffer)
    (unwind-protect
        (progn
          (evil-local-mode)
          (evil-normal-state)
          (should (eq (key-binding (kbd "K")) #'nael-infoview-describe)))
      (kill-buffer))))

(provide 'nael-evil-tests)

;;; nael-evil-tests.el ends here
