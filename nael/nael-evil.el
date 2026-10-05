;;; nael-evil.el --- Evil bindings for Nael's infoview  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see LICENSE.GPL3.

;;; Commentary:

;; Bindings for users of Evil in the infoview and in its help buffer.
;; In Evil's normal state, most of the infoview's own keys are motions;
;; these bindings follow the conventions of evil-collection, like `gr'
;; to refresh, and the infoview of lean.nvim, like `]g' for the next
;; goal:
;;
;;   ]g [g   next and previous goal
;;   ]h [h   next and previous heading
;;   gr      refresh
;;   K       describe the subterm at point
;;   q       quit
;;   <localleader> v   view options
;;   <localleader> y   copy the goal at point
;;
;; RET and `gd' work without them, as do Evil's folding commands, such as
;; `za'.  The local-leader keys need a local leader, see
;; `evil-set-leader'.
;;
;; Nothing is bound until `nael-evil-setup' is called, for instance:
;;
;;   (with-eval-after-load 'evil (nael-evil-setup))

;;; Code:

(require 'nael-infoview)

(declare-function evil-define-key* "evil-core")

;;;###autoload
(defun nael-evil-setup ()
  "Bind keys for Evil in Nael's infoview and its help buffer."
  (require 'evil)
  (evil-define-key* '(normal motion) nael-infoview-mode-map
    "]g" #'nael-infoview-next-goal
    "[g" #'nael-infoview-previous-goal
    "]h" #'outline-next-visible-heading
    "[h" #'outline-previous-visible-heading
    "gr" #'nael-infoview-refresh
    "K" #'nael-infoview-describe
    "q" #'quit-window
    (kbd "<localleader>v") #'nael-infoview-view-options
    (kbd "<localleader>y") #'nael-infoview-copy-goal)
  (evil-define-key* '(normal motion) nael-infoview-help-mode-map
    "K" #'nael-infoview-describe))

(provide 'nael-evil)

;;; nael-evil.el ends here
