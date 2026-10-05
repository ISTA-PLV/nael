;;; nael-infoview-tests.el --- Tests for nael-infoview  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see nael/LICENSE.GPL3.

;;; Commentary:

;; Tests for the infoview buffer of `nael-infoview' that need no Lean
;; server: recorded goals are shown directly.

;;; Code:

(require 'ert)
(require 'nael-infoview)
(require 'nael-render-tests)

(defmacro nael-infoview-test-with-options (&rest body)
  "Run BODY with the default view options of the infoview."
  (declare (indent 0) (debug t))
  `(let ((nael-infoview-hide-instances nil)
         (nael-infoview-hide-types nil)
         (nael-infoview-hide-inaccessible nil)
         (nael-infoview-hide-let-values nil)
         (nael-infoview-reverse nil)
         (nael-infoview-show-diffs t)
         (nael-infoview-expected-type 'expanded))
     ,@body))

(defmacro nael-infoview-test-with-goals (&rest body)
  "Run BODY in the infoview showing the recorded goals of SpinLock."
  (declare (indent 0) (debug t))
  `(nael-infoview-test-with-options
     (unwind-protect
         (progn
           (nael-infoview--show (nael-render-test-fixture) nil
                                (nael-rpc-make-owner) "SpinLock.lean:110:2")
           (with-current-buffer (nael-infoview--buffer)
             (switch-to-buffer (current-buffer))
             (goto-char (point-min))
             ,@body))
       (when-let* ((buffer (nael-infoview--buffer)))
         (kill-buffer buffer)))))

(defun nael-infoview-test-goto (string)
  "Move point to the start of the first occurrence of STRING."
  (goto-char (point-min))
  (search-forward string)
  (goto-char (match-beginning 0)))

(ert-deftest nael-infoview-test-show ()
  (nael-infoview-test-with-goals
    (should (string-prefix-p "Tactic state: 3 goals\ngoal 1\n"
                             (buffer-string)))
    (should (= (seq-count (lambda (overlay)
                            (and (overlay-get overlay 'mouse-face)
                                 (not (overlay-get overlay 'outline-button))))
                          (overlays-in (point-min) (point-max)))
               63))
    (should (string-search "SpinLock.lean:110:2"
                           (nael-infoview--mode-line-process)))
    (should-not header-line-format)))

(ert-deftest nael-infoview-test-own-overlays ()
  "Rendering again replaces the infoview's overlays, and only those."
  (nael-infoview-test-with-goals
    (cl-flet ((count (property)
                (seq-count (lambda (overlay) (overlay-get overlay property))
                           (overlays-in (point-min) (point-max)))))
      (let ((foreign (make-overlay 1 5))
            (nodes (count 'nael-infoview-node))
            (buttons (count 'outline-button)))
        (should (> nodes 0))
        (should (> buttons 0))
        (nael-infoview-test-goto "IProp GF")
        (nael-infoview--highlight-point)
        (nael-infoview-toggle-types)
        (nael-infoview-toggle-types)
        (should (overlay-buffer foreign))
        (should (= (count 'nael-infoview-node) nodes))
        (should (= (count 'outline-button) buttons))
        (should (= (seq-count (lambda (overlay)
                                (eq (overlay-get overlay 'face)
                                    'nael-infoview-point-node))
                              (overlays-in (point-min) (point-max)))
                   1))))))

(ert-deftest nael-infoview-test-wrap ()
  "Long lines wrap in the infoview, even where Emacs truncates by default."
  (let ((default (default-value 'truncate-lines)))
    (setq-default truncate-lines t)
    (unwind-protect
        (nael-infoview-test-with-goals
          (should-not truncate-lines)
          (should word-wrap)
          (should (get-text-property (point-min) 'wrap-prefix))
          (with-current-buffer (nael-infoview--help-buffer)
            (unwind-protect
                (progn (should-not truncate-lines)
                       (should word-wrap))
              (kill-buffer))))
      (setq-default truncate-lines default))))

(ert-deftest nael-infoview-test-fold-survives-render ()
  "Folded goals and sections stay folded when the goals are shown again."
  (nael-infoview-test-with-goals
    (nael-infoview-test-goto "goal 2")
    (outline-hide-subtree)
    (should (outline-invisible-p (line-end-position)))
    (should (equal (nael-infoview--folded-keys) '((goal . 1))))
    ;; Render again, as a filter toggle or a refresh does.
    (nael-infoview-toggle-instances)
    (nael-infoview-test-goto "goal 2")
    (should (outline-invisible-p (line-end-position)))
    (nael-infoview-test-goto "goal 1")
    (should-not (outline-invisible-p (line-end-position)))
    (nael-infoview-test-goto "goal 3")
    (should-not (outline-invisible-p (line-end-position)))))

(defun nael-infoview-test-button ()
  "Return the icon of the outline button of the heading at point, or nil.
In the margin, the button is a space displaying the icon there."
  (when-let* ((overlay (seq-find (lambda (o) (overlay-get o 'outline-button))
                                 (overlays-at (line-beginning-position))))
              (string (overlay-get overlay 'before-string)))
    (pcase (get-text-property 0 'display string)
      (`((margin left-margin) ,icon) icon))))

(ert-deftest nael-infoview-test-fold-buttons ()
  "Every foldable heading has a button that shows whether it is folded."
  (nael-infoview-test-with-goals
    ;; The buttons are separated from the text by a blank column.
    (should (= left-margin-width 2))
    (dolist (heading '("Tactic state" "goal 1" "goal 2" "goal 3"))
      (nael-infoview-test-goto heading)
      (should (nael-infoview-test-button)))
    (nael-infoview-test-goto "hlc : HasLC")
    (should-not (nael-infoview-test-button))
    (nael-infoview-test-goto "goal 2")
    (let ((open (nael-infoview-test-button)))
      (outline-hide-subtree)
      (should-not (equal (nael-infoview-test-button) open))
      ;; Still closed after rendering again.
      (nael-infoview-toggle-types)
      (nael-infoview-test-goto "goal 2")
      (should-not (equal (nael-infoview-test-button) open)))))

(defun nael-infoview-test-renumber (goals suffix)
  "Return GOALS with fresh metavariable ids ending in SUFFIX.
That is what the goals look like after the next tactic."
  (list :goals (vconcat
                (mapcar (lambda (goal)
                          (plist-put (copy-sequence goal) :mvarId
                                     (concat (plist-get goal :mvarId) suffix)))
                        (plist-get goals :goals)))))

(ert-deftest nael-infoview-test-fold-survives-tactic ()
  "A folded goal stays folded at the same place after the next tactic."
  (nael-infoview-test-with-goals
    (nael-infoview-test-goto "goal 2")
    (outline-hide-subtree)
    (nael-infoview--show (nael-infoview-test-renumber
                          (nael-render-test-fixture) ".next")
                         nil (nael-rpc-make-owner) "SpinLock.lean:111:2")
    (nael-infoview-test-goto "goal 2")
    (should (outline-invisible-p (line-end-position)))
    (nael-infoview-test-goto "goal 1")
    (should-not (outline-invisible-p (line-end-position)))))

(ert-deftest nael-infoview-test-fold-case-by-name ()
  "A folded case stays folded where it moves, an unfolded one open."
  (nael-infoview-test-with-options
   (let ((inl (nael-render-test-goal :userName "inl"))
        (inr (nael-render-test-goal :userName "inr")))
    (unwind-protect
        (progn
          (nael-infoview--show (list :goals (vector inl inr)) nil
                               (nael-rpc-make-owner) "x")
          (with-current-buffer (nael-infoview--buffer)
            (nael-infoview-test-goto "case inr")
            (outline-hide-subtree)
            ;; `inl' is solved; `inr' is now the first goal.
            (nael-infoview--show (list :goals (vector inr)) nil
                                 (nael-rpc-make-owner) "x")
            (nael-infoview-test-goto "case inr")
            (should (outline-invisible-p (line-end-position)))
            (nael-infoview--show (list :goals (vector inl inr)) nil
                                 (nael-rpc-make-owner) "x")
            (nael-infoview-test-goto "case inl")
            (should-not (outline-invisible-p (line-end-position)))))
      (kill-buffer (nael-infoview--buffer))))))

(ert-deftest nael-infoview-test-filters ()
  (nael-infoview-test-with-goals
    (should (string-search "inst✝" (buffer-string)))
    (nael-infoview-toggle-instances)
    (should-not (string-search "inst✝" (buffer-string)))
    (should nael-infoview-hide-instances)
    ;; The menu shows the state of the filters.
    (let ((item (assq (intern "Hide instances")
                      (lookup-key nael-infoview-mode-map [menu-bar Infoview]))))
      (should (equal (plist-get (nthcdr 4 item) :button)
                     '(:toggle . nael-infoview-hide-instances))))
    (nael-infoview-toggle-instances)
    (should (string-search "inst✝" (buffer-string)))))

(ert-deftest nael-infoview-test-view-options ()
  "A menu shows and changes the view options."
  (nael-infoview-test-with-goals
    (should (eq (key-binding "v") #'nael-infoview-view-options))
    (should (transient-get-suffix 'nael-infoview-view-options "i"))
    (should (equal (nael-infoview--switch-description "Hide types" t)
                   "Hide types on"))
    (should (eq (get-text-property 11 'face (nael-infoview--switch-description
                                             "Hide types" t))
                'transient-value))
    (cl-flet ((current ()
                (let ((description (nael-infoview--expected-type-description)))
                  (seq-find (lambda (value)
                              (let ((i (string-search (symbol-name value)
                                                      description)))
                                (eq (get-text-property i 'face description)
                                    'transient-value)))
                            '(expanded collapsed hidden)))))
      (should (eq (current) 'expanded))
      (nael-infoview-cycle-expected-type)
      (should (eq nael-infoview-expected-type 'collapsed))
      (should (eq (current) 'collapsed))
      (nael-infoview-cycle-expected-type)
      (should (eq (current) 'hidden))
      (nael-infoview-cycle-expected-type)
      (should (eq (current) 'expanded)))))

(ert-deftest nael-infoview-test-toggle-diffs ()
  "Diffs can be switched off and on."
  (nael-infoview-test-with-goals
    (cl-flet ((marked-p ()
                (text-property-search-forward
                 'face 'nael-render-diff-removed
                 (lambda (face faces) (memq face (ensure-list faces))))))
      (goto-char (point-min))
      (should (marked-p))
      (nael-infoview-toggle-diffs)
      (goto-char (point-min))
      (should-not (marked-p))
      (nael-infoview-toggle-diffs)
      (goto-char (point-min))
      (should (marked-p)))))

(ert-deftest nael-infoview-test-expected-type-collapsed ()
  "A collapsed expected type starts folded and stays as the user leaves it."
  (nael-infoview-test-with-options
    (let ((nael-infoview-expected-type 'collapsed)
          (term-goal (list :hyps [] :type (nael-render-test-code "Nat"))))
      (unwind-protect
          (progn
            (nael-infoview--show nil term-goal (nael-rpc-make-owner) "x")
            (with-current-buffer (nael-infoview--buffer)
              (nael-infoview-test-goto "Expected type")
              (should (outline-invisible-p (line-end-position)))
              (outline-show-subtree)
              (nael-infoview--show nil term-goal (nael-rpc-make-owner) "x")
              (nael-infoview-test-goto "Expected type")
              (should-not (outline-invisible-p (line-end-position)))
              ;; Gone and back: folded again.
              (nael-infoview--show nil nil (nael-rpc-make-owner) "x")
              (nael-infoview--show nil term-goal (nael-rpc-make-owner) "x")
              (nael-infoview-test-goto "Expected type")
              (should (outline-invisible-p (line-end-position)))))
        (kill-buffer (nael-infoview--buffer))))))

(ert-deftest nael-infoview-test-eldoc-commands ()
  "ElDoc shows the subterm at point after commands that return to it."
  (dolist (command '(nael-infoview-next-goal quit-window xref-go-back
                     evil-jump-backward +popup/quit-window
                     better-jumper-jump-backward))
    (should (eldoc--message-command-p command))))

(ert-deftest nael-infoview-test-goal-navigation ()
  (nael-infoview-test-with-goals
    (nael-infoview-next-goal)
    (should (looking-at "goal 1"))
    (nael-infoview-next-goal)
    (should (looking-at "goal 2"))
    (forward-line 2)
    (nael-infoview-previous-goal)
    (should (looking-at "goal 2"))
    (nael-infoview-previous-goal)
    (should (looking-at "goal 1"))
    (should-error (nael-infoview-previous-goal) :type 'user-error)
    (nael-infoview-next-goal 2)
    (should (looking-at "goal 3"))
    (should-error (nael-infoview-next-goal) :type 'user-error)))

(ert-deftest nael-infoview-test-copy-goal ()
  (nael-infoview-test-with-goals
    (nael-infoview-test-goto "goal 3")
    (forward-line 3)
    (let ((kill-ring nil))
      (nael-infoview-copy-goal)
      (should (string-prefix-p "goal 3\nhlc : HasLC\n" (car kill-ring)))
      (should (string-suffix-p "⊢ GName" (car kill-ring)))
      (should-not (text-properties-at 0 (car kill-ring))))))

(ert-deftest nael-infoview-test-point-highlight ()
  "The innermost subterm at point is highlighted, and only that."
  (nael-infoview-test-with-goals
    (nael-infoview-test-goto "IProp GF")
    (search-forward "G")
    (nael-infoview--highlight-point)
    (let ((overlay nael-infoview--point-overlay)
          (node (nael-render-node-at (point))))
      (should (equal (buffer-substring-no-properties
                      (overlay-start overlay) (overlay-end overlay))
                     "GF"))
      ;; Buffer positions are the renderer's positions.
      (should (= (overlay-start overlay) (nael-render-node-beg node))))
    (goto-char (point-min))
    (nael-infoview--highlight-point)
    (should-not nael-infoview--point-overlay)))

(ert-deftest nael-infoview-test-window-anchor ()
  "The window keeps its place in a goal when the goals are rendered again."
  (nael-infoview-test-with-goals
    (nael-infoview-test-goto "goal 3")
    (forward-line 2)
    (forward-char 3)
    (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                (point))))
      ;; Hiding instances removes lines above, in goals 1 to 3.
      (nael-infoview-toggle-instances)
      (should (equal (buffer-substring-no-properties
                      (line-beginning-position) (point))
                     line))
      (should (equal (get-text-property (point) 'nael-goal)
                     (save-excursion (nael-infoview-test-goto "goal 3")
                                     (get-text-property (point)
                                                        'nael-goal)))))))

;;;; Windows:

(defmacro nael-infoview-test-with-windows (&rest body)
  "Run BODY with the infoview displayed in its side window, selected.
The main window shows the buffer Main."
  (declare (indent 0) (debug t))
  `(nael-infoview-test-with-goals
     (let ((main (get-buffer-create "Main")))
       (unwind-protect
           (progn
             (dolist (window (window-list))
               (when (window-parameter window 'window-side)
                 (delete-window window)))
             (delete-other-windows)
             (switch-to-buffer main)
             (setq nael-infoview--window
                   (display-buffer (nael-infoview--buffer)
                                   nael-infoview-display-action))
             (select-window nael-infoview--window)
             ,@body)
         (dolist (window (window-list))
           (when (window-parameter window 'window-side)
             (delete-window window)))
         (setq nael-infoview--window nil)
         (kill-buffer main)))))

(ert-deftest nael-infoview-test-jump-out ()
  "A jump from the infoview shows its target in another window."
  (nael-infoview-test-with-windows
    (let ((target (get-buffer-create "Target")))
      (unwind-protect
          (progn
            ;; As `xref-find-definitions' does.
            (switch-to-buffer target)
            (should (eq (window-buffer nael-infoview--window)
                        (nael-infoview--buffer)))
            (should (eq (window-buffer) target))
            (should-not (eq (selected-window) nael-infoview--window)))
        (kill-buffer target)))))

(ert-deftest nael-infoview-test-jump-back ()
  "Going back to the infoview selects its window, not a second one."
  (nael-infoview-test-with-windows
    (let ((home nael-infoview--window)
          (pos (save-excursion (nael-infoview-test-goto "goal 2") (point))))
      (select-window (get-buffer-window "Main"))
      ;; As `xref-go-back' does, then the command loop.
      (switch-to-buffer (nael-infoview--buffer))
      (goto-char pos)
      (nael-infoview--keep-window)
      (should (eq (selected-window) home))
      (should (= (point) pos))
      (should (equal (sort (mapcar #'buffer-name
                                   (mapcar #'window-buffer (window-list)))
                           #'string<)
                     (list "*Nael Infoview*" "Main")))
      ;; Nothing happens in the infoview's own window.
      (nael-infoview--keep-window)
      (should (eq (selected-window) home)))))

(provide 'nael-infoview-tests)

;;; nael-infoview-tests.el ends here
