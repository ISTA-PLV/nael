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
(require 'nael-test-helpers)

(ert-deftest nael-infoview-test-show ()
  (nael-test-with-goals
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
  (nael-test-with-goals
    (cl-flet ((count (property)
                (seq-count (lambda (overlay) (overlay-get overlay property))
                           (overlays-in (point-min) (point-max)))))
      (let ((foreign (make-overlay 1 5))
            (nodes (count 'nael-infoview-node))
            (buttons (count 'outline-button)))
        (should (> nodes 0))
        (should (> buttons 0))
        (nael-test-goto "IProp GF")
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
        (nael-test-with-goals
          (should-not truncate-lines)
          (should word-wrap)
          (should (get-text-property (point-min) 'wrap-prefix))
          (with-current-buffer (nael-infoview--help-buffer)
            (unwind-protect
                (progn (should-not truncate-lines)
                       (should word-wrap))
              (kill-buffer))))
      (setq-default truncate-lines default))))

(defun nael-infoview-test-button ()
  "Return the icon of the outline button of the heading at point, or nil.
In the margin, the button is a space displaying the icon there."
  (when-let* ((overlay (seq-find (lambda (o) (overlay-get o 'outline-button))
                                 (overlays-at (line-beginning-position))))
              (string (overlay-get overlay 'before-string)))
    (pcase (get-text-property 0 'display string)
      (`((margin left-margin) ,icon) icon))))

(ert-deftest nael-infoview-test-fold-buttons ()
  "Foldable headings have buttons, in the margin."
  (nael-test-with-goals
    (should (= left-margin-width 2))
    (nael-test-goto "goal 1")
    (should (nael-infoview-test-button))
    (nael-test-goto "hlc : HasLC")
    (should-not (nael-infoview-test-button))))

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
  (nael-test-with-goals
    (nael-test-goto "goal 2")
    (outline-hide-subtree)
    (nael-infoview--show (nael-infoview-test-renumber
                          (nael-test-spinlock-goals) ".next")
                         nil (nael-rpc-make-owner) "SpinLock.lean:111:2")
    (nael-test-goto "goal 2")
    (should (outline-invisible-p (line-end-position)))
    (nael-test-goto "goal 1")
    (should-not (outline-invisible-p (line-end-position)))))

(ert-deftest nael-infoview-test-fold-case-by-name ()
  "A folded case stays folded where it moves, an unfolded one open."
  (nael-test-with-options
   (let ((inl (nael-test-goal :userName "inl"))
        (inr (nael-test-goal :userName "inr")))
    (unwind-protect
        (progn
          (nael-infoview--show (list :goals (vector inl inr)) nil
                               (nael-rpc-make-owner) "x")
          (with-current-buffer (nael-infoview--buffer)
            (nael-test-goto "case inr")
            (outline-hide-subtree)
            ;; `inl' is solved; `inr' is now the first goal.
            (nael-infoview--show (list :goals (vector inr)) nil
                                 (nael-rpc-make-owner) "x")
            (nael-test-goto "case inr")
            (should (outline-invisible-p (line-end-position)))
            (nael-infoview--show (list :goals (vector inl inr)) nil
                                 (nael-rpc-make-owner) "x")
            (nael-test-goto "case inl")
            (should-not (outline-invisible-p (line-end-position)))))
      (kill-buffer (nael-infoview--buffer))))))

(ert-deftest nael-infoview-test-filters ()
  (nael-test-with-goals
    (should (string-search "inst✝" (buffer-string)))
    (nael-infoview-toggle-instances)
    (should-not (string-search "inst✝" (buffer-string)))
    (should nael-infoview-hide-instances)
    (nael-infoview-toggle-instances)
    (should (string-search "inst✝" (buffer-string)))))

(ert-deftest nael-infoview-test-view-options ()
  "A menu shows and changes the view options."
  (nael-test-with-goals
    (should (eq (key-binding "v") #'nael-infoview-view-options))
    (should (transient-get-suffix 'nael-infoview-view-options "i"))
    (should (equal (nael-infoview--switch-description "Hide types" t)
                   "Hide types on"))
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
  (nael-test-with-goals
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
  (nael-test-with-options
    (let ((nael-infoview-expected-type 'collapsed)
          (term-goal (list :hyps [] :type (nael-test-code "Nat"))))
      (unwind-protect
          (progn
            (nael-infoview--show nil term-goal (nael-rpc-make-owner) "x")
            (with-current-buffer (nael-infoview--buffer)
              (nael-test-goto "Expected type")
              (should (outline-invisible-p (line-end-position)))
              (outline-show-subtree)
              (nael-infoview--show nil term-goal (nael-rpc-make-owner) "x")
              (nael-test-goto "Expected type")
              (should-not (outline-invisible-p (line-end-position)))
              ;; Gone and back: folded again.
              (nael-infoview--show nil nil (nael-rpc-make-owner) "x")
              (nael-infoview--show nil term-goal (nael-rpc-make-owner) "x")
              (nael-test-goto "Expected type")
              (should (outline-invisible-p (line-end-position)))))
        (kill-buffer (nael-infoview--buffer))))))

(ert-deftest nael-infoview-test-goal-navigation ()
  (nael-test-with-goals
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
  (nael-test-with-goals
    (nael-test-goto "goal 3")
    (forward-line 3)
    (let ((kill-ring nil))
      (nael-infoview-copy-goal)
      (should (string-prefix-p "goal 3\nhlc : HasLC\n" (car kill-ring)))
      (should (string-suffix-p "⊢ GName" (car kill-ring)))
      (should-not (text-properties-at 0 (car kill-ring))))))

(ert-deftest nael-infoview-test-point-highlight ()
  "The innermost subterm at point is highlighted, and only that."
  (nael-test-with-goals
    (nael-test-goto "IProp GF")
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
  (nael-test-with-goals
    (nael-test-goto "goal 3")
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
                     (save-excursion (nael-test-goto "goal 3")
                            (get-text-property (point)
                                               'nael-goal)))))))

;;;; Updates:

(defvar nael-infoview-test-calls nil
  "Requests of the update under test, newest first, as (WHAT SUCCESS OWNER).")

(defmacro nael-infoview-test-with-pending-update (&rest body)
  "Run BODY after an update of a source buffer was requested, unanswered.
The requests are in `nael-infoview-test-calls'; the variable `source'
is the source buffer."
  (declare (indent 0) (debug t))
  `(nael-test-with-options
     (let ((nael-infoview-test-calls nil)
           (nael-infoview--pending-owner nil)
           (source (generate-new-buffer "nael-infoview-test-source")))
       (cl-flet ((record (what)
                   (lambda (&rest args)
                     (let* ((success (seq-find #'functionp args))
                            (keys (cdr (memq success args))))
                       (push (list what success (plist-get keys :owner)
                                   (plist-get keys :failure))
                             nael-infoview-test-calls)))))
         (cl-letf (((symbol-function 'nael-rpc-open) (lambda (&rest _) 'handle))
                   ((symbol-function 'nael-rpc-get-interactive-goals)
                    (record 'goals))
                   ((symbol-function 'nael-rpc-get-interactive-term-goal)
                    (record 'term-goal))
                   ((symbol-function 'nael-rpc-get-interactive-diagnostics)
                    (record 'messages))
                   ((symbol-function 'nael-infoview--accomplished-title)
                    #'ignore))
           (unwind-protect
               (progn
                 (with-current-buffer source (insert "a\nb\nc\n"))
                 (nael-infoview--get-buffer-create)
                 (nael-infoview--request source 1)
                 ,@body)
             (kill-buffer source)
             (when-let* ((buffer (nael-infoview--buffer)))
               (kill-buffer buffer))))))))

(defun nael-infoview-test-answer (&optional failing)
  "Answer the requests of the update under test: one goal, nothing else.
The request for FAILING, one of `goals', `term-goal' and `messages',
fails instead."
  (dolist (call nael-infoview-test-calls)
    (if (eq (car call) failing)
        (funcall (nth 3 call) '(:code -32603 :message "Broken"))
      (funcall (nth 1 call)
               (and (eq (car call) 'goals)
                    (list :goals (vector (nael-test-goal))))))))

(ert-deftest nael-infoview-test-update-answered ()
  "The answers to an update are shown together."
  (nael-infoview-test-with-pending-update
    (should (equal (sort (mapcar #'car nael-infoview-test-calls) #'string<)
                   '(goals messages term-goal)))
    (nael-infoview-test-answer)
    (with-current-buffer (nael-infoview--buffer)
      (should (string-search "⊢ n = 0" (buffer-string)))
      (should-not nael-infoview--status))))

(ert-deftest nael-infoview-test-update-failed ()
  "A failed request for goals is reported; one for messages only drops them."
  (nael-infoview-test-with-pending-update
    (let ((owner (nth 2 (car nael-infoview-test-calls))))
      (nael-infoview-test-answer 'goals)
      (should (nael-rpc-owner-released owner))
      (with-current-buffer (nael-infoview--buffer)
        (should (equal nael-infoview--status '(error . "Broken")))
        (should-not (string-search "⊢ n = 0" (buffer-string))))))
  (nael-infoview-test-with-pending-update
    (nael-infoview-test-answer 'messages)
    (with-current-buffer (nael-infoview--buffer)
      (should-not nael-infoview--status)
      (should (string-search "⊢ n = 0" (buffer-string))))))

(ert-deftest nael-infoview-test-update-after-kill ()
  "Answers that arrive after the infoview was killed are dropped."
  (nael-infoview-test-with-pending-update
    (let ((owner (nth 2 (car nael-infoview-test-calls))))
      (kill-buffer (nael-infoview--buffer))
      (should (nael-rpc-owner-released owner))
      (nael-infoview-test-answer)
      ;; Not shown in a buffer made anew.
      (should-not (nael-infoview--buffer)))))

(ert-deftest nael-infoview-test-update-superseded-by-processing ()
  "Moving to a line being processed drops the answers for the old one."
  (nael-infoview-test-with-pending-update
    (let ((owner (nth 2 (car nael-infoview-test-calls)))
          (nael-infoview--target (cons source 5)))
      (cl-letf (((symbol-function 'nael-infoview-visible-p) #'always)
                ((symbol-function 'eglot-managed-p) #'always)
                ((symbol-function 'nael-infoview--rpc-supported-p) #'always)
                ((symbol-function 'nael-eglot-processing-line-p) #'always))
        (nael-infoview--update))
      (should (nael-rpc-owner-released owner))
      (nael-infoview-test-answer)
      (with-current-buffer (nael-infoview--buffer)
        (should (eq nael-infoview--status 'processing))
        (should-not nael-infoview--results)))))

;;;; Windows:

(ert-deftest nael-infoview-test-jump-out ()
  "A jump from the infoview shows its target in another window."
  (nael-test-with-windows
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
  (nael-test-with-windows
    (let ((home nael-infoview--window)
          (pos (save-excursion (nael-test-goto "goal 2") (point))))
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

;;;; Complete proofs:

(ert-deftest nael-infoview-test-accomplished-title ()
  "Above goals in a complete proof, a line says so; it replaces no goals."
  (nael-test-with-options
    (unwind-protect
        (progn
          (nael-infoview--show (nael-test-spinlock-goals) nil
                               (nael-rpc-make-owner) "SpinLock.lean:110:2"
                               nil "Goals accomplished 🎉")
          (with-current-buffer (nael-infoview--buffer)
            (should (string-prefix-p
                     "Goals accomplished 🎉\nTactic state: 3 goals\ngoal 1\n"
                     (buffer-string)))
            ;; A line of its own, not a heading that folds the goals.
            (goto-char (point-min))
            (should-not (outline-on-heading-p))
            (should (eq (get-text-property (point) 'face)
                        'nael-render-goals-accomplished))
            (forward-line)
            (should (outline-on-heading-p)))
          ;; No goals left: the line instead of "no goals".
          (nael-infoview--show (list :goals []) nil (nael-rpc-make-owner)
                               "A.lean:2:6" nil "Goals accomplished 🎉")
          (with-current-buffer (nael-infoview--buffer)
            (should (equal (string-trim (buffer-string))
                           "Goals accomplished 🎉")))
          (nael-infoview--show (list :goals []) nil (nael-rpc-make-owner)
                               "A.lean:2:6")
          (with-current-buffer (nael-infoview--buffer)
            (should (string-prefix-p "Tactic state: no goals"
                            (buffer-string))))
          ;; Without goals, the title alone.
          (nael-infoview--show nil nil (nael-rpc-make-owner) "A.lean:1:0"
                               nil "Goals accomplished 🎉")
          (with-current-buffer (nael-infoview--buffer)
            (should (equal (string-trim (buffer-string))
                           "Goals accomplished 🎉")))
          (nael-infoview--show nil nil (nael-rpc-make-owner) "A.lean:1:0")
          (with-current-buffer (nael-infoview--buffer)
            (should (string-prefix-p "No goals" (buffer-string)))))
      (when-let* ((buffer (nael-infoview--buffer)))
        (kill-buffer buffer)))))

(defmacro nael-infoview-test-with-proof (&rest body)
  "Run BODY in a buffer of 10 lines whose lines 2 to 5 are a complete proof."
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (dotimes (i 10) (insert (format "line %d\n" i)))
     (setq nael-eglot-diagnostics
           (vector (nael-test-diagnostic 3 '(2 . 0) '(5 . 6)
                                         :tags '(2) :silent t)))
     ,@body))

(defun nael-infoview-test-line (line)
  "Return the position of the start of the 0-based LINE."
  (save-excursion (goto-char (point-min)) (forward-line line) (point)))

(ert-deftest nael-infoview-test-accomplished ()
  "The title comes from the diagnostics and the sign of the source buffer."
  (nael-infoview-test-with-proof
    (let ((nael-margin-goals-accomplished "🎉"))
      (cl-letf (((symbol-function 'char-displayable-p) #'always))
        (cl-flet ((title (line)
                    (nael-infoview--accomplished-title
                     (current-buffer) (nael-infoview-test-line line))))
          (should-not (title 1))
          (should (equal (title 3) "Goals accomplished 🎉"))
          (setq-local nael-margin-goals-accomplished "🐣")
          (should (equal (title 5) "Goals accomplished 🐣"))
          (setq-local nael-margin-goals-accomplished "")
          (should (equal (title 2) "Goals accomplished")))))))

(ert-deftest nael-infoview-test-accomplished-trigger ()
  "The infoview updates when the diagnostics around point change."
  (nael-infoview-test-with-proof
    (let ((scheduled 0)
          (diagnostics nael-eglot-diagnostics)
          (nael-infoview--target (cons (current-buffer)
                                       (nael-infoview-test-line 4))))
      (cl-letf (((symbol-function 'nael-infoview-visible-p) #'always)
                ((symbol-function 'nael-infoview--schedule)
                 (lambda (&rest _) (cl-incf scheduled))))
        (setq nael-infoview--diagnostics-key nil)
        ;; The range of the complete proof is only its first line.
        (nael-infoview--diagnostics diagnostics)
        (should (= scheduled 1))
        (nael-infoview--diagnostics diagnostics)
        (should (= scheduled 1))
        (nael-infoview--diagnostics [])
        (should (= scheduled 2))))))

(provide 'nael-infoview-tests)

;;; nael-infoview-tests.el ends here
