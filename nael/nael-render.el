;;; nael-render.el --- Render Lean's interactive goals  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see LICENSE.GPL3.

;;; Commentary:

;; This file turns the results of the Lean RPC methods
;; `Lean.Widget.getInteractiveGoals' and
;; `Lean.Widget.getInteractiveTermGoal' into propertized text.  It has
;; no side effects: it makes no requests and touches no buffer but a
;; temporary one, so that it can be tested on recorded results.
;;
;; Code in a goal is `TaggedText': strings in which some substrings are
;; tagged with information about the subterm they display.  The server
;; has already broken the lines.  Every tagged subterm becomes a node
;; (`nael-render-node'), stored in the text property `nael-node' of its
;; characters.  Text properties do not nest, so a character carries the
;; innermost node, and each node knows its parent.  The result also
;; lists all nodes, so that a buffer can put overlays on their exact
;; extents.
;;
;; Goals and hypotheses carry the text properties `nael-goal' and
;; `nael-hyp', made from the server's stable identifiers, so that state
;; such as folding can survive a refresh.  The same metavariable can be
;; listed as several goals, so `nael-goal' is (MVARID . N) for the Nth
;; occurrence of MVARID.  Headings carry `outline-level';
;; section headings also carry `nael-section', `goals' or `term-goal'.

;;; Code:

(require 'cl-lib)
(require 'seq)

(require 'nael)

(defgroup nael-render nil
  "Rendering of Lean's interactive goals."
  :group 'nael
  :prefix "nael-render-")

(defface nael-render-heading
  '((t :inherit bold))
  "Face of section headings such as \"Tactic state\"."
  :group 'nael-render)

(defface nael-render-case
  '((t :inherit font-lock-keyword-face))
  "Face of the case name of a goal."
  :group 'nael-render)

(defface nael-render-hyp-name
  '((t :inherit font-lock-variable-name-face))
  "Face of the names of hypotheses."
  :group 'nael-render)

(defface nael-render-inaccessible
  '((t :inherit shadow))
  "Face of inaccessible names, which contain \"✝\"."
  :group 'nael-render)

(defface nael-render-goal-prefix
  '((t :inherit font-lock-builtin-face))
  "Face of the turnstile before the target of a goal."
  :group 'nael-render)

(defface nael-render-diff-inserted
  '((t :inherit diff-refine-added))
  "Face of what the tactic at point has added or changed."
  :group 'nael-render)

(defface nael-render-diff-removed
  '((t :inherit diff-refine-removed))
  "Face of what the tactic at point will remove or change."
  :group 'nael-render)

;;;; Data:

(defun nael-render--true-p (value)
  "Return non-nil if VALUE is JSON true."
  (and value (not (eq value :json-false))))

(defun nael-render-diff-face (status)
  "Return the face for STATUS, the `diffStatus' of a subterm, or nil.
Before a tactic, the server marks what it is about to change; after a
tactic, what it has changed."
  (pcase status
    ((or "wasChanged" "wasInserted" "willInsert") 'nael-render-diff-inserted)
    ((or "willChange" "willDelete" "wasDeleted") 'nael-render-diff-removed)))

(cl-defstruct (nael-render-node (:constructor nael-render--node-create)
                                (:copier nil))
  "A tagged subterm of rendered code."
  info                                  ; RPC reference, for hovers
  subexpr-pos                           ; position in the expression
  diff-status                           ; string or nil
  beg end                               ; extent in the rendered text
  parent)                               ; enclosing node, or nil

(cl-defstruct (nael-render-result (:constructor nael-render--result-create)
                                  (:copier nil))
  "Rendered text and the nodes in it.
Positions of nodes count from 1 at the start of TEXT, as in a buffer
that contains only TEXT."
  text
  nodes)

;;;; Code:

(defvar nael-render--nodes nil
  "Nodes rendered so far, in reverse order.")

(defvar nael-render--goal-ids nil
  "Metavariable ids of the goals rendered so far, for `nael-goal'.")

(defvar nael-render--faces nil
  "Faces to add after fontification, as a list of (BEG END FACE).
Fontification would remove faces put on the text earlier.")

(defun nael-render--insert (string face)
  "Insert STRING at point and note FACE for it."
  (let ((beg (point)))
    (insert string)
    (push (list beg (point) face) nael-render--faces)))

(defun nael-render--tagged (tagged parent)
  "Insert TAGGED, a `TaggedText', at point; PARENT is the enclosing node."
  (cond
   ((plist-member tagged :text)
    (insert (plist-get tagged :text)))
   ((plist-member tagged :append)
    (seq-doseq (child (plist-get tagged :append))
      (nael-render--tagged child parent)))
   ((plist-member tagged :tag)
    (let* ((info (aref (plist-get tagged :tag) 0))
           (node (nael-render--node-create
                  :info (plist-get info :info)
                  :subexpr-pos (plist-get info :subexprPos)
                  :diff-status (plist-get info :diffStatus)
                  :beg (point)
                  :parent parent)))
      (nael-render--tagged (aref (plist-get tagged :tag) 1) node)
      (setf (nael-render-node-end node) (point))
      (push node nael-render--nodes)))))

(defun nael-render--code (tagged)
  "Insert TAGGED, a `CodeWithInfos', at point."
  (nael-render--tagged tagged nil))

(defun nael-render--fontify (beg end)
  "Fontify the code between BEG and END with the rules of `nael-mode'."
  (set-syntax-table nael-mode-syntax-table)
  (setq-local font-lock-defaults nael-font-lock-defaults)
  (font-lock-ensure beg end))

(defun nael-render--inaccessible-p (name)
  "Return non-nil if NAME is inaccessible."
  (string-search "✝" name))

;;;; Goals:

(defun nael-render--hyp (hyp options)
  "Insert HYP, an `InteractiveHypothesisBundle', as a line.
OPTIONS is a plist of view options; see `nael-render-goals'."
  (let ((names (seq-remove
                (lambda (name)
                  (and (plist-get options :hide-inaccessible)
                       (nael-render--inaccessible-p name)))
                (plist-get hyp :names))))
    (unless (or (null names)
                (and (plist-get options :hide-instances)
                     (nael-render--true-p (plist-get hyp :isInstance)))
                (and (plist-get options :hide-types)
                     (nael-render--true-p (plist-get hyp :isType))))
      (let ((beg (point))
            (diff (cond ((nael-render--true-p (plist-get hyp :isInserted))
                         'nael-render-diff-inserted)
                        ((nael-render--true-p (plist-get hyp :isRemoved))
                         'nael-render-diff-removed))))
        (cl-loop for (name . more) on names
                 do (nael-render--insert
                     name (if (nael-render--inaccessible-p name)
                              'nael-render-inaccessible
                            'nael-render-hyp-name))
                 when more do (insert " "))
        (when diff
          (push (list beg (point) diff) nael-render--faces))
        (insert " : ")
        (nael-render--code (plist-get hyp :type))
        (when-let* ((val (plist-get hyp :val))
                    ((not (plist-get options :hide-let-values))))
          (insert " := ")
          (nael-render--code val))
        (insert "\n")
        (put-text-property beg (point) 'nael-hyp
                           (append (plist-get hyp :fvarIds) nil))))))

(defun nael-render--target (goal)
  "Insert the target of GOAL, with its turnstile, as the last line."
  (let ((prefix (or (plist-get goal :goalPrefix) "⊢ ")))
    (nael-render--insert prefix 'nael-render-goal-prefix)
    (nael-render--code (plist-get goal :type))
    (insert "\n")))

(defun nael-render--goal (goal heading options)
  "Insert GOAL, an `InteractiveGoal' or `InteractiveTermGoal'.
If HEADING is non-nil, insert it as a level 2 heading first.  OPTIONS
is a plist of view options; see `nael-render-goals'."
  (let ((beg (point))
        (hyps (plist-get goal :hyps)))
    (when heading
      (nael-render--insert (propertize heading 'outline-level 2)
                           'nael-render-case)
      (insert "\n"))
    (let ((code-beg (point)))
      (if (plist-get options :reverse)
          (progn (nael-render--target goal)
                 (seq-doseq (hyp (reverse hyps))
                   (nael-render--hyp hyp options)))
        (seq-doseq (hyp hyps)
          (nael-render--hyp hyp options))
        (nael-render--target goal))
      (nael-render--fontify code-beg (point)))
    (when-let* ((diff (cond ((nael-render--true-p (plist-get goal :isInserted))
                             'nael-render-diff-inserted)
                            ((nael-render--true-p (plist-get goal :isRemoved))
                             'nael-render-diff-removed))))
      (push (list beg (point) diff) nael-render--faces))
    (let ((id (plist-get goal :mvarId)))
      (put-text-property beg (point) 'nael-goal
                         (cons id (seq-count (lambda (other) (equal other id))
                                             nael-render--goal-ids)))
      (push id nael-render--goal-ids))))

(defun nael-render--heading (text section)
  "Insert TEXT as the level 1 heading of SECTION, a symbol."
  (nael-render--insert (propertize text 'outline-level 1 'nael-section section)
                       'nael-render-heading)
  (insert "\n"))

(defun nael-render--goals-section (goals options)
  "Insert the section for GOALS, a vector of `InteractiveGoal's.
OPTIONS is a plist of view options; see `nael-render-goals'."
  (let ((count (length goals)))
    (nael-render--heading
     (format "Tactic state: %s"
             (pcase count
               (0 "no goals")
               (1 "1 goal")
               (_ (format "%d goals" count))))
     'goals)
    (seq-do-indexed
     (lambda (goal index)
       (unless (zerop index) (insert "\n"))
       (nael-render--goal
        goal
        (cond ((plist-get goal :userName)
               (format "case %s" (plist-get goal :userName)))
              ((> count 1)
               (format "goal %d" (1+ index))))
        options))
     goals)))

(defun nael-render--apply-faces ()
  "Add the noted faces, then the diff faces of the rendered nodes."
  (pcase-dolist (`(,beg ,end ,face) (reverse nael-render--faces))
    (add-face-text-property beg end face))
  (dolist (node nael-render--nodes)
    (when-let* ((face (nael-render-diff-face
                       (nael-render-node-diff-status node))))
      (add-face-text-property (nael-render-node-beg node)
                              (nael-render-node-end node)
                              face))))

(defun nael-render--put-nodes ()
  "Store each rendered node in `nael-node' over its extent.
Outer nodes come first, so that inner ones win."
  (dolist (node (sort (copy-sequence nael-render--nodes)
                      (lambda (a b)
                        (> (- (nael-render-node-end a)
                              (nael-render-node-beg a))
                           (- (nael-render-node-end b)
                              (nael-render-node-beg b))))))
    (put-text-property (nael-render-node-beg node)
                       (nael-render-node-end node)
                       'nael-node node)))

(defun nael-render-goals (goals term-goal &optional options)
  "Render GOALS and TERM-GOAL as a `nael-render-result'.

GOALS is the result of `Lean.Widget.getInteractiveGoals' and TERM-GOAL
that of `Lean.Widget.getInteractiveTermGoal'; either may be nil.  A
section is rendered for each of them that is non-nil.

OPTIONS is a plist of view options:
  :hide-instances    hide instance hypotheses, such as `inst✝ : Ring R';
  :hide-types        hide hypotheses that are types, such as `α : Type';
  :hide-inaccessible hide inaccessible names, such as `n✝';
  :hide-let-values   hide the values of let-bound hypotheses;
  :reverse           show the target first, then hypotheses upwards."
  (let ((nael-render--nodes nil)
        (nael-render--goal-ids nil)
        (nael-render--faces nil))
    (with-temp-buffer
      (when goals
        (nael-render--goals-section (plist-get goals :goals) options))
      (when term-goal
        (unless (bobp) (insert "\n"))
        (nael-render--heading "Expected type" 'term-goal)
        (nael-render--goal term-goal nil options))
      (nael-render--apply-faces)
      (nael-render--put-nodes)
      (nael-render--result-create
       :text (buffer-string)
       :nodes (nreverse nael-render--nodes)))))

(defun nael-render-node-at (pos &optional object)
  "Return the innermost node at POS in OBJECT, a buffer or string."
  (get-text-property pos 'nael-node object))

(defun nael-render-node-ancestors (node)
  "Return NODE and its enclosing nodes, innermost first."
  (let (nodes)
    (while node
      (push node nodes)
      (setq node (nael-render-node-parent node)))
    (nreverse nodes)))

(provide 'nael-render)

;;; nael-render.el ends here
