;;; nael-render-tests.el --- Tests for nael-render  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see nael/LICENSE.GPL3.

;;; Commentary:

;; Tests for `nael-render' on a recorded goal and on small handmade
;; ones.

;;; Code:

(require 'ert)
(require 'nael-render)

(defconst nael-render-test-fixtures
  (expand-file-name "fixtures/"
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(defun nael-render-test-fixture ()
  "Return the recorded goals of iris-lean's SpinLock."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "goals-iris-spinlock.json" nael-render-test-fixtures))
    (json-parse-buffer :object-type 'plist :array-type 'array
                       :null-object nil :false-object :json-false)))

(defun nael-render-test-code (&rest parts)
  "Return `CodeWithInfos' made of PARTS.
A string is plain text; a list (TEXT REF [DIFF]) is a tagged subterm."
  (list :append
        (vconcat
         (mapcar (lambda (part)
                   (if (stringp part)
                       (list :text part)
                     (pcase-let ((`(,text ,ref ,diff) part))
                       (list :tag
                             (vector (append (list :info (list :__rpcref ref)
                                                   :subexprPos "/")
                                             (when diff
                                               (list :diffStatus diff)))
                                     (list :text text))))))
                 parts))))

(defun nael-render-test-goal (&rest keys)
  "Return a goal with target `n = 0', merged with KEYS."
  (append keys
          (list :hyps []
                :type (nael-render-test-code '("n" "1") " = 0")
                :goalPrefix "⊢ "
                :mvarId "_uniq.1")))

(defun nael-render-test-text (goals &optional term-goal options)
  "Render GOALS and TERM-GOAL with OPTIONS and return the plain text."
  (substring-no-properties
   (nael-render-result-text (nael-render-goals goals term-goal options))))

(defun nael-render-test-faces (string pos)
  "Return the faces of STRING at 0-based POS as a list."
  (ensure-list (get-text-property pos 'face string)))

;;;; Recorded goals:

(ert-deftest nael-render-test-fixture-text ()
  (let ((text (nael-render-test-text (nael-render-test-fixture))))
    (should (string-prefix-p "Tactic state: 3 goals\ngoal 1\nhlc : HasLC\n"
                             text))
    (should (string-search "inst✝ : " text))
    (should (string-search "\n⊢ |={E}=> isLock" text))
    (should (string-search "\ngoal 3\n" text))))

(ert-deftest nael-render-test-fixture-nodes ()
  "Every tag becomes a node with a reference and a nonempty extent."
  (let* ((result (nael-render-goals (nael-render-test-fixture) nil))
         (nodes (nael-render-result-nodes result)))
    (should (= (length nodes) 63))
    (dolist (node nodes)
      (should (stringp (plist-get (nael-render-node-info node) :__rpcref)))
      (should (< (nael-render-node-beg node) (nael-render-node-end node))))))

(ert-deftest nael-render-test-innermost-node ()
  "At each position, `nael-node' is the smallest node containing it."
  (let* ((result (nael-render-goals (nael-render-test-fixture) nil))
         (text (nael-render-result-text result))
         (nodes (nael-render-result-nodes result)))
    (dotimes (i (length text))
      (let* ((pos (1+ i))
             (containing (seq-filter
                          (lambda (node)
                            (and (<= (nael-render-node-beg node) pos)
                                 (< pos (nael-render-node-end node))))
                          nodes))
             (at (nael-render-node-at i text)))
        (if (null containing)
            (should-not at)
          (should (memq at containing))
          (dolist (node containing)
            (should (<= (- (nael-render-node-end at) (nael-render-node-beg at))
                        (- (nael-render-node-end node)
                           (nael-render-node-beg node)))))
          ;; The node's parent chain is its enclosing nodes.
          (should (seq-set-equal-p (nael-render-node-ancestors at)
                                   containing)))))))

(ert-deftest nael-render-test-fixture-diff ()
  "The subterm marked `willChange' gets the removed face."
  (let* ((result (nael-render-goals (nael-render-test-fixture) nil))
         (node (seq-find (lambda (node)
                           (equal (nael-render-node-diff-status node)
                                  "willChange"))
                         (nael-render-result-nodes result))))
    (should node)
    (should (memq 'nael-render-diff-removed
                  (nael-render-test-faces (nael-render-result-text result)
                                          (1- (nael-render-node-beg node)))))))

(ert-deftest nael-render-test-hide-instances ()
  (let ((text (nael-render-test-text (nael-render-test-fixture) nil
                                     '(:hide-instances t))))
    (should-not (string-search "inst✝" text))
    (should (string-search "hlc : HasLC" text))))

(ert-deftest nael-render-test-hide-inaccessible ()
  (let ((text (nael-render-test-text (nael-render-test-fixture) nil
                                     '(:hide-inaccessible t))))
    (should-not (string-search "✝" text))))

;;;; Handmade goals:

(ert-deftest nael-render-test-hypotheses ()
  (let* ((goal (nael-render-test-goal
                :userName "succ"
                :hyps (vector (list :names ["n" "m"] :fvarIds ["_uniq.2" "_uniq.3"]
                                    :type (nael-render-test-code '("Nat" "4")))
                              (list :names ["k"] :fvarIds ["_uniq.5"]
                                    :type (nael-render-test-code '("Nat" "6"))
                                    :val (nael-render-test-code '("2" "7"))
                                    :isInserted t))))
         (result (nael-render-goals (list :goals (vector goal)) nil))
         (text (nael-render-result-text result)))
    (should (equal (substring-no-properties text)
                   (concat "Tactic state: 1 goal\n"
                           "case succ\n"
                           "n m : Nat\n"
                           "k : Nat := 2\n"
                           "⊢ n = 0\n")))
    (should (memq 'nael-render-hyp-name
                  (nael-render-test-faces text (string-search "n m" text))))
    (should (memq 'nael-render-diff-inserted
                  (nael-render-test-faces text (string-search "k :" text))))
    (should (equal (get-text-property (string-search "n m" text) 'nael-hyp text)
                   '("_uniq.2" "_uniq.3")))
    (should (equal (get-text-property (string-search "⊢" text) 'nael-goal text)
                   '("_uniq.1" . 0)))
    (should (equal (get-text-property 0 'outline-level text) 1))
    (should (equal (get-text-property (string-search "case" text)
                                      'outline-level text)
                   2))
    (should (equal (nael-render-test-text
                    (list :goals (vector goal)) nil
                    '(:hide-let-values t :reverse t))
                   (concat "Tactic state: 1 goal\n"
                           "case succ\n"
                           "⊢ n = 0\n"
                           "k : Nat\n"
                           "n m : Nat\n")))))

(ert-deftest nael-render-test-repeated-goal ()
  "A metavariable listed twice yields two distinct goal ids."
  (let ((text (nael-render-goals
               (list :goals (vector (nael-render-test-goal)
                                    (nael-render-test-goal)))
               nil)))
    (setq text (nael-render-result-text text))
    (should (equal (get-text-property (string-search "goal 1" text)
                                      'nael-goal text)
                   '("_uniq.1" . 0)))
    (should (equal (get-text-property (string-search "goal 2" text)
                                      'nael-goal text)
                   '("_uniq.1" . 1)))))

(ert-deftest nael-render-test-goal-diff ()
  "A removed goal marks its turnstile and heading, not its content."
  (let* ((removed (nael-render-test-goal :isRemoved t :userName "h"))
         (text (nael-render-result-text
                (nael-render-goals (list :goals (vector removed)) nil))))
    (should (memq 'nael-render-diff-removed
                  (nael-render-test-faces text (string-search "⊢" text))))
    (should (memq 'nael-render-diff-removed
                  (nael-render-test-faces text (string-search "case h" text))))
    (should-not (memq 'nael-render-diff-removed
                      (nael-render-test-faces
                       text (string-search "= 0" text))))))

(ert-deftest nael-render-test-goal-keys ()
  "Goals are keyed by case name and occurrence, or by their place."
  (let ((text (nael-render-result-text
               (nael-render-goals
                (list :goals (vector (nael-render-test-goal :userName "h")
                                     (nael-render-test-goal)
                                     (nael-render-test-goal :userName "h")))
                (list :hyps [] :type (nael-render-test-code "Nat"))))))
    (cl-flet ((key (string) (get-text-property (string-search string text)
                                               'nael-key text)))
      (should (equal (key "case h\n⊢") '(case "h" . 0)))
      (should (equal (key "goal 2") '(goal . 1)))
      (should (equal (key "Nat\n") 'term-goal)))
    (should (equal (get-text-property
                    (string-search "case h" text
                                   (1+ (string-search "case h" text)))
                    'nael-key text)
                   '(case "h" . 1)))))

(ert-deftest nael-render-test-sections ()
  (should (equal (nael-render-test-text nil nil) ""))
  (should (equal (nael-render-test-text '(:goals []) nil)
                 "Tactic state: no goals\n"))
  (should (equal (nael-render-test-text
                  nil (list :hyps [] :type (nael-render-test-code "Nat")))
                 "Expected type\n⊢ Nat\n"))
  (should (equal (nael-render-test-text
                  (list :goals (vector (nael-render-test-goal)
                                       (nael-render-test-goal)))
                  (list :hyps [] :type (nael-render-test-code "Nat")))
                 (concat "Tactic state: 2 goals\n"
                         "goal 1\n⊢ n = 0\n\n"
                         "goal 2\n⊢ n = 0\n\n"
                         "Expected type\n⊢ Nat\n"))))

(ert-deftest nael-render-test-diff-statuses ()
  (should (eq (nael-render-diff-face "wasChanged") 'nael-render-diff-inserted))
  (should (eq (nael-render-diff-face "willInsert") 'nael-render-diff-inserted))
  (should (eq (nael-render-diff-face "willDelete") 'nael-render-diff-removed))
  (should-not (nael-render-diff-face nil)))

;;;; Hover information:

(ert-deftest nael-render-test-info ()
  "Hover information renders as the subterm and its type, both with nodes."
  (let* ((result (nael-render-info
                  (list :exprExplicit (nael-render-test-code '("x.isLt" "1"))
                        :type (nael-render-test-code '("↑x" "2") " < 3")
                        :doc "Not rendered.")))
         (text (nael-render-result-text result)))
    (should (equal (substring-no-properties text) "x.isLt : ↑x < 3"))
    (should (equal (mapcar (lambda (node)
                             (substring-no-properties
                              text (1- (nael-render-node-beg node))
                              (1- (nael-render-node-end node))))
                           (nael-render-result-nodes result))
                   '("x.isLt" "↑x")))
    (should (equal (nael-render-node-info (nael-render-node-at 0 text))
                   '(:__rpcref "1")))
    (should-not (nael-render-node-at 7 text))
    ;; Fontified as Lean code.
    (should (memq 'font-lock-type-face
                  (nael-render-test-faces
                   (nael-render-result-text
                    (nael-render-info
                     (list :type (nael-render-test-code "Type → Prop"))))
                   0)))))

(ert-deftest nael-render-test-info-partial ()
  "Missing parts of hover information are left out."
  (should (equal (substring-no-properties
                  (nael-render-result-text
                   (nael-render-info
                    (list :type (nael-render-test-code "Nat")))))
                 "Nat"))
  (should (equal (nael-render-result-text (nael-render-info nil)) "")))

(provide 'nael-render-tests)

;;; nael-render-tests.el ends here
