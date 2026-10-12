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
(require 'nael-test-helpers)

(defun nael-render-test-text (goals &optional term-goal options)
  "Render GOALS and TERM-GOAL with OPTIONS and return the plain text."
  (substring-no-properties
   (nael-render-result-text (nael-render-goals goals term-goal options))))

;;;; Recorded goals:

(ert-deftest nael-render-test-fixture-text ()
  (let ((text (nael-render-test-text (nael-test-spinlock-goals))))
    (should (string-prefix-p "Tactic state: 3 goals\ngoal 1\nhlc : HasLC\n"
                             text))
    (should (string-search "inst✝ : " text))
    (should (string-search "\n⊢ |={E}=> isLock" text))
    (should (string-search "\ngoal 3\n" text))))

(ert-deftest nael-render-test-fixture-nodes ()
  "Every tag becomes a node with a reference and a nonempty extent."
  (let* ((result (nael-render-goals (nael-test-spinlock-goals) nil))
         (nodes (nael-render-result-nodes result)))
    (should (= (length nodes) 63))
    (dolist (node nodes)
      (should (stringp (plist-get (nael-render-node-info node) :__rpcref)))
      (should (< (nael-render-node-beg node) (nael-render-node-end node))))))

(ert-deftest nael-render-test-innermost-node ()
  "At each position, `nael-node' is the smallest node containing it."
  (let* ((result (nael-render-goals (nael-test-spinlock-goals) nil))
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
                           (nael-render-node-beg node))))))))))

(ert-deftest nael-render-test-fixture-diff ()
  "The subterm marked `willChange' gets the removed face."
  (let* ((result (nael-render-goals (nael-test-spinlock-goals) nil))
         (node (seq-find (lambda (node)
                           (equal (nael-render-node-diff-status node)
                                  "willChange"))
                         (nael-render-result-nodes result))))
    (should node)
    (should (memq 'nael-render-diff-removed
                  (nael-test-faces (nael-render-result-text result)
                                   (1- (nael-render-node-beg node)))))))

(ert-deftest nael-render-test-hide-instances ()
  (let ((text (nael-render-test-text (nael-test-spinlock-goals) nil
                                     '(:hide-instances t))))
    (should-not (string-search "inst✝" text))
    (should (string-search "hlc : HasLC" text))))

(ert-deftest nael-render-test-hide-inaccessible ()
  (let ((text (nael-render-test-text (nael-test-spinlock-goals) nil
                                     '(:hide-inaccessible t))))
    (should-not (string-search "✝" text))))

;;;; Handmade goals:

(ert-deftest nael-render-test-hypotheses ()
  (let* ((goal (nael-test-goal
                :userName "succ"
                :hyps (vector (list :names ["n" "m"] :fvarIds ["_uniq.2" "_uniq.3"]
                                    :type (nael-test-code '("Nat" "4")))
                              (list :names ["k"] :fvarIds ["_uniq.5"]
                                    :type (nael-test-code '("Nat" "6"))
                                    :val (nael-test-code '("2" "7"))
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
                  (nael-test-faces text (string-search "n m" text))))
    (should (memq 'nael-render-diff-inserted
                  (nael-test-faces text (string-search "k :" text))))
    (should (eq (get-text-property (string-search "⊢" text) 'nael-goal text)
                (get-text-property (string-search "n m" text) 'nael-goal text)))
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
  "A metavariable listed twice yields two goals that can be told apart."
  (let ((text (nael-render-result-text
               (nael-render-goals
                (list :goals (vector (nael-test-goal)
                                     (nael-test-goal)))
                nil))))
    (should (get-text-property (string-search "goal 1" text) 'nael-goal text))
    (should-not (equal (get-text-property (string-search "goal 1" text)
                                          'nael-goal text)
                       (get-text-property (string-search "goal 2" text)
                                          'nael-goal text)))))

(ert-deftest nael-render-test-goal-diff ()
  "A removed goal marks its turnstile and heading, not its content."
  (let* ((removed (nael-test-goal :isRemoved t :userName "h"))
         (text (nael-render-result-text
                (nael-render-goals (list :goals (vector removed)) nil))))
    (should (memq 'nael-render-diff-removed
                  (nael-test-faces text (string-search "⊢" text))))
    (should (memq 'nael-render-diff-removed
                  (nael-test-faces text (string-search "case h" text))))
    (should-not (memq 'nael-render-diff-removed
                      (nael-test-faces
                       text (string-search "= 0" text))))))

(ert-deftest nael-render-test-goal-keys ()
  "Goals are keyed by case name and occurrence, or by their place."
  (let ((text (nael-render-result-text
               (nael-render-goals
                (list :goals (vector (nael-test-goal :userName "h")
                                     (nael-test-goal)
                                     (nael-test-goal :userName "h")))
                (list :hyps [] :type (nael-test-code "Nat"))))))
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
                  nil (list :hyps [] :type (nael-test-code "Nat")))
                 "Expected type\n⊢ Nat\n"))
  (should (equal (nael-render-test-text
                  (list :goals (vector (nael-test-goal)
                                       (nael-test-goal)))
                  (list :hyps [] :type (nael-test-code "Nat")))
                 (concat "Tactic state: 2 goals\n"
                         "goal 1\n⊢ n = 0\n\n"
                         "goal 2\n⊢ n = 0\n\n"
                         "Expected type\n⊢ Nat\n"))))

(ert-deftest nael-render-test-diff-statuses ()
  (should (eq (nael-render-diff-face "wasChanged") 'nael-render-diff-inserted))
  (should (eq (nael-render-diff-face "willInsert") 'nael-render-diff-inserted))
  (should (eq (nael-render-diff-face "willDelete") 'nael-render-diff-removed))
  (should-not (nael-render-diff-face nil)))

(ert-deftest nael-render-test-wrap-prefix ()
  "Wrapped lines of code continue further in than their line."
  (let* ((nael-render-wrap-indent 4)
         (goal (nael-test-goal
                :type (nael-test-code "n =\n  0")))
         (result (nael-render-goals (list :goals (vector goal)) nil))
         (text (nael-render-result-text result)))
    (cl-flet ((prefix (string)
                (get-text-property (string-search string text) 'wrap-prefix
                                   text)))
      (should (equal (prefix "Tactic") "    "))
      (should (equal (prefix "⊢ n") "    "))
      (should (equal (prefix "0") "      ")))))

;;;; Hover information:

(ert-deftest nael-render-test-info ()
  "Hover information renders as the subterm and its type, both with nodes."
  (let* ((result (nael-render-info
                  (list :exprExplicit (nael-test-code '("x.isLt" "1"))
                        :type (nael-test-code '("↑x" "2") " < 3")
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
                  (nael-test-faces
                   (nael-render-result-text
                    (nael-render-info
                     (list :type (nael-test-code "Type → Prop"))))
                   0)))))

(ert-deftest nael-render-test-info-partial ()
  "Missing parts of hover information are left out."
  (should (equal (substring-no-properties
                  (nael-render-result-text
                   (nael-render-info
                    (list :type (nael-test-code "Nat")))))
                 "Nat"))
  (should (equal (nael-render-result-text (nael-render-info nil)) "")))

(provide 'nael-render-tests)

;;; nael-render-tests.el ends here
