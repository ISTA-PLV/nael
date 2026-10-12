;;; lena-render-tests.el --- Tests for lena-render  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see lena/LICENSE.GPL3.

;;; Commentary:

;; Tests for `lena-render' on a recorded goal and on small handmade
;; ones.

;;; Code:

(require 'ert)
(require 'lena-render)
(require 'lena-test-helpers)

(defun lena-render-test-text (goals &optional term-goal options)
  "Render GOALS and TERM-GOAL with OPTIONS and return the plain text."
  (substring-no-properties
   (lena-render-result-text (lena-render-goals goals term-goal options))))

;;;; Recorded goals:

(ert-deftest lena-render-test-fixture-text ()
  (let ((text (lena-render-test-text (lena-test-spinlock-goals))))
    (should (string-prefix-p "Tactic state: 3 goals\ngoal 1\nhlc : HasLC\n"
                             text))
    (should (string-search "inst✝ : " text))
    (should (string-search "\n⊢ |={E}=> isLock" text))
    (should (string-search "\ngoal 3\n" text))))

(ert-deftest lena-render-test-fixture-nodes ()
  "Every tag becomes a node with a reference and a nonempty extent."
  (let* ((result (lena-render-goals (lena-test-spinlock-goals) nil))
         (nodes (lena-render-result-nodes result)))
    (should (= (length nodes) 63))
    (dolist (node nodes)
      (should (stringp (plist-get (lena-render-node-info node) :__rpcref)))
      (should (< (lena-render-node-beg node) (lena-render-node-end node))))))

(ert-deftest lena-render-test-innermost-node ()
  "At each position, `lena-node' is the smallest node containing it."
  (let* ((result (lena-render-goals (lena-test-spinlock-goals) nil))
         (text (lena-render-result-text result))
         (nodes (lena-render-result-nodes result)))
    (dotimes (i (length text))
      (let* ((pos (1+ i))
             (containing (seq-filter
                          (lambda (node)
                            (and (<= (lena-render-node-beg node) pos)
                                 (< pos (lena-render-node-end node))))
                          nodes))
             (at (lena-render-node-at i text)))
        (if (null containing)
            (should-not at)
          (should (memq at containing))
          (dolist (node containing)
            (should (<= (- (lena-render-node-end at) (lena-render-node-beg at))
                        (- (lena-render-node-end node)
                           (lena-render-node-beg node))))))))))

(ert-deftest lena-render-test-fixture-diff ()
  "The subterm marked `willChange' gets the removed face."
  (let* ((result (lena-render-goals (lena-test-spinlock-goals) nil))
         (node (seq-find (lambda (node)
                           (equal (lena-render-node-diff-status node)
                                  "willChange"))
                         (lena-render-result-nodes result))))
    (should node)
    (should (memq 'lena-render-diff-removed
                  (lena-test-faces (lena-render-result-text result)
                                   (1- (lena-render-node-beg node)))))))

(ert-deftest lena-render-test-hide-instances ()
  (let ((text (lena-render-test-text (lena-test-spinlock-goals) nil
                                     '(:hide-instances t))))
    (should-not (string-search "inst✝" text))
    (should (string-search "hlc : HasLC" text))))

(ert-deftest lena-render-test-hide-inaccessible ()
  (let ((text (lena-render-test-text (lena-test-spinlock-goals) nil
                                     '(:hide-inaccessible t))))
    (should-not (string-search "✝" text))))

;;;; Handmade goals:

(ert-deftest lena-render-test-hypotheses ()
  (let* ((goal (lena-test-goal
                :userName "succ"
                :hyps (vector (list :names ["n" "m"] :fvarIds ["_uniq.2" "_uniq.3"]
                                    :type (lena-test-code '("Nat" "4")))
                              (list :names ["k"] :fvarIds ["_uniq.5"]
                                    :type (lena-test-code '("Nat" "6"))
                                    :val (lena-test-code '("2" "7"))
                                    :isInserted t))))
         (result (lena-render-goals (list :goals (vector goal)) nil))
         (text (lena-render-result-text result)))
    (should (equal (substring-no-properties text)
                   (concat "Tactic state: 1 goal\n"
                           "case succ\n"
                           "n m : Nat\n"
                           "k : Nat := 2\n"
                           "⊢ n = 0\n")))
    (should (memq 'lena-render-hyp-name
                  (lena-test-faces text (string-search "n m" text))))
    (should (memq 'lena-render-diff-inserted
                  (lena-test-faces text (string-search "k :" text))))
    (should (eq (get-text-property (string-search "⊢" text) 'lena-goal text)
                (get-text-property (string-search "n m" text) 'lena-goal text)))
    (should (equal (get-text-property 0 'outline-level text) 1))
    (should (equal (get-text-property (string-search "case" text)
                                      'outline-level text)
                   2))
    (should (equal (lena-render-test-text
                    (list :goals (vector goal)) nil
                    '(:hide-let-values t :reverse t))
                   (concat "Tactic state: 1 goal\n"
                           "case succ\n"
                           "⊢ n = 0\n"
                           "k : Nat\n"
                           "n m : Nat\n")))))

(ert-deftest lena-render-test-repeated-goal ()
  "A metavariable listed twice yields two goals that can be told apart."
  (let ((text (lena-render-result-text
               (lena-render-goals
                (list :goals (vector (lena-test-goal)
                                     (lena-test-goal)))
                nil))))
    (should (get-text-property (string-search "goal 1" text) 'lena-goal text))
    (should-not (equal (get-text-property (string-search "goal 1" text)
                                          'lena-goal text)
                       (get-text-property (string-search "goal 2" text)
                                          'lena-goal text)))))

(ert-deftest lena-render-test-goal-diff ()
  "A removed goal marks its turnstile and heading, not its content."
  (let* ((removed (lena-test-goal :isRemoved t :userName "h"))
         (text (lena-render-result-text
                (lena-render-goals (list :goals (vector removed)) nil))))
    (should (memq 'lena-render-diff-removed
                  (lena-test-faces text (string-search "⊢" text))))
    (should (memq 'lena-render-diff-removed
                  (lena-test-faces text (string-search "case h" text))))
    (should-not (memq 'lena-render-diff-removed
                      (lena-test-faces
                       text (string-search "= 0" text))))))

(ert-deftest lena-render-test-goal-keys ()
  "Goals are keyed by case name and occurrence, or by their place."
  (let ((text (lena-render-result-text
               (lena-render-goals
                (list :goals (vector (lena-test-goal :userName "h")
                                     (lena-test-goal)
                                     (lena-test-goal :userName "h")))
                (list :hyps [] :type (lena-test-code "Nat"))))))
    (cl-flet ((key (string) (get-text-property (string-search string text)
                                               'lena-key text)))
      (should (equal (key "case h\n⊢") '(case "h" . 0)))
      (should (equal (key "goal 2") '(goal . 1)))
      (should (equal (key "Nat\n") 'term-goal)))
    (should (equal (get-text-property
                    (string-search "case h" text
                                   (1+ (string-search "case h" text)))
                    'lena-key text)
                   '(case "h" . 1)))))

(ert-deftest lena-render-test-sections ()
  (should (equal (lena-render-test-text nil nil) ""))
  (should (equal (lena-render-test-text '(:goals []) nil)
                 "Tactic state: no goals\n"))
  (should (equal (lena-render-test-text
                  nil (list :hyps [] :type (lena-test-code "Nat")))
                 "Expected type\n⊢ Nat\n"))
  (should (equal (lena-render-test-text
                  (list :goals (vector (lena-test-goal)
                                       (lena-test-goal)))
                  (list :hyps [] :type (lena-test-code "Nat")))
                 (concat "Tactic state: 2 goals\n"
                         "goal 1\n⊢ n = 0\n\n"
                         "goal 2\n⊢ n = 0\n\n"
                         "Expected type\n⊢ Nat\n"))))

(ert-deftest lena-render-test-diff-statuses ()
  (should (eq (lena-render-diff-face "wasChanged") 'lena-render-diff-inserted))
  (should (eq (lena-render-diff-face "willInsert") 'lena-render-diff-inserted))
  (should (eq (lena-render-diff-face "willDelete") 'lena-render-diff-removed))
  (should-not (lena-render-diff-face nil)))

(ert-deftest lena-render-test-wrap-prefix ()
  "Wrapped lines of code continue further in than their line."
  (let* ((lena-render-wrap-indent 4)
         (goal (lena-test-goal
                :type (lena-test-code "n =\n  0")))
         (result (lena-render-goals (list :goals (vector goal)) nil))
         (text (lena-render-result-text result)))
    (cl-flet ((prefix (string)
                (get-text-property (string-search string text) 'wrap-prefix
                                   text)))
      (should (equal (prefix "Tactic") "    "))
      (should (equal (prefix "⊢ n") "    "))
      (should (equal (prefix "0") "      ")))))

;;;; Hover information:

(ert-deftest lena-render-test-info ()
  "Hover information renders as the subterm and its type, both with nodes."
  (let* ((result (lena-render-info
                  (list :exprExplicit (lena-test-code '("x.isLt" "1"))
                        :type (lena-test-code '("↑x" "2") " < 3")
                        :doc "Not rendered.")))
         (text (lena-render-result-text result)))
    (should (equal (substring-no-properties text) "x.isLt : ↑x < 3"))
    (should (equal (mapcar (lambda (node)
                             (substring-no-properties
                              text (1- (lena-render-node-beg node))
                              (1- (lena-render-node-end node))))
                           (lena-render-result-nodes result))
                   '("x.isLt" "↑x")))
    (should (equal (lena-render-node-info (lena-render-node-at 0 text))
                   '(:__rpcref "1")))
    (should-not (lena-render-node-at 7 text))
    ;; Fontified as Lean code.
    (should (memq 'font-lock-type-face
                  (lena-test-faces
                   (lena-render-result-text
                    (lena-render-info
                     (list :type (lena-test-code "Type → Prop"))))
                   0)))))

(ert-deftest lena-render-test-info-partial ()
  "Missing parts of hover information are left out."
  (should (equal (substring-no-properties
                  (lena-render-result-text
                   (lena-render-info
                    (list :type (lena-test-code "Nat")))))
                 "Nat"))
  (should (equal (lena-render-result-text (lena-render-info nil)) "")))

(provide 'lena-render-tests)

;;; lena-render-tests.el ends here
