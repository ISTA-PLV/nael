;;; lena-messages-tests.el --- Tests for messages in the infoview  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see lena/LICENSE.GPL3.

;;; Commentary:

;; Tests for the messages in the infoview that need no Lean server:
;; their rendering in `lena-render', on recorded and handmade messages;
;; and the infoview's folds, links, suggestions and traces, with the
;; calls to the server replaced.

;;; Code:

(require 'ert)
(require 'lena-infoview)
(require 'lena-test-helpers)

(defun lena-messages-test-fixture (&rest indices)
  "Return the recorded messages at INDICES, or all, as a vector."
  (let ((all (lena-test-read-fixture "interactive-diagnostics.json")))
    (if indices
        (vconcat (mapcar (lambda (index) (aref all index)) indices))
      all)))

(defun lena-messages-test-render (diagnostics line &optional children)
  "Render DIAGNOSTICS requested at LINE, with fetched trace CHILDREN."
  (lena-render-goals nil nil nil (list :diagnostics diagnostics :line line
                                       :trace-children children)))

(defun lena-messages-test-text (&rest args)
  "Return the plain text of `lena-messages-test-render' with ARGS."
  (substring-no-properties
   (lena-render-result-text (apply #'lena-messages-test-render args))))

(defun lena-messages-test-headings (string)
  "Return the headings of STRING as (LINE LEVEL KEY FOLDED LAZY)."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let (headings)
      (while (not (eobp))
        (when-let* ((level (get-text-property (point) 'outline-level)))
          (push (list (line-number-at-pos) level
                      (get-text-property (point) 'lena-key)
                      (get-text-property (point) 'lena-folded)
                      (get-text-property (point) 'lena-trace-lazy))
                headings))
        (forward-line))
      (nreverse headings))))

(defun lena-messages-test-message (message &rest keys)
  "Return a diagnostic with MESSAGE, a `TaggedText', merged with KEYS.
It is an error on line 3 unless KEYS say otherwise."
  (append keys
          (list :range (lena-test-range '(3 . 2) '(3 . 8))
                :severity 1
                :message message)))

(defun lena-messages-test-trace (cls text children &rest keys)
  "Return a trace embed of CLS with TEXT and CHILDREN, merged with KEYS."
  (list :tag (vector (list :trace (append keys
                                          (list :indent 0 :cls cls
                                                :collapsed :json-false
                                                :msg (list :text text)
                                                :children children)))
                     (list :text ""))))

;;;; Rendering:

(ert-deftest lena-messages-test-fixture-text ()
  "Recorded messages: an error, a trace, a suggestion and a #check."
  (should (equal (lena-messages-test-text
                  (lena-messages-test-fixture 2 3 4 5) 215)
                 "Messages (4)
213:0 error
Invalid `end`: There is no current scope to end

Note: Scopes are introduced using `namespace` and `section`
216:27 information
[Meta.synthInstance] ✅️ Inhabited Nat
218:2 information
Try this:
  [apply] exact Nat.add_comm a b
219:0 information
Nat.add_comm (n m : Nat) : n + m = m + n
")))

(ert-deftest lena-messages-test-headings ()
  "Messages and traces are headings; those not on the line start folded."
  (let ((result (lena-messages-test-render
                 (lena-messages-test-fixture 2 3 4) 215)))
    (should (equal (lena-messages-test-headings
                    (lena-render-result-text result))
                   '((1 1 nil nil nil)
                     (2 2 (message 212 0 "error" 0) t nil)
                     (6 2 (message 215 27 "information" 0) nil nil)
                     (7 3 (message 215 27 "information" 0 0) nil
                        (:__rpcref "2"))
                     (8 2 (message 217 2 "information" 0) t nil))))
    ;; The section is a section of its own.
    (should (eq (get-text-property 0 'lena-section
                                   (lena-render-result-text result))
                'messages))))

(ert-deftest lena-messages-test-goals-accomplished ()
  "Messages that a proof is complete are left out; the goals say it."
  (should (string-prefix-p "Messages (4)\n"
                           (lena-messages-test-text
                            (lena-messages-test-fixture) 215)))
  (should (equal (lena-messages-test-text (lena-messages-test-fixture 0 1) 68)
                 ""))
  (should (equal (lena-messages-test-text [] 68) "")))

(ert-deftest lena-messages-test-nodes ()
  "Code in messages is made of nodes, as in goals."
  (let* ((result (lena-messages-test-render (lena-messages-test-fixture 5) 218))
         (text (lena-render-result-text result))
         (nodes (lena-render-result-nodes result)))
    (should nodes)
    (dolist (node nodes)
      (should (lena-render-node-info node))
      (should (< (lena-render-node-beg node) (lena-render-node-end node))))
    (should (seq-some (lambda (node)
                        (equal (substring-no-properties
                                text (1- (lena-render-node-beg node))
                                (1- (lena-render-node-end node)))
                               "n + m = m + n"))
                      nodes))))

(ert-deftest lena-messages-test-severities ()
  "The severity follows the position, in its face."
  (let ((text (lena-render-result-text
               (lena-messages-test-render
                (vector (lena-messages-test-message '(:text "e"))
                        (lena-messages-test-message '(:text "w") :severity 2)
                        (lena-messages-test-message '(:text "i") :severity 3)
                        (lena-messages-test-message '(:text "h") :severity 4))
                3))))
    (should (equal (substring-no-properties text)
                   "Messages (4)\n4:2 error\ne\n4:2 warning\nw\n4:2 information\ni\n4:2 hint\nh\n"))
    (dolist (case '(("error" . compilation-error)
                    ("warning" . compilation-warning)
                    ("information" . compilation-info)
                    ("hint" . compilation-info)))
      (should (memq (cdr case)
                    (lena-test-faces
                     text (string-search (car case) text)))))
    ;; The position knows the range of the message.
    (should (equal (get-text-property (string-search "4:2" text)
                                      'lena-message-range text)
                   (lena-test-range '(3 . 2) '(3 . 8))))))

(ert-deftest lena-messages-test-trace-children ()
  "Fetched children of a trace are shown under it, one level deeper."
  (let* ((children (make-hash-table :test #'equal))
         (key '(message 3 2 "error" 0 0)))
    (puthash key
             (vector (lena-messages-test-trace
                      "Meta.synthInstance" "child" '(:lazy (:__rpcref "8"))))
             children)
    (let ((text (lena-render-result-text
                 (lena-messages-test-render
                  (vector (lena-messages-test-message
                           (lena-messages-test-trace
                            "Meta.synthInstance" "top"
                            '(:lazy (:__rpcref "7")))))
                  3 children))))
      (should (equal (substring-no-properties text)
                     "Messages (1)\n4:2 error\n[Meta.synthInstance] top\n  [Meta.synthInstance] child\n"))
      (should (equal (cdr (lena-messages-test-headings text))
                     `((2 2 (message 3 2 "error" 0) nil nil)
                       (3 3 ,key nil nil)
                       (4 4 (message 3 2 "error" 0 0 0) nil
                          (:__rpcref "8"))))))))

(ert-deftest lena-messages-test-same-start ()
  "Messages with the same start and severity keep their traces apart."
  (let ((children (make-hash-table :test #'equal))
        (message (lambda (ref)
                   (lena-messages-test-message
                    (lena-messages-test-trace "a" ref (list :lazy (list :__rpcref ref)))))))
    (puthash '(message 3 2 "error" 0 0)
             (vector (lena-messages-test-trace "a" "child" '(:strict [])))
             children)
    (let ((text (lena-render-result-text
                 (lena-messages-test-render
                  (vector (funcall message "1") (funcall message "2"))
                  3 children))))
      (should (equal (substring-no-properties text)
                     "Messages (2)\n4:2 error\n[a] 1\n  [a] child\n4:2 error\n[a] 2\n"))
      (should (equal (mapcar (lambda (heading) (list (nth 2 heading) (nth 4 heading)))
                             (cdr (lena-messages-test-headings text)))
                     '(((message 3 2 "error" 0) nil)
                       ((message 3 2 "error" 0 0) nil)
                       ((message 3 2 "error" 0 0 0) nil)
                       ((message 3 2 "error" 1) nil)
                       ((message 3 2 "error" 1 0) (:__rpcref "2"))))))))

(ert-deftest lena-messages-test-strict-trace ()
  "Children sent with a trace are shown; a collapsed trace starts folded."
  (let ((text (lena-render-result-text
               (lena-messages-test-render
                (vector (lena-messages-test-message
                         (list :append
                               (vector
                                (lena-messages-test-trace
                                 "a" "one"
                                 (list :strict
                                       (vector (lena-messages-test-trace
                                                "a.b" "inner" '(:strict []))
                                               (lena-messages-test-trace
                                                "a.b" "inner2" '(:strict []))))
                                 :collapsed t)
                                (lena-messages-test-trace
                                 "a" "two" '(:strict []))))))
                3))))
    (should (equal (substring-no-properties text)
                   "Messages (1)\n4:2 error\n[a] one\n  [a.b] inner\n  [a.b] inner2\n[a] two\n"))
    (should (equal (cdr (lena-messages-test-headings text))
                   '((2 2 (message 3 2 "error" 0) nil nil)
                     (3 3 (message 3 2 "error" 0 0) t nil)
                     (4 4 (message 3 2 "error" 0 0 0) nil nil)
                     (5 4 (message 3 2 "error" 0 0 1) nil nil)
                     (6 3 (message 3 2 "error" 0 1) nil nil))))
    (should (memq 'lena-render-trace-class
                  (lena-test-faces text (string-search "[a]" text))))))

(ert-deftest lena-messages-test-suggestion ()
  "Lean's suggestions become text that knows what to replace."
  (let* ((text (lena-render-result-text
                (lena-messages-test-render (lena-messages-test-fixture 4) 217)))
         (pos (string-search "[apply]" text)))
    (should (equal (get-text-property pos 'lena-suggestion text)
                   '(:range (:end (:character 8 :line 217)
                             :start (:character 2 :line 217))
                     :suggestion "exact Nat.add_comm a b"
                     :help "Apply suggestion")))
    (should (memq 'lena-render-suggestion (lena-test-faces text pos)))))

(ert-deftest lena-messages-test-other-widgets ()
  "Other widgets show their alternative text."
  (should (equal (lena-messages-test-text
                  (vector (lena-messages-test-message
                           (list :tag (vector (list :widget
                                                    (list :wi (list :id "ProofWidgets.Foo"
                                                                    :props nil)
                                                          :alt '(:text "alternative")))
                                              '(:text "")))))
                  3)
                 "Messages (1)\n4:2 error\nalternative\n")))

(ert-deftest lena-messages-test-goal ()
  "Goals in messages are shown as goals, on lines of their own."
  (let* ((result (lena-messages-test-render
                  (vector (lena-messages-test-message
                           (list :append
                                 (vector '(:text "unsolved goals")
                                         (list :tag (vector (list :goal (lena-test-goal
                                                                         :userName "right"))
                                                            '(:text "")))))))
                  3))
         (text (lena-render-result-text result)))
    (should (equal (substring-no-properties text)
                   "Messages (1)\n4:2 error\nunsolved goals\ncase right\n⊢ n = 0\n"))
    ;; Not a heading: the message is one.
    (should-not (get-text-property (string-search "case" text)
                                   'outline-level text))
    (should (= (length (lena-render-result-nodes result)) 1))))

(defconst lena-messages-test-imports
  (lena-test-diagnostic
   3 '(0 . 0) '(19 . 0)
   :message '(:text "Imports are out of date and should be rebuilt; use the \"Restart File\" command in your editor."))
  "A message about the whole file of 20 lines that its imports are out of date.")

(ert-deftest lena-messages-test-whole-file ()
  "Messages about the whole file are unfolded, first, and may restart it."
  (let* ((other (lena-test-diagnostic 1 '(5 . 0) '(5 . 4)
                                      :message '(:text "Boom")))
         (crash (lena-test-diagnostic
                 1 '(0 . 0) '(19 . 0) :message '(:text "Server crashed")))
         (text (lena-render-result-text
                (lena-render-goals nil nil nil
                                   (list :diagnostics
                                         (vector other lena-messages-test-imports
                                                 crash)
                                         :line 5 :line-count 20)))))
    (should (equal (substring-no-properties text)
                   "Messages (3)
1:0 information
Imports are out of date and should be rebuilt; use the \"Restart File\" command in your editor.
[restart file]
1:0 error
Server crashed
6:0 error
Boom
"))
    ;; Not folded although they start on another line.
    (should (equal (mapcar (lambda (heading) (nth 3 heading))
                           (cdr (lena-messages-test-headings text)))
                   '(nil nil nil)))
    (should (get-text-property (string-search "[restart file]" text)
                               'lena-restart-file text)))
  ;; Without the number of lines, such a message is like any other.
  (let ((text (lena-render-result-text
               (lena-messages-test-render (vector lena-messages-test-imports)
                                          5))))
    (should-not (string-search "[restart file]" text))
    (should (nth 3 (cadr (lena-messages-test-headings text))))))

(defun lena-messages-test-widget (id props &optional alt)
  "Return a message embedding the widget ID with PROPS and ALT."
  (lena-messages-test-message
   (list :tag (vector (list :widget (list :wi (list :id id :props props)
                                          :alt (or alt '(:text ""))))
                      '(:text "")))))

(defconst lena-messages-test-diff-props
  '(:diff [(:text "end Prob" :type "unchanged") (:text "x" :type "deletion")
           (:text "e" :type "insertion")]
    :range (:end (:character 9 :line 3) :start (:character 0 :line 3))
    :suggestion "end Probe")
  "Props of a diff suggestion, as Lean sends them for `end Probx'.")

(ert-deftest lena-messages-test-diff-suggestion ()
  "Lean's hints show the change they suggest as a diff, to be applied."
  (let* ((text (lena-render-result-text
                (lena-messages-test-render
                 (vector (lena-messages-test-widget
                          "Lean.Meta.Hint.tryThisDiffWidget"
                          lena-messages-test-diff-props
                          '(:text "end Prob̲")))
                 3)))
         (pos (string-search "end Probxe" text)))
    (should pos)
    (should (memq 'lena-render-suggestion-deleted
                  (lena-test-faces text (+ pos 8))))
    (should (memq 'lena-render-suggestion-inserted
                  (lena-test-faces text (+ pos 9))))
    (should-not (memq 'lena-render-suggestion-inserted
                      (lena-test-faces text pos)))
    (should (equal (get-text-property pos 'lena-suggestion text)
                   '(:range (:end (:character 9 :line 3)
                             :start (:character 0 :line 3))
                     :suggestion "end Probe" :help "Apply suggestion")))
    (should (get-text-property (+ pos 9) 'lena-suggestion text))))

(ert-deftest lena-messages-test-explanation ()
  "The code of an error links to its explanation."
  (let* ((url "https://lean-lang.org/doc/reference/4.34.1/find/?domain=Manual.errorExplanation&name=lean.invalidField")
         (text (lena-render-result-text
                (lena-messages-test-render
                 (vector (lena-messages-test-message
                          (list :append
                                (vector '(:text "Invalid field `foo`")
                                        (list :tag (vector (list :widget (list :wi (list :id "Lean.errorDescriptionWidget"
                                                                                         :props (list :code "lean.invalidField"
                                                                                                      :explanationUrl url))
                                                                               :alt '(:text "")))
                                                           '(:text "")))))))
                 3))))
    (should (string-search "Invalid field `foo` [lean.invalidField]\n" text))
    (should (equal (get-text-property (string-search "[lean" text)
                                      'lena-explanation text)
                   url))))

;;;; The infoview:

(defvar lena-messages-test-requests nil
  "Calls of `lena-rpc-lazy-trace-children', newest first.")

(defmacro lena-messages-test-with-infoview (diagnostics line &rest body)
  "Run BODY in the infoview showing DIAGNOSTICS requested at LINE.
The source buffer is `lena-messages-test-source', with ten lines.  Calls
for the children of traces are recorded in `lena-messages-test-requests'
as (CHILDREN SUCCESS FAILURE OWNER)."
  (declare (indent 2) (debug t))
  `(lena-test-with-options
     (let ((lena-messages-test-requests nil)
           (source (generate-new-buffer "lena-messages-test-source")))
       (cl-letf (((symbol-function 'lena-rpc-handle-live-p) #'always)
                 ((symbol-function 'lena-rpc-lazy-trace-children)
                  (lambda (_handle children success &rest keys)
                    (push (list children success (plist-get keys :failure)
                                (plist-get keys :owner))
                          lena-messages-test-requests))))
         (unwind-protect
             (progn
               (with-current-buffer source
                 (dotimes (i 10) (insert (format "line %d\n" i))))
               (lena-messages-test-show ,diagnostics ,line source)
               (with-current-buffer (lena-infoview--buffer)
                 (switch-to-buffer (current-buffer))
                 (goto-char (point-min))
                 ,@body))
           (when-let* ((buffer (lena-infoview--buffer)))
             (kill-buffer buffer))
           (kill-buffer source))))))

(defun lena-messages-test-show (diagnostics line source)
  "Show DIAGNOSTICS requested at LINE of the buffer SOURCE."
  (lena-infoview--show nil nil (lena-rpc-make-owner) "A.lean:1:0" 'handle nil
                       (list :diagnostics diagnostics :line line
                             :buffer source
                             :tick (buffer-chars-modified-tick source))))

(ert-deftest lena-messages-test-restart-file ()
  "The button in a message about out-of-date imports restarts the file."
  (lena-messages-test-with-infoview (vector lena-messages-test-imports) 5
    (setq lena-infoview--messages
          (plist-put lena-infoview--messages :line-count 20))
    (lena-infoview--render)
    (lena-test-goto "[restart file]")
    (let (restarted)
      (cl-letf (((symbol-function 'lena-eglot-restart-file)
                 (lambda () (setq restarted (current-buffer)))))
        (push-button))
      (should (eq restarted source)))))

(ert-deftest lena-messages-test-folds-follow-point ()
  "A message folds and unfolds with point, until the user folds it."
  (lena-messages-test-with-infoview (lena-messages-test-fixture 2) 215
    (should-not (lena-test-visible-p "Invalid `end`"))
    ;; Point on its first line: unfolded.
    (lena-messages-test-show (lena-messages-test-fixture 2) 212 source)
    (should (lena-test-visible-p "Invalid `end`"))
    ;; Elsewhere again: folded.
    (lena-messages-test-show (lena-messages-test-fixture 2) 213 source)
    (should-not (lena-test-visible-p "Invalid `end`"))
    ;; Unfolded by the user, it stays so wherever point is.
    (lena-test-goto "213:0")
    (outline-show-subtree)
    (lena-messages-test-show (lena-messages-test-fixture 2) 214 source)
    (should (lena-test-visible-p "Invalid `end`"))
    ;; Folded by the user on its own line, too.
    (lena-messages-test-show (lena-messages-test-fixture 2) 212 source)
    (lena-test-goto "213:0")
    (outline-hide-subtree)
    (lena-messages-test-show (lena-messages-test-fixture 2) 212 source)
    (should-not (lena-test-visible-p "Invalid `end`"))))

(ert-deftest lena-messages-test-controls ()
  "Positions, suggestions and unexpanded traces react to keys and clicks."
  (lena-messages-test-with-infoview (lena-messages-test-fixture 3 4) 215
    (cl-flet ((type-at (string)
                (lena-test-goto string)
                (button-type (button-at (point)))))
      (should (eq (type-at "216:27") 'lena-infoview-message))
      (should (eq (type-at "[apply]") 'lena-infoview-suggestion))
      (should (eq (type-at "[Meta.synthInstance]") 'lena-infoview-trace))
      ;; TAB on a trace fetches it; on other buttons it still folds.
      (should (eq (lookup-key lena-infoview-trace-map (kbd "TAB"))
                  #'push-button))
      (should-not (lookup-key lena-infoview-button-map (kbd "TAB"))))
    ;; Rendering again replaces them.
    (let ((count (seq-count (lambda (overlay)
                              (overlay-get overlay 'lena-infoview-control))
                            (overlays-in (point-min) (point-max)))))
      (should (= count 4))
      (lena-infoview--render)
      (should (= count (seq-count (lambda (overlay)
                                    (overlay-get overlay 'lena-infoview-control))
                                  (overlays-in (point-min) (point-max))))))))

(ert-deftest lena-messages-test-goto ()
  "The position of a message leads to its place in the source."
  (lena-messages-test-with-infoview
      (vector (lena-messages-test-message '(:text "Boom")
                                          :range (lena-test-range
                                                  '(2 . 3) '(2 . 5))))
      2
    (with-current-buffer source
      (goto-char (point-min))
      (forward-line 2)
      ;; A character outside the BMP counts twice in LSP's columns.
      (insert "𝓝"))
    (lena-test-goto "3:3")
    (lena-infoview-goto-message)
    (should (eq (current-buffer) source))
    (should (equal (buffer-substring (line-beginning-position) (point))
                   "𝓝l"))))

(ert-deftest lena-messages-test-apply ()
  "A suggestion replaces its range in the source, undoably."
  (lena-messages-test-with-infoview
      (vector (lena-messages-test-message
               (list :tag (vector (list :widget
                                        (list :wi (list :id "Lean.Meta.Hint.textInsertionWidget"
                                                        :props (list :range (lena-test-range '(1 . 0) '(1 . 4))
                                                                     :suggestion "exact rfl"
                                                                     :acceptSuggestionProps
                                                                     '(:kind "text" :linkText "[apply]"
                                                                       :hoverText "Apply suggestion")))
                                              :alt '(:text "[apply]")))
                                  '(:text "")))))
      3
    (with-current-buffer source
      (buffer-enable-undo)
      (undo-boundary))
    (lena-test-goto "[apply]")
    (should (equal (get-char-property (point) 'help-echo) "Apply suggestion"))
    (lena-infoview-apply-suggestion)
    (should (eq (current-buffer) source))
    (should (equal (buffer-substring (line-beginning-position) (point))
                   "exact rfl"))
    (should (equal (buffer-substring (line-beginning-position)
                                     (line-end-position))
                   "exact rfl 1"))
    ;; One step of undo takes it back.
    (undo-start)
    (undo-more 1)
    (goto-char (point-min))
    (forward-line 1)
    (should (looking-at "line 1$"))
    ;; Once the source changed, the range may be wrong.
    (with-current-buffer (lena-infoview--buffer)
      (lena-test-goto "[apply]")
      (should-error (lena-infoview-apply-suggestion) :type 'user-error))))

(ert-deftest lena-messages-test-browse-explanation ()
  "The link of an error code opens its explanation in a browser."
  (lena-messages-test-with-infoview
      (vector (lena-messages-test-widget "Lean.errorDescriptionWidget"
                                         '(:code "lean.invalidField"
                                           :explanationUrl "https://example.org/x")))
      3
    (lena-test-goto "[lean.invalidField]")
    (let (browsed)
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url &rest _) (setq browsed url))))
        (push-button))
      (should (equal browsed "https://example.org/x")))))

(ert-deftest lena-messages-test-expand-trace ()
  "A trace's children are fetched on request, shown unfolded and kept."
  (lena-messages-test-with-infoview (lena-messages-test-fixture 3) 215
    (lena-test-goto "[Meta.synthInstance]")
    (lena-infoview-expand-trace)
    ;; Asking again while the answer is pending sends nothing.
    (lena-infoview-expand-trace)
    (should (= (length lena-messages-test-requests) 1))
    (pcase-let ((`(,children ,success ,_failure ,owner)
                 (car lena-messages-test-requests)))
      (should (equal children '(:__rpcref "2")))
      (should (eq owner lena-infoview--owner))
      (funcall success (vector (lena-messages-test-trace
                                "Meta.synthInstance" "child" '(:strict [])))))
    (should (lena-test-visible-p "[Meta.synthInstance] child"))
    (should (member '(message 215 27 "information" 0 0)
                    lena-infoview--expanded-traces))
    ;; No longer a trace to expand: TAB cycles as on other headings.
    (lena-test-goto "[Meta.synthInstance] ✅")
    (should-not (get-text-property (point) 'lena-trace-lazy))
    ;; Rendering again keeps them, without asking.
    (lena-infoview--render)
    (should (lena-test-visible-p "[Meta.synthInstance] child"))
    (should (= (length lena-messages-test-requests) 1))))

(ert-deftest lena-messages-test-expand-after-update ()
  "After an update, expanded traces are fetched again; late answers drop."
  (lena-messages-test-with-infoview (lena-messages-test-fixture 3) 215
    (lena-test-goto "[Meta.synthInstance]")
    (lena-infoview-expand-trace)
    (let ((late (nth 1 (car lena-messages-test-requests))))
      ;; New results arrive before the answer.
      (lena-messages-test-show (lena-messages-test-fixture 3) 215 source)
      (should (= (length lena-messages-test-requests) 2))
      (should (eq (nth 3 (car lena-messages-test-requests))
                  lena-infoview--owner))
      (funcall late (vector (lena-messages-test-trace
                             "Meta.synthInstance" "late" '(:strict []))))
      (should-not (lena-test-visible-p "late"))
      (funcall (nth 1 (car lena-messages-test-requests))
               (vector (lena-messages-test-trace
                        "Meta.synthInstance" "fresh" '(:strict []))))
      (should (lena-test-visible-p "fresh")))))

(ert-deftest lena-messages-test-expand-failures ()
  "Failures are reported; a dead session asks for an update."
  (lena-messages-test-with-infoview (lena-messages-test-fixture 3) 215
    (lena-test-goto "[Meta.synthInstance]")
    (lena-infoview-expand-trace)
    (let ((messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args)
                   (push (apply #'format-message format args) messages))))
        (funcall (nth 2 (car lena-messages-test-requests))
                 '(:code -32602 :message "Invalid RPC reference")))
      (should (string-search "Invalid RPC reference" (car messages))))
    (should-not lena-infoview--fetching)
    (cl-letf (((symbol-function 'lena-rpc-handle-live-p) #'ignore))
      (should-error (lena-infoview-expand-trace) :type 'user-error))))

(provide 'lena-messages-tests)

;;; lena-messages-tests.el ends here
