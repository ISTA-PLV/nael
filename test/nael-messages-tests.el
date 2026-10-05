;;; nael-messages-tests.el --- Tests for messages in the infoview  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see nael/LICENSE.GPL3.

;;; Commentary:

;; Tests for the messages in the infoview that need no Lean server:
;; their rendering in `nael-render', on recorded and handmade messages;
;; and the infoview's folds, links, suggestions and traces, with the
;; calls to the server replaced.

;;; Code:

(require 'ert)
(require 'nael-infoview)
(require 'nael-test-helpers)

(defun nael-messages-test-fixture (&rest indices)
  "Return the recorded messages at INDICES, or all, as a vector."
  (let ((all (nael-test-read-fixture "interactive-diagnostics.json")))
    (if indices
        (vconcat (mapcar (lambda (index) (aref all index)) indices))
      all)))

(defun nael-messages-test-render (diagnostics line &optional children)
  "Render DIAGNOSTICS requested at LINE, with fetched trace CHILDREN."
  (nael-render-goals nil nil nil (list :diagnostics diagnostics :line line
                                       :trace-children children)))

(defun nael-messages-test-text (&rest args)
  "Return the plain text of `nael-messages-test-render' with ARGS."
  (substring-no-properties
   (nael-render-result-text (apply #'nael-messages-test-render args))))

(defun nael-messages-test-headings (string)
  "Return the headings of STRING as (LINE LEVEL KEY FOLDED LAZY)."
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (let (headings)
      (while (not (eobp))
        (when-let* ((level (get-text-property (point) 'outline-level)))
          (push (list (line-number-at-pos) level
                      (get-text-property (point) 'nael-key)
                      (get-text-property (point) 'nael-folded)
                      (get-text-property (point) 'nael-trace-lazy))
                headings))
        (forward-line))
      (nreverse headings))))

(defun nael-messages-test-message (message &rest keys)
  "Return a diagnostic with MESSAGE, a `TaggedText', merged with KEYS.
It is an error on line 3 unless KEYS say otherwise."
  (append keys
          (list :range (nael-test-range '(3 . 2) '(3 . 8))
                :severity 1
                :message message)))

(defun nael-messages-test-trace (cls text children &rest keys)
  "Return a trace embed of CLS with TEXT and CHILDREN, merged with KEYS."
  (list :tag (vector (list :trace (append keys
                                          (list :indent 0 :cls cls
                                                :collapsed :json-false
                                                :msg (list :text text)
                                                :children children)))
                     (list :text ""))))

;;;; Rendering:

(ert-deftest nael-messages-test-fixture-text ()
  "Recorded messages: an error, a trace, a suggestion and a #check."
  (should (equal (nael-messages-test-text
                  (nael-messages-test-fixture 2 3 4 5) 215)
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

(ert-deftest nael-messages-test-headings ()
  "Messages and traces are headings; those not on the line start folded."
  (let ((result (nael-messages-test-render
                 (nael-messages-test-fixture 2 3 4) 215)))
    (should (equal (nael-messages-test-headings
                    (nael-render-result-text result))
                   '((1 1 nil nil nil)
                     (2 2 (message 212 0 "error" 0) t nil)
                     (6 2 (message 215 27 "information" 0) nil nil)
                     (7 3 (message 215 27 "information" 0 0) nil
                        (:__rpcref "2"))
                     (8 2 (message 217 2 "information" 0) t nil))))
    ;; The section is a section of its own.
    (should (eq (get-text-property 0 'nael-section
                                   (nael-render-result-text result))
                'messages))))

(ert-deftest nael-messages-test-goals-accomplished ()
  "Messages that a proof is complete are left out; the goals say it."
  (should (string-prefix-p "Messages (4)\n"
                           (nael-messages-test-text
                            (nael-messages-test-fixture) 215)))
  (should (equal (nael-messages-test-text (nael-messages-test-fixture 0 1) 68)
                 ""))
  (should (equal (nael-messages-test-text [] 68) "")))

(ert-deftest nael-messages-test-nodes ()
  "Code in messages is made of nodes, as in goals."
  (let* ((result (nael-messages-test-render (nael-messages-test-fixture 5) 218))
         (text (nael-render-result-text result))
         (nodes (nael-render-result-nodes result)))
    (should nodes)
    (dolist (node nodes)
      (should (nael-render-node-info node))
      (should (< (nael-render-node-beg node) (nael-render-node-end node))))
    (should (seq-some (lambda (node)
                        (equal (substring-no-properties
                                text (1- (nael-render-node-beg node))
                                (1- (nael-render-node-end node)))
                               "n + m = m + n"))
                      nodes))))

(ert-deftest nael-messages-test-severities ()
  "The severity follows the position, in its face."
  (let ((text (nael-render-result-text
               (nael-messages-test-render
                (vector (nael-messages-test-message '(:text "e"))
                        (nael-messages-test-message '(:text "w") :severity 2)
                        (nael-messages-test-message '(:text "i") :severity 3)
                        (nael-messages-test-message '(:text "h") :severity 4))
                3))))
    (should (equal (substring-no-properties text)
                   "Messages (4)\n4:2 error\ne\n4:2 warning\nw\n4:2 information\ni\n4:2 hint\nh\n"))
    (dolist (case '(("error" . compilation-error)
                    ("warning" . compilation-warning)
                    ("information" . compilation-info)
                    ("hint" . compilation-info)))
      (should (memq (cdr case)
                    (nael-test-faces
                     text (string-search (car case) text)))))
    ;; The position knows the range of the message.
    (should (equal (get-text-property (string-search "4:2" text)
                                      'nael-message-range text)
                   (nael-test-range '(3 . 2) '(3 . 8))))))

(ert-deftest nael-messages-test-trace-children ()
  "Fetched children of a trace are shown under it, one level deeper."
  (let* ((children (make-hash-table :test #'equal))
         (key '(message 3 2 "error" 0 0)))
    (puthash key
             (vector (nael-messages-test-trace
                      "Meta.synthInstance" "child" '(:lazy (:__rpcref "8"))))
             children)
    (let ((text (nael-render-result-text
                 (nael-messages-test-render
                  (vector (nael-messages-test-message
                           (nael-messages-test-trace
                            "Meta.synthInstance" "top"
                            '(:lazy (:__rpcref "7")))))
                  3 children))))
      (should (equal (substring-no-properties text)
                     "Messages (1)\n4:2 error\n[Meta.synthInstance] top\n  [Meta.synthInstance] child\n"))
      (should (equal (cdr (nael-messages-test-headings text))
                     `((2 2 (message 3 2 "error" 0) nil nil)
                       (3 3 ,key nil nil)
                       (4 4 (message 3 2 "error" 0 0 0) nil
                          (:__rpcref "8"))))))))

(ert-deftest nael-messages-test-same-start ()
  "Messages with the same start and severity keep their traces apart."
  (let ((children (make-hash-table :test #'equal))
        (message (lambda (ref)
                   (nael-messages-test-message
                    (nael-messages-test-trace "a" ref (list :lazy (list :__rpcref ref)))))))
    (puthash '(message 3 2 "error" 0 0)
             (vector (nael-messages-test-trace "a" "child" '(:strict [])))
             children)
    (let ((text (nael-render-result-text
                 (nael-messages-test-render
                  (vector (funcall message "1") (funcall message "2"))
                  3 children))))
      (should (equal (substring-no-properties text)
                     "Messages (2)\n4:2 error\n[a] 1\n  [a] child\n4:2 error\n[a] 2\n"))
      (should (equal (mapcar (lambda (heading) (list (nth 2 heading) (nth 4 heading)))
                             (cdr (nael-messages-test-headings text)))
                     '(((message 3 2 "error" 0) nil)
                       ((message 3 2 "error" 0 0) nil)
                       ((message 3 2 "error" 0 0 0) nil)
                       ((message 3 2 "error" 1) nil)
                       ((message 3 2 "error" 1 0) (:__rpcref "2"))))))))

(ert-deftest nael-messages-test-strict-trace ()
  "Children sent with a trace are shown; a collapsed trace starts folded."
  (let ((text (nael-render-result-text
               (nael-messages-test-render
                (vector (nael-messages-test-message
                         (list :append
                               (vector
                                (nael-messages-test-trace
                                 "a" "one"
                                 (list :strict
                                       (vector (nael-messages-test-trace
                                                "a.b" "inner" '(:strict []))
                                               (nael-messages-test-trace
                                                "a.b" "inner2" '(:strict []))))
                                 :collapsed t)
                                (nael-messages-test-trace
                                 "a" "two" '(:strict []))))))
                3))))
    (should (equal (substring-no-properties text)
                   "Messages (1)\n4:2 error\n[a] one\n  [a.b] inner\n  [a.b] inner2\n[a] two\n"))
    (should (equal (cdr (nael-messages-test-headings text))
                   '((2 2 (message 3 2 "error" 0) nil nil)
                     (3 3 (message 3 2 "error" 0 0) t nil)
                     (4 4 (message 3 2 "error" 0 0 0) nil nil)
                     (5 4 (message 3 2 "error" 0 0 1) nil nil)
                     (6 3 (message 3 2 "error" 0 1) nil nil))))
    (should (memq 'nael-render-trace-class
                  (nael-test-faces text (string-search "[a]" text))))))

(ert-deftest nael-messages-test-suggestion ()
  "Lean's suggestions become text that knows what to replace."
  (let* ((text (nael-render-result-text
                (nael-messages-test-render (nael-messages-test-fixture 4) 217)))
         (pos (string-search "[apply]" text)))
    (should (equal (get-text-property pos 'nael-suggestion text)
                   '(:range (:end (:character 8 :line 217)
                             :start (:character 2 :line 217))
                     :suggestion "exact Nat.add_comm a b"
                     :help "Apply suggestion")))
    (should (memq 'nael-render-suggestion (nael-test-faces text pos)))))

(ert-deftest nael-messages-test-other-widgets ()
  "Other widgets show their alternative text."
  (should (equal (nael-messages-test-text
                  (vector (nael-messages-test-message
                           (list :tag (vector (list :widget
                                                    (list :wi (list :id "ProofWidgets.Foo"
                                                                    :props nil)
                                                          :alt '(:text "alternative")))
                                              '(:text "")))))
                  3)
                 "Messages (1)\n4:2 error\nalternative\n")))

(ert-deftest nael-messages-test-goal ()
  "Goals in messages are shown as goals, on lines of their own."
  (let* ((result (nael-messages-test-render
                  (vector (nael-messages-test-message
                           (list :append
                                 (vector '(:text "unsolved goals")
                                         (list :tag (vector (list :goal (nael-test-goal
                                                                         :userName "right"))
                                                            '(:text "")))))))
                  3))
         (text (nael-render-result-text result)))
    (should (equal (substring-no-properties text)
                   "Messages (1)\n4:2 error\nunsolved goals\ncase right\n⊢ n = 0\n"))
    ;; Not a heading: the message is one.
    (should-not (get-text-property (string-search "case" text)
                                   'outline-level text))
    (should (= (length (nael-render-result-nodes result)) 1))))

(defconst nael-messages-test-imports
  (nael-test-diagnostic
   3 '(0 . 0) '(19 . 0)
   :message '(:text "Imports are out of date and should be rebuilt; use the \"Restart File\" command in your editor."))
  "A message about the whole file of 20 lines that its imports are out of date.")

(ert-deftest nael-messages-test-whole-file ()
  "Messages about the whole file are unfolded, first, and may restart it."
  (let* ((other (nael-test-diagnostic 1 '(5 . 0) '(5 . 4)
                                      :message '(:text "Boom")))
         (crash (nael-test-diagnostic
                 1 '(0 . 0) '(19 . 0) :message '(:text "Server crashed")))
         (text (nael-render-result-text
                (nael-render-goals nil nil nil
                                   (list :diagnostics
                                         (vector other nael-messages-test-imports
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
                           (cdr (nael-messages-test-headings text)))
                   '(nil nil nil)))
    (should (get-text-property (string-search "[restart file]" text)
                               'nael-restart-file text)))
  ;; Without the number of lines, such a message is like any other.
  (let ((text (nael-render-result-text
               (nael-messages-test-render (vector nael-messages-test-imports)
                                          5))))
    (should-not (string-search "[restart file]" text))
    (should (nth 3 (cadr (nael-messages-test-headings text))))))

(defun nael-messages-test-widget (id props &optional alt)
  "Return a message embedding the widget ID with PROPS and ALT."
  (nael-messages-test-message
   (list :tag (vector (list :widget (list :wi (list :id id :props props)
                                          :alt (or alt '(:text ""))))
                      '(:text "")))))

(defconst nael-messages-test-diff-props
  '(:diff [(:text "end Prob" :type "unchanged") (:text "x" :type "deletion")
           (:text "e" :type "insertion")]
    :range (:end (:character 9 :line 3) :start (:character 0 :line 3))
    :suggestion "end Probe")
  "Props of a diff suggestion, as Lean sends them for `end Probx'.")

(ert-deftest nael-messages-test-diff-suggestion ()
  "Lean's hints show the change they suggest as a diff, to be applied."
  (let* ((text (nael-render-result-text
                (nael-messages-test-render
                 (vector (nael-messages-test-widget
                          "Lean.Meta.Hint.tryThisDiffWidget"
                          nael-messages-test-diff-props
                          '(:text "end Prob̲")))
                 3)))
         (pos (string-search "end Probxe" text)))
    (should pos)
    (should (memq 'nael-render-suggestion-deleted
                  (nael-test-faces text (+ pos 8))))
    (should (memq 'nael-render-suggestion-inserted
                  (nael-test-faces text (+ pos 9))))
    (should-not (memq 'nael-render-suggestion-inserted
                      (nael-test-faces text pos)))
    (should (equal (get-text-property pos 'nael-suggestion text)
                   '(:range (:end (:character 9 :line 3)
                             :start (:character 0 :line 3))
                     :suggestion "end Probe" :help "Apply suggestion")))
    (should (get-text-property (+ pos 9) 'nael-suggestion text))))

(ert-deftest nael-messages-test-explanation ()
  "The code of an error links to its explanation."
  (let* ((url "https://lean-lang.org/doc/reference/4.34.1/find/?domain=Manual.errorExplanation&name=lean.invalidField")
         (text (nael-render-result-text
                (nael-messages-test-render
                 (vector (nael-messages-test-message
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
                                      'nael-explanation text)
                   url))))

;;;; The infoview:

(defvar nael-messages-test-requests nil
  "Calls of `nael-rpc-lazy-trace-children', newest first.")

(defmacro nael-messages-test-with-infoview (diagnostics line &rest body)
  "Run BODY in the infoview showing DIAGNOSTICS requested at LINE.
The source buffer is `nael-messages-test-source', with ten lines.  Calls
for the children of traces are recorded in `nael-messages-test-requests'
as (CHILDREN SUCCESS FAILURE OWNER)."
  (declare (indent 2) (debug t))
  `(nael-test-with-options
     (let ((nael-messages-test-requests nil)
           (source (generate-new-buffer "nael-messages-test-source")))
       (cl-letf (((symbol-function 'nael-rpc-handle-live-p) #'always)
                 ((symbol-function 'nael-rpc-lazy-trace-children)
                  (lambda (_handle children success &rest keys)
                    (push (list children success (plist-get keys :failure)
                                (plist-get keys :owner))
                          nael-messages-test-requests))))
         (unwind-protect
             (progn
               (with-current-buffer source
                 (dotimes (i 10) (insert (format "line %d\n" i))))
               (nael-messages-test-show ,diagnostics ,line source)
               (with-current-buffer (nael-infoview--buffer)
                 (switch-to-buffer (current-buffer))
                 (goto-char (point-min))
                 ,@body))
           (when-let* ((buffer (nael-infoview--buffer)))
             (kill-buffer buffer))
           (kill-buffer source))))))

(defun nael-messages-test-show (diagnostics line source)
  "Show DIAGNOSTICS requested at LINE of the buffer SOURCE."
  (nael-infoview--show nil nil (nael-rpc-make-owner) "A.lean:1:0" 'handle nil
                       (list :diagnostics diagnostics :line line
                             :buffer source
                             :tick (buffer-chars-modified-tick source))))

(ert-deftest nael-messages-test-restart-file ()
  "The button in a message about out-of-date imports restarts the file."
  (nael-messages-test-with-infoview (vector nael-messages-test-imports) 5
    (setq nael-infoview--messages
          (plist-put nael-infoview--messages :line-count 20))
    (nael-infoview--render)
    (nael-test-goto "[restart file]")
    (let (restarted)
      (cl-letf (((symbol-function 'nael-eglot-restart-file)
                 (lambda () (setq restarted (current-buffer)))))
        (push-button))
      (should (eq restarted source)))))

(ert-deftest nael-messages-test-folds-follow-point ()
  "A message folds and unfolds with point, until the user folds it."
  (nael-messages-test-with-infoview (nael-messages-test-fixture 2) 215
    (should-not (nael-test-visible-p "Invalid `end`"))
    ;; Point on its first line: unfolded.
    (nael-messages-test-show (nael-messages-test-fixture 2) 212 source)
    (should (nael-test-visible-p "Invalid `end`"))
    ;; Elsewhere again: folded.
    (nael-messages-test-show (nael-messages-test-fixture 2) 213 source)
    (should-not (nael-test-visible-p "Invalid `end`"))
    ;; Unfolded by the user, it stays so wherever point is.
    (nael-test-goto "213:0")
    (outline-show-subtree)
    (nael-messages-test-show (nael-messages-test-fixture 2) 214 source)
    (should (nael-test-visible-p "Invalid `end`"))
    ;; Folded by the user on its own line, too.
    (nael-messages-test-show (nael-messages-test-fixture 2) 212 source)
    (nael-test-goto "213:0")
    (outline-hide-subtree)
    (nael-messages-test-show (nael-messages-test-fixture 2) 212 source)
    (should-not (nael-test-visible-p "Invalid `end`"))))

(ert-deftest nael-messages-test-controls ()
  "Positions, suggestions and unexpanded traces react to keys and clicks."
  (nael-messages-test-with-infoview (nael-messages-test-fixture 3 4) 215
    (cl-flet ((type-at (string)
                (nael-test-goto string)
                (button-type (button-at (point)))))
      (should (eq (type-at "216:27") 'nael-infoview-message))
      (should (eq (type-at "[apply]") 'nael-infoview-suggestion))
      (should (eq (type-at "[Meta.synthInstance]") 'nael-infoview-trace))
      ;; TAB on a trace fetches it; on other buttons it still folds.
      (should (eq (lookup-key nael-infoview-trace-map (kbd "TAB"))
                  #'push-button))
      (should-not (lookup-key nael-infoview-button-map (kbd "TAB"))))
    ;; Rendering again replaces them.
    (let ((count (seq-count (lambda (overlay)
                              (overlay-get overlay 'nael-infoview-control))
                            (overlays-in (point-min) (point-max)))))
      (should (= count 4))
      (nael-infoview--render)
      (should (= count (seq-count (lambda (overlay)
                                    (overlay-get overlay 'nael-infoview-control))
                                  (overlays-in (point-min) (point-max))))))))

(ert-deftest nael-messages-test-goto ()
  "The position of a message leads to its place in the source."
  (nael-messages-test-with-infoview
      (vector (nael-messages-test-message '(:text "Boom")
                                          :range (nael-test-range
                                                  '(2 . 3) '(2 . 5))))
      2
    (with-current-buffer source
      (goto-char (point-min))
      (forward-line 2)
      ;; A character outside the BMP counts twice in LSP's columns.
      (insert "𝓝"))
    (nael-test-goto "3:3")
    (nael-infoview-goto-message)
    (should (eq (current-buffer) source))
    (should (equal (buffer-substring (line-beginning-position) (point))
                   "𝓝l"))))

(ert-deftest nael-messages-test-apply ()
  "A suggestion replaces its range in the source, undoably."
  (nael-messages-test-with-infoview
      (vector (nael-messages-test-message
               (list :tag (vector (list :widget
                                        (list :wi (list :id "Lean.Meta.Hint.textInsertionWidget"
                                                        :props (list :range (nael-test-range '(1 . 0) '(1 . 4))
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
    (nael-test-goto "[apply]")
    (should (equal (get-char-property (point) 'help-echo) "Apply suggestion"))
    (nael-infoview-apply-suggestion)
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
    (with-current-buffer (nael-infoview--buffer)
      (nael-test-goto "[apply]")
      (should-error (nael-infoview-apply-suggestion) :type 'user-error))))

(ert-deftest nael-messages-test-browse-explanation ()
  "The link of an error code opens its explanation in a browser."
  (nael-messages-test-with-infoview
      (vector (nael-messages-test-widget "Lean.errorDescriptionWidget"
                                         '(:code "lean.invalidField"
                                           :explanationUrl "https://example.org/x")))
      3
    (nael-test-goto "[lean.invalidField]")
    (let (browsed)
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url &rest _) (setq browsed url))))
        (push-button))
      (should (equal browsed "https://example.org/x")))))

(ert-deftest nael-messages-test-expand-trace ()
  "A trace's children are fetched on request, shown unfolded and kept."
  (nael-messages-test-with-infoview (nael-messages-test-fixture 3) 215
    (nael-test-goto "[Meta.synthInstance]")
    (nael-infoview-expand-trace)
    ;; Asking again while the answer is pending sends nothing.
    (nael-infoview-expand-trace)
    (should (= (length nael-messages-test-requests) 1))
    (pcase-let ((`(,children ,success ,_failure ,owner)
                 (car nael-messages-test-requests)))
      (should (equal children '(:__rpcref "2")))
      (should (eq owner nael-infoview--owner))
      (funcall success (vector (nael-messages-test-trace
                                "Meta.synthInstance" "child" '(:strict [])))))
    (should (nael-test-visible-p "[Meta.synthInstance] child"))
    (should (member '(message 215 27 "information" 0 0)
                    nael-infoview--expanded-traces))
    ;; No longer a trace to expand: TAB cycles as on other headings.
    (nael-test-goto "[Meta.synthInstance] ✅")
    (should-not (get-text-property (point) 'nael-trace-lazy))
    ;; Rendering again keeps them, without asking.
    (nael-infoview--render)
    (should (nael-test-visible-p "[Meta.synthInstance] child"))
    (should (= (length nael-messages-test-requests) 1))))

(ert-deftest nael-messages-test-expand-after-update ()
  "After an update, expanded traces are fetched again; late answers drop."
  (nael-messages-test-with-infoview (nael-messages-test-fixture 3) 215
    (nael-test-goto "[Meta.synthInstance]")
    (nael-infoview-expand-trace)
    (let ((late (nth 1 (car nael-messages-test-requests))))
      ;; New results arrive before the answer.
      (nael-messages-test-show (nael-messages-test-fixture 3) 215 source)
      (should (= (length nael-messages-test-requests) 2))
      (should (eq (nth 3 (car nael-messages-test-requests))
                  nael-infoview--owner))
      (funcall late (vector (nael-messages-test-trace
                             "Meta.synthInstance" "late" '(:strict []))))
      (should-not (nael-test-visible-p "late"))
      (funcall (nth 1 (car nael-messages-test-requests))
               (vector (nael-messages-test-trace
                        "Meta.synthInstance" "fresh" '(:strict []))))
      (should (nael-test-visible-p "fresh")))))

(ert-deftest nael-messages-test-expand-failures ()
  "Failures are reported; a dead session asks for an update."
  (nael-messages-test-with-infoview (nael-messages-test-fixture 3) 215
    (nael-test-goto "[Meta.synthInstance]")
    (nael-infoview-expand-trace)
    (let ((messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args)
                   (push (apply #'format-message format args) messages))))
        (funcall (nth 2 (car nael-messages-test-requests))
                 '(:code -32602 :message "Invalid RPC reference")))
      (should (string-search "Invalid RPC reference" (car messages))))
    (should-not nael-infoview--fetching)
    (cl-letf (((symbol-function 'nael-rpc-handle-live-p) #'ignore))
      (should-error (nael-infoview-expand-trace) :type 'user-error))))

(provide 'nael-messages-tests)

;;; nael-messages-tests.el ends here
