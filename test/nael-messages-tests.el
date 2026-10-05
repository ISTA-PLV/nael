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
(require 'nael-eglot-tests)
(require 'nael-infoview-tests)
(require 'nael-render-tests)

(defun nael-messages-test-fixture (&rest indices)
  "Return the recorded messages at INDICES, or all, as a vector."
  (let ((all (with-temp-buffer
               (insert-file-contents
                (expand-file-name "interactive-diagnostics.json"
                                  nael-render-test-fixtures))
               (json-parse-buffer :object-type 'plist :array-type 'array
                                  :null-object nil :false-object :json-false))))
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
          (list :range (nael-eglot-test-range '(3 . 2) '(3 . 8))
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
                  (nael-messages-test-fixture 6 8 9 11) 215)
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
                 (nael-messages-test-fixture 6 8 9) 215)))
    (should (equal (nael-messages-test-headings
                    (nael-render-result-text result))
                   '((1 1 nil nil nil)
                     (2 2 (message 212 0 "error") t nil)
                     (6 2 (message 215 27 "information") nil nil)
                     (7 3 (message 215 27 "information" 0) nil
                        (:__rpcref "2"))
                     (8 2 (message 217 2 "information") t nil))))
    ;; The section is a section of its own.
    (should (eq (get-text-property 0 'nael-section
                                   (nael-render-result-text result))
                'messages))))

(ert-deftest nael-messages-test-goals-accomplished ()
  "Messages that a proof is complete are left out; the goals say it."
  (should (string-prefix-p "Messages (5)\n"
                           (nael-messages-test-text
                            (nael-messages-test-fixture) 215)))
  (should (equal (nael-messages-test-text (nael-messages-test-fixture 0 1) 68)
                 ""))
  (should (equal (nael-messages-test-text [] 68) "")))

(ert-deftest nael-messages-test-nodes ()
  "Code in messages is made of nodes, as in goals."
  (let* ((result (nael-messages-test-render (nael-messages-test-fixture 11) 218))
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
    (dolist (case '(("error" . nael-render-message-error)
                    ("warning" . nael-render-message-warning)
                    ("information" . nael-render-message-info)
                    ("hint" . nael-render-message-info)))
      (should (memq (cdr case)
                    (nael-render-test-faces
                     text (string-search (car case) text)))))
    ;; The position knows the range of the message.
    (should (equal (get-text-property (string-search "4:2" text)
                                      'nael-message-range text)
                   (nael-eglot-test-range '(3 . 2) '(3 . 8))))))

(ert-deftest nael-messages-test-trace-children ()
  "Fetched children of a trace are shown under it, one level deeper."
  (let* ((children (make-hash-table :test #'equal))
         (key '(message 3 2 "error" 0)))
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
                     `((2 2 (message 3 2 "error") nil nil)
                       (3 3 ,key nil nil)
                       (4 4 (message 3 2 "error" 0 0) nil
                          (:__rpcref "8"))))))))

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
                   '((2 2 (message 3 2 "error") nil nil)
                     (3 3 (message 3 2 "error" 0) t nil)
                     (4 4 (message 3 2 "error" 0 0) nil nil)
                     (5 4 (message 3 2 "error" 0 1) nil nil)
                     (6 3 (message 3 2 "error" 1) nil nil))))
    (should (memq 'nael-render-trace-class
                  (nael-render-test-faces text (string-search "[a]" text))))))

(ert-deftest nael-messages-test-suggestion ()
  "Lean's suggestions become text that knows what to replace."
  (let* ((text (nael-render-result-text
                (nael-messages-test-render (nael-messages-test-fixture 9) 217)))
         (pos (string-search "[apply]" text)))
    (should (equal (get-text-property pos 'nael-suggestion text)
                   '(:range (:end (:character 8 :line 217)
                             :start (:character 2 :line 217))
                     :suggestion "exact Nat.add_comm a b"
                     :help "Apply suggestion")))
    (should (memq 'nael-render-suggestion (nael-render-test-faces text pos)))))

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
                                         (list :tag (vector (list :goal (nael-render-test-goal
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

;;;; The infoview:

(defvar nael-messages-test-requests nil
  "Calls of `nael-rpc-lazy-trace-children', newest first.")

(defmacro nael-messages-test-with-infoview (diagnostics line &rest body)
  "Run BODY in the infoview showing DIAGNOSTICS requested at LINE.
The source buffer is `nael-messages-test-source', with ten lines.  Calls
for the children of traces are recorded in `nael-messages-test-requests'
as (CHILDREN SUCCESS FAILURE OWNER)."
  (declare (indent 2) (debug t))
  `(nael-infoview-test-with-options
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

(defun nael-messages-test-visible-p (string)
  "Return non-nil if STRING is shown, not folded away, in the buffer."
  (save-excursion
    (goto-char (point-min))
    (and (search-forward string nil t)
         (not (invisible-p (match-beginning 0))))))

(ert-deftest nael-messages-test-folds ()
  "Messages from other lines start folded; folds are kept after that."
  (nael-messages-test-with-infoview (nael-messages-test-fixture 6 8 9) 215
    (should (nael-messages-test-visible-p "[Meta.synthInstance]"))
    (should-not (nael-messages-test-visible-p "Invalid `end`"))
    (should-not (nael-messages-test-visible-p "Try this"))
    ;; Unfolded by the user, it stays so.
    (nael-infoview-test-goto "213:0")
    (outline-show-subtree)
    (nael-infoview--render)
    (should (nael-messages-test-visible-p "Invalid `end`"))
    (should-not (nael-messages-test-visible-p "Try this"))))

(ert-deftest nael-messages-test-folds-follow-point ()
  "A message folds and unfolds with point, until the user folds it."
  (nael-messages-test-with-infoview (nael-messages-test-fixture 6) 215
    (should-not (nael-messages-test-visible-p "Invalid `end`"))
    ;; Point on its first line: unfolded.
    (nael-messages-test-show (nael-messages-test-fixture 6) 212 source)
    (should (nael-messages-test-visible-p "Invalid `end`"))
    ;; Elsewhere again: folded.
    (nael-messages-test-show (nael-messages-test-fixture 6) 213 source)
    (should-not (nael-messages-test-visible-p "Invalid `end`"))
    ;; Unfolded by the user, it stays so wherever point is.
    (nael-infoview-test-goto "213:0")
    (outline-show-subtree)
    (nael-messages-test-show (nael-messages-test-fixture 6) 214 source)
    (should (nael-messages-test-visible-p "Invalid `end`"))
    ;; Folded by the user on its own line, too.
    (nael-messages-test-show (nael-messages-test-fixture 6) 212 source)
    (nael-infoview-test-goto "213:0")
    (outline-hide-subtree)
    (nael-messages-test-show (nael-messages-test-fixture 6) 212 source)
    (should-not (nael-messages-test-visible-p "Invalid `end`"))))

(ert-deftest nael-messages-test-controls ()
  "Positions, suggestions and unexpanded traces react to keys and clicks."
  (nael-messages-test-with-infoview (nael-messages-test-fixture 8 9) 215
    (cl-flet ((map-at (string)
                (nael-infoview-test-goto string)
                (seq-some (lambda (overlay) (overlay-get overlay 'keymap))
                          (overlays-at (point)))))
      (should (eq (map-at "216:27") nael-infoview-message-map))
      (should (eq (map-at "[apply]") nael-infoview-suggestion-map))
      (should (eq (map-at "[Meta.synthInstance]") nael-infoview-trace-map))
      (should (eq (lookup-key nael-infoview-trace-map (kbd "TAB"))
                  #'nael-infoview-expand-trace)))
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
                                          :range (nael-eglot-test-range
                                                  '(2 . 3) '(2 . 5))))
      2
    (with-current-buffer source
      (goto-char (point-min))
      (forward-line 2)
      ;; A character outside the BMP counts twice in LSP's columns.
      (insert "𝓝"))
    (nael-infoview-test-goto "3:3")
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
                                                        :props (list :range (nael-eglot-test-range '(1 . 0) '(1 . 4))
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
    (nael-infoview-test-goto "[apply]")
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
      (nael-infoview-test-goto "[apply]")
      (should-error (nael-infoview-apply-suggestion) :type 'user-error))))

(ert-deftest nael-messages-test-expand-trace ()
  "A trace's children are fetched on request, shown unfolded and kept."
  (nael-messages-test-with-infoview (nael-messages-test-fixture 8) 215
    (nael-infoview-test-goto "[Meta.synthInstance]")
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
    (should (nael-messages-test-visible-p "[Meta.synthInstance] child"))
    (should (member '(message 215 27 "information" 0)
                    nael-infoview--expanded-traces))
    ;; No longer a trace to expand: TAB cycles as on other headings.
    (nael-infoview-test-goto "[Meta.synthInstance] ✅")
    (should-not (get-text-property (point) 'nael-trace-lazy))
    ;; Rendering again keeps them, without asking.
    (nael-infoview--render)
    (should (nael-messages-test-visible-p "[Meta.synthInstance] child"))
    (should (= (length nael-messages-test-requests) 1))))

(ert-deftest nael-messages-test-expand-after-update ()
  "After an update, expanded traces are fetched again; late answers drop."
  (nael-messages-test-with-infoview (nael-messages-test-fixture 8) 215
    (nael-infoview-test-goto "[Meta.synthInstance]")
    (nael-infoview-expand-trace)
    (let ((late (nth 1 (car nael-messages-test-requests))))
      ;; New results arrive before the answer.
      (nael-messages-test-show (nael-messages-test-fixture 8) 215 source)
      (should (= (length nael-messages-test-requests) 2))
      (should (eq (nth 3 (car nael-messages-test-requests))
                  nael-infoview--owner))
      (funcall late (vector (nael-messages-test-trace
                             "Meta.synthInstance" "late" '(:strict []))))
      (should-not (nael-messages-test-visible-p "late"))
      (funcall (nth 1 (car nael-messages-test-requests))
               (vector (nael-messages-test-trace
                        "Meta.synthInstance" "fresh" '(:strict []))))
      (should (nael-messages-test-visible-p "fresh")))))

(ert-deftest nael-messages-test-expand-failures ()
  "Failures are reported; a dead session asks for an update."
  (nael-messages-test-with-infoview (nael-messages-test-fixture 8) 215
    (nael-infoview-test-goto "[Meta.synthInstance]")
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
