;;; nael-render.el --- Render Lean's interactive goals  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see LICENSE.GPL3.

;;; Commentary:

;; This file turns the results of the Lean RPC methods
;; `Lean.Widget.getInteractiveGoals' and
;; `Lean.Widget.getInteractiveTermGoal', and the hover information of
;; `Lean.Widget.InteractiveDiagnostics.infoToInteractive', into
;; propertized text.  It has no side effects: it makes no requests and
;; touches no buffer but a temporary one, so that it can be tested on
;; recorded results.
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
;; occurrence of MVARID.  Metavariables change with every tactic, so
;; goals also carry `nael-key', which identifies them by their place
;; instead: (case NAME . N) for the Nth goal named NAME, (goal . N) for
;; the Nth goal without a name, and `term-goal' for the expected type.
;; Headings carry `outline-level'; section headings also carry
;; `nael-section', `goals', `term-goal' or `messages'.
;;
;; Messages, the results of `Lean.Widget.getInteractiveDiagnostics',
;; are `TaggedText' too, whose tags embed code, goals, traces and
;; widgets.  Each message is a heading of level 2 with the `nael-key'
;; (message LINE CHARACTER SEVERITY), and each trace in it a heading of
;; level 3 or deeper, with the key of the message followed by the
;; trace's path: its index among the traces of its parent, for each
;; enclosing trace.  Headings that a buffer should fold at first carry
;; `nael-folded'.  The server sends the children of a trace on request
;; only: its heading then carries `nael-trace-lazy', the reference for
;; `Lean.Widget.lazyTraceChildrenToInteractive'.  The position of a
;; message carries `nael-message-range', and a suggestion to apply
;; carries `nael-suggestion'.

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

(defface nael-render-goals-accomplished
  '((t :inherit success))
  "Face of the line that says that the proof at point is complete."
  :group 'nael-render)

(defface nael-render-message-error
  '((t :inherit compilation-error))
  "Face of the severity of an error in the messages."
  :group 'nael-render)

(defface nael-render-message-warning
  '((t :inherit compilation-warning))
  "Face of the severity of a warning in the messages."
  :group 'nael-render)

(defface nael-render-message-info
  '((t :inherit compilation-info))
  "Face of the severity of information or a hint in the messages."
  :group 'nael-render)

(defface nael-render-trace-class
  '((t :inherit shadow))
  "Face of the class of a trace, such as \"[Meta.synthInstance]\"."
  :group 'nael-render)

(defface nael-render-suggestion
  '((t :inherit button))
  "Face of a suggestion that can be applied, such as \"[apply]\"."
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

(defcustom nael-render-wrap-indent 4
  "Columns by which a wrapped line of code continues further in.
When a window is too narrow for a line, its continuation is indented
by this much more than the line itself, so that it reads as part of
it.  See `wrap-prefix'."
  :type 'natnum
  :group 'nael-render)

(defface nael-render-diff-inserted
  '((t :inherit diff-added :extend nil))
  "Face of what the tactic at point has added or changed."
  :group 'nael-render)

(defface nael-render-diff-removed
  '((t :inherit diff-removed :extend nil))
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

(defvar nael-render--diffs t
  "Non-nil if diffs are rendered; nil under the option `:hide-diffs'.")

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
            (diff (nael-render--flag-diff-face hyp)))
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

(defun nael-render--flag-diff-face (object)
  "Return the diff face for OBJECT, a goal or hypothesis, or nil.
Before a tactic, the server marks what it will remove; after it, what
it has inserted."
  (when nael-render--diffs
    (cond ((nael-render--true-p (plist-get object :isInserted))
           'nael-render-diff-inserted)
          ((nael-render--true-p (plist-get object :isRemoved))
           'nael-render-diff-removed))))

(defun nael-render--target (goal)
  "Insert the target of GOAL, with its turnstile, as the last line.
The turnstile shows whether the goal as a whole is removed or inserted."
  (let ((prefix (or (plist-get goal :goalPrefix) "⊢ ")))
    (nael-render--insert prefix (or (nael-render--flag-diff-face goal)
                                    'nael-render-goal-prefix))
    (nael-render--code (plist-get goal :type))
    (insert "\n")))

(defun nael-render--goal (goal heading key options)
  "Insert GOAL, an `InteractiveGoal' or `InteractiveTermGoal'.
If HEADING is non-nil, insert it as a level 2 heading first.  KEY is
stored in the property `nael-key'.  OPTIONS is a plist of view options;
see `nael-render-goals'."
  (let ((beg (point))
        (hyps (plist-get goal :hyps)))
    (when heading
      (nael-render--insert (propertize heading 'outline-level 2)
                           'nael-render-case)
      (when-let* ((diff (nael-render--flag-diff-face goal)))
        (push (list beg (point) diff) nael-render--faces))
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
    (put-text-property beg (point) 'nael-key key)
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
  (let ((count (length goals))
        (names nil))
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
       (let ((name (plist-get goal :userName)))
         (nael-render--goal
          goal
          (cond (name (format "case %s" name))
                ((> count 1) (format "goal %d" (1+ index))))
          (if name
              (cons 'case (cons name (seq-count (lambda (other)
                                                  (equal other name))
                                                names)))
            (cons 'goal index))
          options)
         (push name names)))
     goals)))

;;;; Messages:

(defvar nael-render--trace-children nil
  "Hash table of the fetched children of traces, by key, or nil.")

(defvar nael-render--trace-key nil
  "Key of the enclosing message or trace.")

(defvar nael-render--trace-count nil
  "A list whose car counts the traces rendered under the enclosing one.")

(defvar nael-render--options nil
  "The view options of the current rendering.")

(defun nael-render--severity (severity)
  "Return the name and face of the LSP SEVERITY as a cons."
  (pcase severity
    (2 '("warning" . nael-render-message-warning))
    (3 '("information" . nael-render-message-info))
    (4 '("hint" . nael-render-message-info))
    (_ '("error" . nael-render-message-error))))

(defun nael-render--fontify-code (beg end)
  "Fontify the code between BEG and END on its own.
Narrowed, so that text around it, like quotes in a message, does not
change how it is parsed."
  (save-restriction
    (narrow-to-region beg end)
    (nael-render--fontify beg end)))

(defun nael-render--msg (tagged)
  "Insert TAGGED, a `TaggedText' of message embeds, at point."
  (cond
   ((plist-member tagged :text)
    (insert (plist-get tagged :text)))
   ((plist-member tagged :append)
    (seq-doseq (child (plist-get tagged :append))
      (nael-render--msg child)))
   ((plist-member tagged :tag)
    (nael-render--embed (aref (plist-get tagged :tag) 0)))))

(defun nael-render--embed (embed)
  "Insert EMBED, a `MsgEmbed', at point."
  (cond
   ((plist-member embed :expr)
    (let ((beg (point)))
      (nael-render--code (plist-get embed :expr))
      (nael-render--fontify-code beg (point))))
   ((plist-member embed :goal)
    (let ((goal (plist-get embed :goal)))
      (unless (bolp) (insert "\n"))
      ;; The case as a line, not a heading: it belongs to the message.
      (when-let* ((name (plist-get goal :userName)))
        (nael-render--insert (format "case %s" name) 'nael-render-case)
        (insert "\n"))
      (nael-render--goal goal nil (cons 'message-goal nael-render--trace-key)
                         nael-render--options)))
   ((plist-member embed :trace)
    (nael-render--trace (plist-get embed :trace)))
   ((plist-member embed :widget)
    (nael-render--widget (plist-get embed :widget)))))

(defun nael-render--trace (trace)
  "Insert TRACE, a trace embed, as a heading with its children below.
Children that the server sends on request only are taken from
`nael-render--trace-children' if they were fetched."
  (let* ((key (append nael-render--trace-key
                      (list (prog1 (car nael-render--trace-count)
                              (cl-incf (car nael-render--trace-count))))))
         (children (plist-get trace :children))
         (lazy (plist-get children :lazy))
         (fetched (and lazy nael-render--trace-children
                       (gethash key nael-render--trace-children)))
         (kids (or (plist-get children :strict) fetched))
         (beg (progn (unless (bolp) (insert "\n")) (point))))
    (insert (make-string (* 2 (max 0 (- (length key) 5))) ?\s))
    (nael-render--insert (format "[%s] " (plist-get trace :cls))
                         'nael-render-trace-class)
    (nael-render--msg (plist-get trace :msg))
    ;; The heading is the trace's own line; its children follow.
    (add-text-properties
     beg (point)
     (append (list 'outline-level (- (length key) 2) 'nael-key key)
             (when (and kids (nael-render--true-p (plist-get trace :collapsed)))
               (list 'nael-folded t))
             (when (and lazy (not fetched))
               (list 'nael-trace-lazy lazy))))
    (insert "\n")
    (let ((nael-render--trace-key key)
          (nael-render--trace-count (list 0)))
      (seq-doseq (child kids)
        (nael-render--msg child)
        (unless (bolp) (insert "\n"))))))

(defun nael-render--widget (widget)
  "Insert WIDGET, a widget embed.
Lean's suggestions to replace code, such as \"Try this\", become text
with the property `nael-suggestion', a plist with the `:range' to
replace, the `:suggestion' and the `:help' to show.  Other widgets are
programs for VS Code; their alternative text is inserted instead."
  (let* ((instance (plist-get widget :wi))
         (props (plist-get instance :props)))
    (if (equal (plist-get instance :id) "Lean.Meta.Hint.textInsertionWidget")
        (let ((accept (plist-get props :acceptSuggestionProps))
              (beg (point)))
          (nael-render--insert (or (plist-get accept :linkText) "[apply]")
                               'nael-render-suggestion)
          (put-text-property beg (point) 'nael-suggestion
                             (list :range (plist-get props :range)
                                   :suggestion (plist-get props :suggestion)
                                   :help (plist-get accept :hoverText))))
      (nael-render--msg (plist-get widget :alt)))))

(defun nael-render--message (diagnostic line)
  "Insert DIAGNOSTIC, an `InteractiveDiagnostic', as a heading and text.
LINE is the 0-based line at which the messages were requested; a
message that starts on another line is folded at first."
  (let* ((range (or (plist-get diagnostic :fullRange)
                    (plist-get diagnostic :range)))
         (start (plist-get range :start))
         (severity (nael-render--severity (plist-get diagnostic :severity)))
         (key (list 'message (plist-get start :line) (plist-get start :character)
                    (car severity)))
         (beg (point)))
    (insert (propertize (format "%d:%d" (1+ (plist-get start :line))
                                (plist-get start :character))
                        'nael-message-range range)
            " ")
    (nael-render--insert (car severity) (cdr severity))
    (add-text-properties beg (point)
                         (append (list 'outline-level 2 'nael-key key)
                                 (unless (eql (plist-get start :line) line)
                                   (list 'nael-folded t))))
    (insert "\n")
    (let ((nael-render--trace-key key)
          (nael-render--trace-count (list 0)))
      (nael-render--msg (plist-get diagnostic :message)))
    (unless (bolp) (insert "\n"))))

(defun nael-render--messages-section (messages)
  "Insert the section for MESSAGES; see `nael-render-goals'.
That a proof is complete is shown above the goals, so its message is
left out."
  (let ((diagnostics (seq-remove
                      (lambda (diagnostic)
                        (seq-contains-p (plist-get diagnostic :leanTags) 2))
                      (plist-get messages :diagnostics)))
        (nael-render--trace-children (plist-get messages :trace-children)))
    (when diagnostics
      (unless (bobp) (insert "\n"))
      (nael-render--heading (format "Messages (%d)" (length diagnostics))
                            'messages)
      (dolist (diagnostic diagnostics)
        (nael-render--message diagnostic (plist-get messages :line))))))

(defun nael-render--apply-faces ()
  "Add the noted faces, then the diff faces of the rendered nodes."
  (pcase-dolist (`(,beg ,end ,face) (reverse nael-render--faces))
    (add-face-text-property beg end face))
  (dolist (node (and nael-render--diffs nael-render--nodes))
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

(defun nael-render-goals (goals term-goal &optional options messages)
  "Render GOALS, TERM-GOAL and MESSAGES as a `nael-render-result'.

GOALS is the result of `Lean.Widget.getInteractiveGoals' and TERM-GOAL
that of `Lean.Widget.getInteractiveTermGoal'; either may be nil.  A
section is rendered for each of them that is non-nil.

MESSAGES, if non-nil, is a plist: `:diagnostics', the result of
`Lean.Widget.getInteractiveDiagnostics'; `:line', the 0-based line at
which they were requested; and `:trace-children', a hash table of the
children of traces that were fetched, by the key of the trace.

OPTIONS is a plist of view options:
  :hide-instances    hide instance hypotheses, such as `inst✝ : Ring R';
  :hide-types        hide hypotheses that are types, such as `α : Type';
  :hide-inaccessible hide inaccessible names, such as `n✝';
  :hide-let-values   hide the values of let-bound hypotheses;
  :reverse           show the target first, then hypotheses upwards;
  :hide-diffs        do not mark what the tactic at point changes;
  :accomplished      a line to show first if the proof at point is
                     complete, such as \"Goals accomplished\"; it
                     replaces the section of GOALS when there are none."
  (let ((nael-render--diffs (not (plist-get options :hide-diffs)))
        (nael-render--options options)
        (nael-render--nodes nil)
        (nael-render--goal-ids nil)
        (nael-render--faces nil))
    (with-temp-buffer
      ;; As in lean.nvim, a line above the goals, which are not folded
      ;; with it; it stands for no goals at all.
      (when-let* ((accomplished (plist-get options :accomplished)))
        (nael-render--insert accomplished 'nael-render-goals-accomplished)
        (insert "\n"))
      (when (and goals
                 (not (and (plist-get options :accomplished)
                           (seq-empty-p (plist-get goals :goals)))))
        (nael-render--goals-section (plist-get goals :goals) options))
      (when term-goal
        (unless (bobp) (insert "\n"))
        (nael-render--heading "Expected type" 'term-goal)
        (nael-render--goal term-goal nil 'term-goal options))
      (when messages
        (nael-render--messages-section messages))
      (nael-render--finish))))

(defun nael-render--put-wrap-prefixes ()
  "Indent the continuations of wrapped lines in the current buffer.
See `nael-render-wrap-indent'."
  (goto-char (point-min))
  (while (not (eobp))
    (let ((beg (point)))
      (skip-chars-forward " ")
      (put-text-property beg (min (1+ (line-end-position)) (point-max))
                         'wrap-prefix
                         (make-string (+ (- (point) beg)
                                         nael-render-wrap-indent)
                                      ?\s))
      (forward-line))))

(defun nael-render--finish ()
  "Return the rendered contents of the current buffer as a result."
  (nael-render--apply-faces)
  (nael-render--put-nodes)
  (nael-render--put-wrap-prefixes)
  (nael-render--result-create
   :text (buffer-string)
   :nodes (reverse nael-render--nodes)))

(defun nael-render-info (info)
  "Render INFO, a result of `infoToInteractive', as a `nael-render-result'.
The text is the expression with all its arguments made explicit, then
its type, as in \"EXPR : TYPE\"; either may be missing.  The
documentation in INFO is not rendered."
  (let ((nael-render--diffs nil)
        (nael-render--nodes nil)
        (nael-render--faces nil)
        (expr (plist-get info :exprExplicit))
        (type (plist-get info :type)))
    (with-temp-buffer
      (when expr (nael-render--code expr))
      (when (and expr type) (insert " : "))
      (when type (nael-render--code type))
      (nael-render--fontify (point-min) (point-max))
      (nael-render--finish))))

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
