;;; lena-render.el --- Render Lean's interactive goals  -*- lexical-binding: t; -*-

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
;; (`lena-render-node'), stored in the text property `lena-node' of its
;; characters.  Text properties do not nest, so a character carries the
;; innermost node.  The result also lists all nodes, so that a buffer can
;; put overlays on their exact extents.
;;
;; Each goal carries the text property `lena-goal', a token of its own,
;; so that adjacent goals can be told apart, and `lena-key', which
;; identifies it by its place so that state such as folding survives a
;; refresh: (case NAME . N) for the Nth goal named NAME, (goal . N) for
;; the Nth goal without a name, and `term-goal' for the expected type.
;; Headings carry `outline-level'; section headings also carry
;; `lena-section', `goals', `term-goal' or `messages'.
;;
;; Messages, the results of `Lean.Widget.getInteractiveDiagnostics',
;; are `TaggedText' too, whose tags embed code, goals, traces and
;; widgets.  Each message is a heading of level 2 with the `lena-key'
;; (message LINE CHARACTER SEVERITY N), where N counts the messages
;; before it with the same start and severity, and each trace in it a
;; heading of level 3 or deeper, with the key of the message followed
;; by the trace's path: its index among the traces of its parent, for
;; each enclosing trace.  Headings that a buffer should fold at first carry
;; `lena-folded'.  The server sends the children of a trace on request
;; only: its heading then carries `lena-trace-lazy', the reference for
;; `Lean.Widget.lazyTraceChildrenToInteractive'.  The position of a
;; message carries `lena-message-range', a suggestion to apply carries
;; `lena-suggestion', a link to the explanation of an error carries
;; `lena-explanation', and a button to restart the file, in a message
;; that its imports are out of date, carries `lena-restart-file'.

;;; Code:

(require 'cl-lib)
(require 'seq)

(require 'lena)

(defgroup lena-render nil
  "Rendering of Lean's interactive goals."
  :group 'lena
  :prefix "lena-render-")

(defface lena-render-heading
  '((t :inherit bold))
  "Face of section headings such as \"Tactic state\"."
  :group 'lena-render)

(defface lena-render-goals-accomplished
  '((t :inherit success))
  "Face of the line that says that the proof at point is complete."
  :group 'lena-render)

(defface lena-render-trace-class
  '((t :inherit shadow))
  "Face of the class of a trace, such as \"[Meta.synthInstance]\"."
  :group 'lena-render)

(defface lena-render-suggestion
  '((t :inherit button))
  "Face of a suggestion that can be applied, such as \"[apply]\"."
  :group 'lena-render)

(defface lena-render-suggestion-inserted
  '((t :inherit diff-refine-added))
  "Face of what a suggestion inserts, in its diff."
  :group 'lena-render)

(defface lena-render-suggestion-deleted
  '((t :inherit diff-refine-removed :strike-through t))
  "Face of what a suggestion deletes, in its diff."
  :group 'lena-render)

(defface lena-render-case
  '((t :inherit font-lock-keyword-face))
  "Face of the case name of a goal."
  :group 'lena-render)

(defface lena-render-hyp-name
  '((t :inherit font-lock-variable-name-face))
  "Face of the names of hypotheses."
  :group 'lena-render)

(defface lena-render-inaccessible
  '((t :inherit shadow))
  "Face of inaccessible names, which contain \"✝\"."
  :group 'lena-render)

(defface lena-render-goal-prefix
  '((t :inherit font-lock-builtin-face))
  "Face of the turnstile before the target of a goal."
  :group 'lena-render)

(defcustom lena-render-wrap-indent 4
  "Columns by which a wrapped line of code continues further in.
When a window is too narrow for a line, its continuation is indented
by this much more than the line itself, so that it reads as part of
it.  See `wrap-prefix'."
  :type 'natnum
  :group 'lena-render)

(defface lena-render-diff-inserted
  '((t :inherit diff-added :extend nil))
  "Face of what the tactic at point has added or changed."
  :group 'lena-render)

(defface lena-render-diff-removed
  '((t :inherit diff-removed :extend nil))
  "Face of what the tactic at point will remove or change."
  :group 'lena-render)

;;;; Data:

(defun lena-render-diff-face (status)
  "Return the face for STATUS, the `diffStatus' of a subterm, or nil.
Before a tactic, the server marks what it is about to change; after a
tactic, what it has changed."
  (pcase status
    ((or "wasChanged" "wasInserted" "willInsert") 'lena-render-diff-inserted)
    ((or "willChange" "willDelete" "wasDeleted") 'lena-render-diff-removed)))

(cl-defstruct (lena-render-node (:constructor lena-render--node-create)
                                (:copier nil))
  "A tagged subterm of rendered code."
  info                                  ; RPC reference, for hovers
  diff-status                           ; string or nil
  beg end)                              ; extent in the rendered text

(cl-defstruct (lena-render-result (:constructor lena-render--result-create)
                                  (:copier nil))
  "Rendered text and the nodes in it.
Positions of nodes count from 1 at the start of TEXT, as in a buffer
that contains only TEXT."
  text
  nodes)

;;;; Code:

(defvar lena-render--nodes nil
  "Nodes rendered so far, in reverse order.")

(defvar lena-render--diffs t
  "Non-nil if diffs are rendered; nil under the option `:hide-diffs'.")

(defvar lena-render--faces nil
  "Faces to add after fontification, as a list of (BEG END FACE).
Fontification would remove faces put on the text earlier.")

(defun lena-render--insert (string face)
  "Insert STRING at point and note FACE for it."
  (let ((beg (point)))
    (insert string)
    (push (list beg (point) face) lena-render--faces)))

(defun lena-render--code (tagged)
  "Insert TAGGED, a `CodeWithInfos', at point.
Each tagged subterm becomes a node.  A node is noted after the nodes
inside it, so that `lena-render--nodes' lists outer nodes first."
  (cond
   ((plist-member tagged :text)
    (insert (plist-get tagged :text)))
   ((plist-member tagged :append)
    (seq-doseq (child (plist-get tagged :append))
      (lena-render--code child)))
   ((plist-member tagged :tag)
    (let* ((info (aref (plist-get tagged :tag) 0))
           (node (lena-render--node-create
                  :info (plist-get info :info)
                  :diff-status (plist-get info :diffStatus)
                  :beg (point))))
      (lena-render--code (aref (plist-get tagged :tag) 1))
      (setf (lena-render-node-end node) (point))
      (push node lena-render--nodes)))))

(defun lena-render--fontify (beg end)
  "Fontify the code between BEG and END with the rules of `lena-mode'."
  (set-syntax-table lena-mode-syntax-table)
  (setq-local font-lock-defaults lena-font-lock-defaults)
  (font-lock-ensure beg end))

(defun lena-render--inaccessible-p (name)
  "Return non-nil if NAME is inaccessible."
  (string-search "✝" name))

;;;; Goals:

(defun lena-render--hyp (hyp options)
  "Insert HYP, an `InteractiveHypothesisBundle', as a line.
OPTIONS is a plist of view options; see `lena-render-goals'."
  (let ((names (seq-remove
                (lambda (name)
                  (and (plist-get options :hide-inaccessible)
                       (lena-render--inaccessible-p name)))
                (plist-get hyp :names))))
    (unless (or (null names)
                (and (plist-get options :hide-instances)
                     (eq t (plist-get hyp :isInstance)))
                (and (plist-get options :hide-types)
                     (eq t (plist-get hyp :isType))))
      (let ((beg (point))
            (diff (lena-render--flag-diff-face hyp)))
        (cl-loop for (name . more) on names
                 do (lena-render--insert
                     name (if (lena-render--inaccessible-p name)
                              'lena-render-inaccessible
                            'lena-render-hyp-name))
                 when more do (insert " "))
        (when diff
          (push (list beg (point) diff) lena-render--faces))
        (insert " : ")
        (lena-render--code (plist-get hyp :type))
        (when-let* ((val (plist-get hyp :val))
                    ((not (plist-get options :hide-let-values))))
          (insert " := ")
          (lena-render--code val))
        (insert "\n")))))

(defun lena-render--flag-diff-face (object)
  "Return the diff face for OBJECT, a goal or hypothesis, or nil.
Before a tactic, the server marks what it will remove; after it, what
it has inserted."
  (when lena-render--diffs
    (cond ((eq t (plist-get object :isInserted))
           'lena-render-diff-inserted)
          ((eq t (plist-get object :isRemoved))
           'lena-render-diff-removed))))

(defun lena-render--target (goal)
  "Insert the target of GOAL, with its turnstile, as the last line.
The turnstile shows whether the goal as a whole is removed or inserted."
  (let ((prefix (or (plist-get goal :goalPrefix) "⊢ ")))
    (lena-render--insert prefix (or (lena-render--flag-diff-face goal)
                                    'lena-render-goal-prefix))
    (lena-render--code (plist-get goal :type))
    (insert "\n")))

(defun lena-render--goal (goal heading key options)
  "Insert GOAL, an `InteractiveGoal' or `InteractiveTermGoal'.
If HEADING is non-nil, insert it as a level 2 heading first.  KEY is
stored in the property `lena-key'.  OPTIONS is a plist of view options;
see `lena-render-goals'."
  (let ((beg (point))
        (hyps (plist-get goal :hyps)))
    (when heading
      (lena-render--insert (propertize heading 'outline-level 2)
                           'lena-render-case)
      (when-let* ((diff (lena-render--flag-diff-face goal)))
        (push (list beg (point) diff) lena-render--faces))
      (insert "\n"))
    (let ((code-beg (point)))
      (if (plist-get options :reverse)
          (progn (lena-render--target goal)
                 (seq-doseq (hyp (reverse hyps))
                   (lena-render--hyp hyp options)))
        (seq-doseq (hyp hyps)
          (lena-render--hyp hyp options))
        (lena-render--target goal))
      (lena-render--fontify code-beg (point)))
    (put-text-property beg (point) 'lena-key key)
    (put-text-property beg (point) 'lena-goal (gensym "goal"))))

(defun lena-render--heading (text section)
  "Insert TEXT as the level 1 heading of SECTION, a symbol."
  (lena-render--insert (propertize text 'outline-level 1 'lena-section section)
                       'lena-render-heading)
  (insert "\n"))

(defun lena-render--goals-section (goals options)
  "Insert the section for GOALS, a vector of `InteractiveGoal's.
OPTIONS is a plist of view options; see `lena-render-goals'."
  (let ((count (length goals))
        (names nil))
    (lena-render--heading
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
         (lena-render--goal
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

(defvar lena-render--trace-children nil
  "Hash table of the fetched children of traces, by key, or nil.")

(defvar lena-render--trace-key nil
  "Key of the enclosing message or trace.")

(defvar lena-render--trace-count nil
  "A list whose car counts the traces rendered under the enclosing one.")

(defvar lena-render--trace-depth 0
  "Number of traces that enclose the one being rendered.")

(defvar lena-render--options nil
  "The view options of the current rendering.")

(defun lena-render--severity (severity)
  "Return the name and face of the LSP SEVERITY as a cons."
  (pcase severity
    (2 '("warning" . compilation-warning))
    (3 '("information" . compilation-info))
    (4 '("hint" . compilation-info))
    (_ '("error" . compilation-error))))

(defun lena-render--fontify-code (beg end)
  "Fontify the code between BEG and END on its own.
Narrowed, so that text around it, like quotes in a message, does not
change how it is parsed."
  (save-restriction
    (narrow-to-region beg end)
    (lena-render--fontify beg end)))

(defun lena-render--msg (tagged)
  "Insert TAGGED, a `TaggedText' of message embeds, at point."
  (cond
   ((plist-member tagged :text)
    (insert (plist-get tagged :text)))
   ((plist-member tagged :append)
    (seq-doseq (child (plist-get tagged :append))
      (lena-render--msg child)))
   ((plist-member tagged :tag)
    (lena-render--embed (aref (plist-get tagged :tag) 0)))))

(defun lena-render--embed (embed)
  "Insert EMBED, a `MsgEmbed', at point."
  (cond
   ((plist-member embed :expr)
    (let ((beg (point)))
      (lena-render--code (plist-get embed :expr))
      (lena-render--fontify-code beg (point))))
   ((plist-member embed :goal)
    (let ((goal (plist-get embed :goal)))
      (unless (bolp) (insert "\n"))
      ;; The case as a line, not a heading: it belongs to the message.
      (when-let* ((name (plist-get goal :userName)))
        (lena-render--insert (format "case %s" name) 'lena-render-case)
        (insert "\n"))
      (lena-render--goal goal nil (cons 'message-goal lena-render--trace-key)
                         lena-render--options)))
   ((plist-member embed :trace)
    (lena-render--trace (plist-get embed :trace)))
   ((plist-member embed :widget)
    (lena-render--widget (plist-get embed :widget)))))

(defun lena-render--trace (trace)
  "Insert TRACE, a trace embed, as a heading with its children below.
Children that the server sends on request only are taken from
`lena-render--trace-children' if they were fetched."
  (let* ((key (append lena-render--trace-key
                      (list (prog1 (car lena-render--trace-count)
                              (cl-incf (car lena-render--trace-count))))))
         (children (plist-get trace :children))
         (lazy (plist-get children :lazy))
         (fetched (and lazy lena-render--trace-children
                       (gethash key lena-render--trace-children)))
         (kids (or (plist-get children :strict) fetched))
         (beg (progn (unless (bolp) (insert "\n")) (point))))
    (insert (make-string (* 2 lena-render--trace-depth) ?\s))
    (lena-render--insert (format "[%s] " (plist-get trace :cls))
                         'lena-render-trace-class)
    (lena-render--msg (plist-get trace :msg))
    ;; The heading is the trace's own line; its children follow.
    (add-text-properties
     beg (point)
     (append (list 'outline-level (+ 3 lena-render--trace-depth) 'lena-key key)
             (when (and kids (eq t (plist-get trace :collapsed)))
               (list 'lena-folded t))
             (when (and lazy (not fetched))
               (list 'lena-trace-lazy lazy))))
    (insert "\n")
    (let ((lena-render--trace-key key)
          (lena-render--trace-count (list 0))
          (lena-render--trace-depth (1+ lena-render--trace-depth)))
      (seq-doseq (child kids)
        (lena-render--msg child)
        (unless (bolp) (insert "\n"))))))

(defun lena-render--suggestion (beg props help)
  "Mark the text from BEG to point as a suggestion with PROPS to apply.
PROPS are those of Lean's widget, with the `:range' to replace and the
`:suggestion'.  HELP is shown in its tooltip."
  (put-text-property beg (point) 'lena-suggestion
                     (list :range (plist-get props :range)
                           :suggestion (plist-get props :suggestion)
                           :help help)))

(defun lena-render--widget (widget)
  "Insert WIDGET, a widget embed.
Widgets are programs for VS Code; the few that Lean itself uses in
messages are rendered here, and others show their alternative text:

- `Lean.Meta.Hint.textInsertionWidget', as in \"Try this\" of `exact?'
  and of tactics of other libraries that use Lean's `TryThis', is the
  link \"[apply]\";
- `Lean.Meta.Hint.tryThisDiffWidget', as in Lean's hints, shows the
  change it suggests as a diff;
- `Lean.errorDescriptionWidget' links the code of an error to its
  explanation in Lean's reference manual.

Suggestions carry `lena-suggestion', a plist with the `:range' to
replace, the `:suggestion' and the `:help' to show; links carry
`lena-explanation', the URL."
  (let* ((instance (plist-get widget :wi))
         (props (plist-get instance :props))
         (beg (point)))
    (pcase (plist-get instance :id)
      ("Lean.Meta.Hint.textInsertionWidget"
       (let ((accept (plist-get props :acceptSuggestionProps)))
         (lena-render--insert (or (plist-get accept :linkText) "[apply]")
                              'lena-render-suggestion)
         (lena-render--suggestion beg props (plist-get accept :hoverText))))
      ("Lean.Meta.Hint.tryThisDiffWidget"
       (seq-doseq (part (plist-get props :diff))
         (lena-render--insert (plist-get part :text)
                              (pcase (plist-get part :type)
                                ("insertion" 'lena-render-suggestion-inserted)
                                ("deletion" 'lena-render-suggestion-deleted)
                                (_ 'default))))
       (lena-render--suggestion beg props "Apply suggestion"))
      ("Lean.errorDescriptionWidget"
       (unless (or (bolp) (memql (char-before) '(?\s ?\n)))
         (insert " "))
       (setq beg (point))
       (lena-render--insert (format "[%s]" (plist-get props :code))
                            'lena-render-suggestion)
       (put-text-property beg (point) 'lena-explanation
                          (plist-get props :explanationUrl)))
      (_ (lena-render--msg (plist-get widget :alt))))))

(defun lena-render--whole-file-p (diagnostic line-count)
  "Return non-nil if DIAGNOSTIC is about the whole file of LINE-COUNT lines.
Lean gives messages about the file, such as that its imports are out of
date, its first line as range and the whole file as full range, so that
they are seen anywhere in it.  The same criterion as `lena-margin'."
  (when-let* ((full (plist-get diagnostic :fullRange))
              ((natnump line-count)))
    (let ((start (plist-get full :start)))
      (and (zerop (plist-get start :line))
           (zerop (plist-get start :character))
           (>= (plist-get (plist-get full :end) :line) (1- line-count))))))

(defun lena-render--plain-text (tagged)
  "Return the plain text of TAGGED, a `TaggedText', without its tags.
A tag's text is the code of a subterm, or the fallback text of an embed
in a message."
  (cond ((plist-member tagged :text) (plist-get tagged :text))
        ((plist-member tagged :append)
         (mapconcat #'lena-render--plain-text (plist-get tagged :append) ""))
        ((plist-member tagged :tag)
         (lena-render--plain-text (aref (plist-get tagged :tag) 1)))
        (t "")))

(defun lena-render--imports-out-of-date-p (diagnostic)
  "Return non-nil if DIAGNOSTIC says that the imports are out of date.
Lean says so in two ways: when the file is opened, and when a file that
it imports changes while it is open.  The text is how VS Code recognizes
the first."
  (string-prefix-p "Imports are out of date"
                   (lena-render--plain-text (plist-get diagnostic :message))))

(defun lena-render--message (diagnostic line whole-file occurrence)
  "Insert DIAGNOSTIC, an `InteractiveDiagnostic', as a heading and text.
LINE is the 0-based line at which the messages were requested; a
message that starts on another line is folded at first, unless
WHOLE-FILE says that it is about the whole file.  OCCURRENCE counts the
messages before it with the same start and severity, which its key
would not tell apart otherwise.  A message that the imports are out of
date gets a button to restart the file."
  (let* ((range (or (plist-get diagnostic :fullRange)
                    (plist-get diagnostic :range)))
         (start (plist-get range :start))
         (severity (lena-render--severity (plist-get diagnostic :severity)))
         (key (list 'message (plist-get start :line) (plist-get start :character)
                    (car severity) occurrence))
         (beg (point)))
    (insert (propertize (format "%d:%d" (1+ (plist-get start :line))
                                (plist-get start :character))
                        'lena-message-range range)
            " ")
    (lena-render--insert (car severity) (cdr severity))
    (add-text-properties beg (point)
                         (append (list 'outline-level 2 'lena-key key)
                                 (unless (or whole-file
                                             (eql (plist-get start :line) line))
                                   (list 'lena-folded t))))
    (insert "\n")
    (let ((lena-render--trace-key key)
          (lena-render--trace-count (list 0))
          (lena-render--trace-depth 0))
      (lena-render--msg (plist-get diagnostic :message)))
    (unless (bolp) (insert "\n"))
    (when (and whole-file (lena-render--imports-out-of-date-p diagnostic))
      (let ((beg (point)))
        (lena-render--insert "[restart file]" 'lena-render-suggestion)
        (put-text-property beg (point) 'lena-restart-file t)
        (insert "\n")))))

(defun lena-render--messages-section (messages)
  "Insert the section for MESSAGES; see `lena-render-goals'.
That a proof is complete is shown above the goals, so its message is
left out.  Messages about the whole file come first."
  (let* ((line-count (plist-get messages :line-count))
         (diagnostics (seq-remove
                       (lambda (diagnostic)
                         (seq-contains-p (plist-get diagnostic :leanTags) 2))
                       (plist-get messages :diagnostics)))
         (whole-file (seq-filter (lambda (diagnostic)
                                   (lena-render--whole-file-p diagnostic
                                                              line-count))
                                 diagnostics))
         (lena-render--trace-children (plist-get messages :trace-children)))
    (when diagnostics
      (unless (bobp) (insert "\n"))
      (lena-render--heading (format "Messages (%d)" (length diagnostics))
                            'messages)
      (let ((seen (make-hash-table :test #'equal)))
        (dolist (diagnostic (append whole-file
                                    (seq-difference diagnostics whole-file
                                                    #'eq)))
          (let ((start (plist-get (or (plist-get diagnostic :fullRange)
                                      (plist-get diagnostic :range))
                                  :start)))
            (lena-render--message
             diagnostic (plist-get messages :line)
             (memq diagnostic whole-file)
             (cl-incf (gethash (list start (plist-get diagnostic :severity))
                               seen -1)))))))))

(defun lena-render--apply-faces ()
  "Add the noted faces, then the diff faces of the rendered nodes."
  (pcase-dolist (`(,beg ,end ,face) (reverse lena-render--faces))
    (add-face-text-property beg end face))
  (dolist (node (and lena-render--diffs lena-render--nodes))
    (when-let* ((face (lena-render-diff-face
                       (lena-render-node-diff-status node))))
      (add-face-text-property (lena-render-node-beg node)
                              (lena-render-node-end node)
                              face))))

(defun lena-render--put-nodes ()
  "Store each rendered node in `lena-node' over its extent.
`lena-render--nodes' lists outer nodes first, so inner ones win."
  (dolist (node lena-render--nodes)
    (put-text-property (lena-render-node-beg node)
                       (lena-render-node-end node)
                       'lena-node node)))

(defun lena-render-goals (goals term-goal &optional options messages)
  "Render GOALS, TERM-GOAL and MESSAGES as a `lena-render-result'.

GOALS is the result of `Lean.Widget.getInteractiveGoals' and TERM-GOAL
that of `Lean.Widget.getInteractiveTermGoal'; either may be nil.  A
section is rendered for each of them that is non-nil.

MESSAGES, if non-nil, is a plist: `:diagnostics', the result of
`Lean.Widget.getInteractiveDiagnostics'; `:line', the 0-based line at
which they were requested; `:line-count', the number of lines of the
file, to tell messages about the whole file; and `:trace-children', a
hash table of the children of traces that were fetched, by the key of
the trace.

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
  (let ((lena-render--diffs (not (plist-get options :hide-diffs)))
        (lena-render--options options)
        (lena-render--nodes nil)
        (lena-render--faces nil))
    (with-temp-buffer
      ;; As in lean.nvim, a line above the goals, which are not folded
      ;; with it; it stands for no goals at all.
      (when-let* ((accomplished (plist-get options :accomplished)))
        (lena-render--insert accomplished 'lena-render-goals-accomplished)
        (insert "\n"))
      (when (and goals
                 (not (and (plist-get options :accomplished)
                           (seq-empty-p (plist-get goals :goals)))))
        (lena-render--goals-section (plist-get goals :goals) options))
      (when term-goal
        (unless (bobp) (insert "\n"))
        (lena-render--heading "Expected type" 'term-goal)
        (lena-render--goal term-goal nil 'term-goal options))
      (when messages
        (lena-render--messages-section messages))
      (lena-render--finish))))

(defun lena-render--put-wrap-prefixes ()
  "Indent the continuations of wrapped lines in the current buffer.
See `lena-render-wrap-indent'."
  (goto-char (point-min))
  (while (not (eobp))
    (let ((beg (point)))
      (skip-chars-forward " ")
      (put-text-property beg (min (1+ (line-end-position)) (point-max))
                         'wrap-prefix
                         (make-string (+ (- (point) beg)
                                         lena-render-wrap-indent)
                                      ?\s))
      (forward-line))))

(defun lena-render--finish ()
  "Return the rendered contents of the current buffer as a result."
  (lena-render--apply-faces)
  (lena-render--put-nodes)
  (lena-render--put-wrap-prefixes)
  (lena-render--result-create
   :text (buffer-string)
   :nodes (reverse lena-render--nodes)))

(defun lena-render-info (info)
  "Render INFO, a result of `infoToInteractive', as a `lena-render-result'.
The text is the expression with all its arguments made explicit, then
its type, as in \"EXPR : TYPE\"; either may be missing.  The
documentation in INFO is not rendered."
  (let ((lena-render--diffs nil)
        (lena-render--nodes nil)
        (lena-render--faces nil)
        (expr (plist-get info :exprExplicit))
        (type (plist-get info :type)))
    (with-temp-buffer
      (when expr (lena-render--code expr))
      (when (and expr type) (insert " : "))
      (when type (lena-render--code type))
      (lena-render--fontify (point-min) (point-max))
      (lena-render--finish))))

(defun lena-render-node-at (pos &optional object)
  "Return the innermost node at POS in OBJECT, a buffer or string."
  (get-text-property pos 'lena-node object))

(provide 'lena-render)

;;; lena-render.el ends here
