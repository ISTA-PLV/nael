;;; nael-test-helpers.el --- Helpers for Nael's tests  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see nael/LICENSE.GPL3.

;;; Commentary:

;; Fixtures, data made by hand, and setups shared by several test files.

;;; Code:

(require 'ert)
(require 'nael-infoview)
(require 'nael-margin)

;;;; Fixtures:

(defconst nael-test-fixtures
  (expand-file-name "fixtures/"
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(defun nael-test-read-fixture (name)
  "Parse the JSON fixture NAME as jsonrpc.el would."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name nael-test-fixtures))
    (json-parse-buffer :object-type 'plist :array-type 'array
                       :null-object nil :false-object :json-false)))

(defun nael-test-spinlock-goals ()
  "Return the recorded goals of iris-lean's SpinLock."
  (nael-test-read-fixture "goals-iris-spinlock.json"))

;;;; Diagnostics:

(defun nael-test-range (start end)
  "Return an LSP range from START to END, each a cons (LINE . CHARACTER)."
  (list :start (list :line (car start) :character (cdr start))
        :end (list :line (car end) :character (cdr end))))

(cl-defun nael-test-diagnostic
    (severity start end &key tags silent (message "Message"))
  "Return a diagnostic of SEVERITY from START to END as the server sends it.
START and END are conses (LINE . CHARACTER).  TAGS are the numbers of
its Lean tags, SILENT says whether it is silent, and MESSAGE is its
message."
  (append (list :range (nael-test-range
                        start (if (> (car end) (car start))
                                  (cons (1+ (car start)) 0)
                                end))
                :fullRange (nael-test-range start end)
                :severity severity
                :message message)
          (and tags (list :leanTags (vconcat tags)))
          (and silent (list :isSilent t))))

;;;; Goals:

(defun nael-test-code (&rest parts)
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

(defun nael-test-goal (&rest keys)
  "Return a goal with target `n = 0', merged with KEYS."
  (append keys
          (list :hyps []
                :type (nael-test-code '("n" "1") " = 0")
                :goalPrefix "⊢ "
                :mvarId "_uniq.1")))

(defun nael-test-faces (string pos)
  "Return the faces of STRING at 0-based POS as a list."
  (ensure-list (get-text-property pos 'face string)))

;;;; The infoview:

(defmacro nael-test-with-options (&rest body)
  "Run BODY with the default view options of the infoview."
  (declare (indent 0) (debug t))
  `(let ((nael-infoview-hide-instances nil)
         (nael-infoview-hide-types nil)
         (nael-infoview-hide-inaccessible nil)
         (nael-infoview-hide-let-values nil)
         (nael-infoview-reverse nil)
         (nael-infoview-show-diffs t)
         (nael-infoview-expected-type 'expanded))
     ,@body))

(defmacro nael-test-with-goals (&rest body)
  "Run BODY in the infoview showing the recorded goals of SpinLock."
  (declare (indent 0) (debug t))
  `(nael-test-with-options
     (unwind-protect
         (progn
           (nael-infoview--show (nael-test-spinlock-goals) nil
                                (nael-rpc-make-owner) "SpinLock.lean:110:2")
           (with-current-buffer (nael-infoview--buffer)
             (switch-to-buffer (current-buffer))
             (goto-char (point-min))
             ,@body))
       (when-let* ((buffer (nael-infoview--buffer)))
         (kill-buffer buffer)))))

(defun nael-test-goto (string)
  "Move point to the start of the first occurrence of STRING."
  (goto-char (point-min))
  (search-forward string)
  (goto-char (match-beginning 0)))

(defmacro nael-test-with-windows (&rest body)
  "Run BODY with the infoview displayed in its side window, selected.
The main window shows the buffer Main."
  (declare (indent 0) (debug t))
  `(nael-test-with-goals
     (let ((main (get-buffer-create "Main")))
       (unwind-protect
           (progn
             (dolist (window (window-list))
               (when (window-parameter window 'window-side)
                 (delete-window window)))
             (delete-other-windows)
             (switch-to-buffer main)
             (setq nael-infoview--window
                   (display-buffer (nael-infoview--buffer)
                          nael-infoview-display-action))
             (select-window nael-infoview--window)
             ,@body)
         (dolist (window (window-list))
           (when (window-parameter window 'window-side)
             (delete-window window)))
         (setq nael-infoview--window nil)
         (kill-buffer main)))))

(defun nael-test-visible-p (string)
  "Return non-nil if STRING is shown, not folded away, in the buffer."
  (save-excursion
    (goto-char (point-min))
    (and (search-forward string nil t)
         (not (invisible-p (match-beginning 0))))))

;;;; The margin:

(defmacro nael-test-with-margin-defaults (&rest body)
  "Run BODY with the default signs, all of which can be displayed."
  (declare (indent 0) (debug t))
  `(let ((nael-margin-goals-accomplished "✓")
         (nael-margin-unsolved-goals "✗")
         (nael-margin-admitted "⚠")
         (nael-margin-bar '("┌" "│" "└"))
         (nael-margin-processing "│")
         (flymake-margin-indicators-string
          '((error "‼" compilation-error)
            (warning "!" compilation-warning)
            (note "!" compilation-info))))
     (cl-letf (((symbol-function 'char-displayable-p) #'always))
       ,@body)))

(defun nael-test-margin-drawn ()
  "Return the drawn signs as (LINE STRING) in order, LINE 0-based."
  (sort (mapcar (lambda (overlay)
                  (let ((spec (get-text-property
                               0 'display (overlay-get overlay 'before-string))))
                    (should (equal (car spec) '(margin left-margin)))
                    (list (1- (line-number-at-pos (overlay-start overlay)))
                          (substring-no-properties (cadr spec)))))
                nael-margin--overlays)
        #'car-less-than-car))

(defun nael-test-empty-display-p (display)
  "Return non-nil if DISPLAY shows nothing, in the margin or the fringe.
Flymake 1.4 chooses between both with `when' specifications."
  (if (eq (car-safe (car-safe display)) 'when)
      (seq-every-p (lambda (spec)
                     (and (eq (car-safe spec) 'when)
                          (nael-test-empty-display-p (cddr spec))))
                   display)
    (member display '(((margin left-margin) "")
                      (left-fringe nael-margin-empty)
                      (right-fringe nael-margin-empty)))))

(provide 'nael-test-helpers)

;;; nael-test-helpers.el ends here
