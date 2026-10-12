;;; lena-test-helpers.el --- Helpers for Lena's tests  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see lena/LICENSE.GPL3.

;;; Commentary:

;; Fixtures, data made by hand, and setups shared by several test files.

;;; Code:

(require 'ert)
(require 'lena-infoview)
(require 'lena-margin)

;; Flymake of Emacs 31 runs its backends only in trusted buffers, and
;; the tests' buffers are their own.
(setq trusted-content :all)

;;;; Fixtures:

(defconst lena-test-fixtures
  (expand-file-name "fixtures/"
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(defun lena-test-read-fixture (name)
  "Parse the JSON fixture NAME as jsonrpc.el would."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name lena-test-fixtures))
    (json-parse-buffer :object-type 'plist :array-type 'array
                       :null-object nil :false-object :json-false)))

(defun lena-test-spinlock-goals ()
  "Return the recorded goals of iris-lean's SpinLock."
  (lena-test-read-fixture "goals-iris-spinlock.json"))

;;;; Diagnostics:

(defun lena-test-range (start end)
  "Return an LSP range from START to END, each a cons (LINE . CHARACTER)."
  (list :start (list :line (car start) :character (cdr start))
        :end (list :line (car end) :character (cdr end))))

(cl-defun lena-test-diagnostic
    (severity start end &key tags silent (message "Message"))
  "Return a diagnostic of SEVERITY from START to END as the server sends it.
START and END are conses (LINE . CHARACTER).  TAGS are the numbers of
its Lean tags, SILENT says whether it is silent, and MESSAGE is its
message."
  (append (list :range (lena-test-range
                        start (if (> (car end) (car start))
                                  (cons (1+ (car start)) 0)
                                end))
                :fullRange (lena-test-range start end)
                :severity severity
                :message message)
          (and tags (list :leanTags (vconcat tags)))
          (and silent (list :isSilent t))))

;;;; Goals:

(defun lena-test-code (&rest parts)
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

(defun lena-test-goal (&rest keys)
  "Return a goal with target `n = 0', merged with KEYS."
  (append keys
          (list :hyps []
                :type (lena-test-code '("n" "1") " = 0")
                :goalPrefix "⊢ "
                :mvarId "_uniq.1")))

(defun lena-test-faces (string pos)
  "Return the faces of STRING at 0-based POS as a list."
  (ensure-list (get-text-property pos 'face string)))

;;;; The infoview:

(defmacro lena-test-with-options (&rest body)
  "Run BODY with the default view options of the infoview."
  (declare (indent 0) (debug t))
  `(let ((lena-infoview-hide-instances nil)
         (lena-infoview-hide-types nil)
         (lena-infoview-hide-inaccessible nil)
         (lena-infoview-hide-let-values nil)
         (lena-infoview-reverse nil)
         (lena-infoview-show-diffs t)
         (lena-infoview-expected-type 'expanded))
     ,@body))

(defmacro lena-test-with-goals (&rest body)
  "Run BODY in the infoview showing the recorded goals of SpinLock."
  (declare (indent 0) (debug t))
  `(lena-test-with-options
     (unwind-protect
         (progn
           (lena-infoview--show (lena-test-spinlock-goals) nil
                                (lena-rpc-make-owner) "SpinLock.lean:110:2")
           (with-current-buffer (lena-infoview--buffer)
             (switch-to-buffer (current-buffer))
             (goto-char (point-min))
             ,@body))
       (when-let* ((buffer (lena-infoview--buffer)))
         (kill-buffer buffer)))))

(defun lena-test-goto (string)
  "Move point to the start of the first occurrence of STRING."
  (goto-char (point-min))
  (search-forward string)
  (goto-char (match-beginning 0)))

(defmacro lena-test-with-windows (&rest body)
  "Run BODY with the infoview displayed in its side window, selected.
The main window shows the buffer Main."
  (declare (indent 0) (debug t))
  `(lena-test-with-goals
     (let ((main (get-buffer-create "Main")))
       (unwind-protect
           (progn
             (dolist (window (window-list))
               (when (window-parameter window 'window-side)
                 (delete-window window)))
             (delete-other-windows)
             (switch-to-buffer main)
             (setq lena-infoview--window
                   (display-buffer (lena-infoview--buffer)
                          lena-infoview-display-action))
             (select-window lena-infoview--window)
             ,@body)
         (dolist (window (window-list))
           (when (window-parameter window 'window-side)
             (delete-window window)))
         (setq lena-infoview--window nil)
         (kill-buffer main)))))

(defun lena-test-visible-p (string)
  "Return non-nil if STRING is shown, not folded away, in the buffer."
  (save-excursion
    (goto-char (point-min))
    (and (search-forward string nil t)
         (not (invisible-p (match-beginning 0))))))

;;;; The margin:

(defmacro lena-test-with-margin-defaults (&rest body)
  "Run BODY with the default signs, all of which can be displayed."
  (declare (indent 0) (debug t))
  `(let ((lena-margin-goals-accomplished "✓")
         (lena-margin-unsolved-goals "✗")
         (lena-margin-admitted "⚠")
         (lena-margin-bar '("┌" "│" "└"))
         (lena-margin-processing "│")
         (flymake-margin-indicators-string
          '((error "‼" compilation-error)
            (warning "!" compilation-warning)
            (note "!" compilation-info))))
     (cl-letf (((symbol-function 'char-displayable-p) #'always))
       ,@body)))

(defun lena-test-margin-drawn ()
  "Return the drawn signs as (LINE STRING) in order, LINE 0-based."
  (sort (mapcar (lambda (overlay)
                  (let ((spec (get-text-property
                               0 'display (overlay-get overlay 'before-string))))
                    (should (equal (car spec) '(margin left-margin)))
                    (list (1- (line-number-at-pos (overlay-start overlay)))
                          (substring-no-properties (cadr spec)))))
                lena-margin--overlays)
        #'car-less-than-car))

(defun lena-test-empty-display-p (display)
  "Return non-nil if DISPLAY shows nothing, in the margin or the fringe.
Flymake 1.4 chooses between both with `when' specifications."
  (if (eq (car-safe (car-safe display)) 'when)
      (seq-every-p (lambda (spec)
                     (and (eq (car-safe spec) 'when)
                          (lena-test-empty-display-p (cddr spec))))
                   display)
    (member display '(((margin left-margin) "")
                      (left-fringe lena-margin-empty)
                      (right-fringe lena-margin-empty)))))

(provide 'lena-test-helpers)

;;; lena-test-helpers.el ends here
