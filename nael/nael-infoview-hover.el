;;; nael-infoview-hover.el --- Information on subterms in Nael's infoview  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see LICENSE.GPL3.

;;; Commentary:

;; Every subterm that the infoview shows carries a reference to what the
;; server knows about it (see `nael-render').  This file asks the server
;; about the subterm at point, as an editor asks about the symbol at
;; point in source code:
;;
;; - ElDoc shows the subterm with all its arguments made explicit and its
;;   type in the echo area, and its documentation in `eldoc-doc-buffer'.
;;   Packages that show ElDoc elsewhere, such as in a child frame, show
;;   it there.
;;
;; The functions here work in any buffer that shows rendered nodes and
;; whose `nael-infoview--handle' is the RPC handle through which their
;; references were obtained.

;;; Code:

(require 'eldoc)

(require 'nael-eglot-compat)
(require 'nael-render)
(require 'nael-rpc)

(defvar-local nael-infoview--handle nil
  "RPC handle through which the references shown in this buffer came.
References belong to an RPC session, so calls about them must use it.")

(defvar-local nael-infoview--eldoc-cache nil
  "The node that ElDoc documented last and its documentation.
The format is (NODE DOC . ECHO), see `nael-infoview-eldoc-function'.")

;;;; Content:

(defun nael-infoview--diff-note (status)
  "Return a sentence explaining STATUS, the diff status of a subterm."
  (pcase status
    ("wasChanged" "The tactic at point changed this subterm.")
    ("willChange" "The tactic at point will change this subterm.")
    ("wasInserted" "The tactic at point inserted this subterm.")
    ("willInsert" "The tactic at point will insert this subterm.")
    ("wasDeleted" "The tactic at point deleted this subterm.")
    ("willDelete" "The tactic at point will delete this subterm.")))

(defun nael-infoview--docstring (info)
  "Return the documentation in INFO, formatted, or nil.
Markdown is shown as Eglot shows it, with `markdown-mode' if installed."
  (let ((doc (plist-get info :doc)))
    (when (and (stringp doc) (not (string-blank-p doc)))
      (string-trim (nael-eglot-compat-format-markup doc)))))

(defun nael-infoview--without-nodes (string)
  "Return a copy of STRING without the nodes in it."
  (let ((string (copy-sequence string)))
    (remove-list-of-text-properties 0 (length string) '(nael-node) string)
    string))

(defun nael-infoview--eldoc-strings (info status)
  "Return the documentation of INFO for ElDoc, as (DOC . ECHO).
ECHO is the subterm and its type; DOC adds the documentation and an
explanation of STATUS, the diff status of the subterm."
  (let ((echo (nael-infoview--without-nodes
               (nael-render-result-text (nael-render-info info)))))
    (cons (string-join (delq nil (list echo
                                       (nael-infoview--docstring info)
                                       (nael-infoview--diff-note status)))
                       "\n\n")
          echo)))

;;;; ElDoc:

(defun nael-infoview-eldoc-function (callback &rest _)
  "Report the subterm at point to ElDoc with CALLBACK.
The echo area shows the subterm with all its arguments made explicit,
and its type.  The documentation buffer of ElDoc also shows the
documentation of the subterm and how the tactic at point changes it."
  (when-let* ((node (nael-render-node-at (point)))
              (handle nael-infoview--handle)
              ((nael-rpc-handle-live-p handle)))
    (if (eq node (car nael-infoview--eldoc-cache))
        (pcase-let ((`(,_ ,doc . ,echo) nael-infoview--eldoc-cache))
          (funcall callback doc :echo echo))
      (let ((buffer (current-buffer))
            (owner (nael-rpc-make-owner)))
        (nael-rpc-info-to-interactive
         handle (nael-render-node-info node)
         (lambda (info)
           (when info
             (pcase-let ((`(,doc . ,echo)
                          (nael-infoview--eldoc-strings
                           info (nael-render-node-diff-status node))))
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq nael-infoview--eldoc-cache
                         (cons node (cons doc echo)))))
               (funcall callback doc :echo echo)))
           ;; The strings refer to nothing on the server.
           (nael-rpc-owner-release owner))
         :owner owner :slot 'infoview-eldoc)))
    t))

;;;; Setup:

(defun nael-infoview-hover-setup ()
  "Make the subterms shown in the current buffer report on themselves."
  (add-hook 'eldoc-documentation-functions #'nael-infoview-eldoc-function
            nil t)
  (when (bound-and-true-p global-eldoc-mode)
    (eldoc-mode)))

(defun nael-infoview-hover-reset (handle)
  "Forget what was found out about the nodes shown so far.
HANDLE is the RPC handle of the references shown from now on."
  (setq nael-infoview--handle handle
        nael-infoview--eldoc-cache nil))

(provide 'nael-infoview-hover)

;;; nael-infoview-hover.el ends here
