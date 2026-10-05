;;; nael-eglot-tests.el --- Tests for nael-eglot  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see nael/LICENSE.GPL3.

;;; Commentary:

;; Tests for the handling of diagnostics in `nael-eglot' that need no
;; Lean server.  Diagnostics are made by hand the way the server makes
;; them: the range of one that spans several lines ends at the start of
;; its second line, and its full range is sent as `:fullRange'.

;;; Code:

(require 'ert)
(require 'nael-eglot)

(defun nael-eglot-test-range (start end)
  "Return an LSP range from START to END, each a cons (LINE . CHARACTER)."
  (list :start (list :line (car start) :character (cdr start))
        :end (list :line (car end) :character (cdr end))))

(cl-defun nael-eglot-test-diagnostic
    (severity start end &key tags silent (message "Message"))
  "Return a diagnostic of SEVERITY from START to END as the server sends it.
START and END are conses (LINE . CHARACTER).  TAGS are the numbers of
its Lean tags, SILENT says whether it is silent, and MESSAGE is its
message."
  (append (list :range (nael-eglot-test-range
                        start (if (> (car end) (car start))
                                  (cons (1+ (car start)) 0)
                                end))
                :fullRange (nael-eglot-test-range start end)
                :severity severity
                :message message)
          (and tags (list :leanTags (vconcat tags)))
          (and silent (list :isSilent t))))

(ert-deftest nael-eglot-test-diagnostic-lines ()
  "Lines of a diagnostic, by its range or its full range."
  (let ((diagnostic (nael-eglot-test-diagnostic 1 '(2 . 4) '(5 . 3))))
    (should (equal (nael-eglot-diagnostic-lines diagnostic) '(2 . 2)))
    (should (equal (nael-eglot-diagnostic-lines diagnostic t) '(2 . 5))))
  ;; A range that ends at the start of a line ends on the line before.
  (should (equal (nael-eglot-diagnostic-lines
                  (nael-eglot-test-diagnostic 1 '(2 . 4) '(5 . 0))
                  t)
                 '(2 . 4)))
  ;; Without a full range, the range counts.
  (should (equal (nael-eglot-diagnostic-lines
                  (list :range (nael-eglot-test-range '(3 . 0) '(3 . 5)))
                  t)
                 '(3 . 3))))

(ert-deftest nael-eglot-test-goals-accomplished-p ()
  "Lines inside a complete proof, by the full range of its diagnostic."
  (with-temp-buffer
    (setq nael-eglot-diagnostics
          (vector (nael-eglot-test-diagnostic 3 '(2 . 0) '(6 . 0)
                                               :tags '(2) :silent t)
                  (nael-eglot-test-diagnostic 1 '(8 . 0) '(9 . 4)
                                               :tags '(1))))
    (should-not (nael-eglot-goals-accomplished-p 1))
    (should (nael-eglot-goals-accomplished-p 2))
    (should (nael-eglot-goals-accomplished-p 5))
    (should-not (nael-eglot-goals-accomplished-p 6))
    (should-not (nael-eglot-goals-accomplished-p 8))))

(defclass nael-eglot-test-server (nael-eglot-server) ()
  "Server whose own handling of diagnostics only records them.")

(defvar nael-eglot-test-handled nil
  "Diagnostics that reached the handler of `nael-eglot-test-server'.")

(cl-defmethod eglot-handle-notification
  ((_server nael-eglot-test-server)
   (_method (eql textDocument/publishDiagnostics))
   &key diagnostics &allow-other-keys)
  "Record DIAGNOSTICS instead of handing them to Flymake."
  (setq nael-eglot-test-handled diagnostics))

(cl-defmethod eglot-client-capabilities ((_server nael-eglot-test-server))
  "Return capabilities without asking Eglot, which needs a project."
  (list :workspace nil))

(ert-deftest nael-eglot-test-silent-diagnostics ()
  "Eglot does not see silent diagnostics; Nael keeps all."
  (let* ((file (make-temp-file "nael-eglot-test" nil ".lean"))
         (buffer (find-file-noselect file))
         (server (make-instance 'nael-eglot-test-server
                                :process (make-pipe-process
                                          :name "nael-eglot-test"
                                          :noquery t)))
         (silent (nael-eglot-test-diagnostic 3 '(0 . 0) '(1 . 4)
                                              :tags '(2) :silent t))
         (error (nael-eglot-test-diagnostic 1 '(3 . 0) '(3 . 4)))
         (hooked nil)
         (nael-eglot-test-handled nil))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (add-hook 'nael-eglot-diagnostics-functions
                      (lambda (diagnostics) (setq hooked diagnostics))
                      nil t))
          (eglot-handle-notification server 'textDocument/publishDiagnostics
                                     :uri (eglot-path-to-uri file)
                                     :diagnostics (vector silent error))
          (should (equal nael-eglot-test-handled (vector error)))
          (should (equal hooked (vector silent error)))
          (should (equal (buffer-local-value 'nael-eglot-diagnostics buffer)
                         (vector silent error))))
      (delete-process (jsonrpc--process server))
      (kill-buffer buffer)
      (delete-file file))))

(ert-deftest nael-eglot-test-capabilities ()
  "The client asks for silent diagnostics and the RPC wire format."
  (let ((server (make-instance 'nael-eglot-test-server
                               :process (make-pipe-process
                                         :name "nael-eglot-test"
                                         :noquery t))))
    (unwind-protect
        (let ((lean (plist-get (eglot-client-capabilities server) :lean)))
          (should (eq (plist-get lean :silentDiagnosticSupport) t))
          (should (equal (plist-get lean :rpcWireFormat) "v1")))
      (delete-process (jsonrpc--process server)))))

(provide 'nael-eglot-tests)

;;; nael-eglot-tests.el ends here
