;;; nael-rpc-tests.el --- Tests for nael-rpc  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see nael/LICENSE.GPL3.

;;; Commentary:

;; Unit tests for `nael-rpc' against a fake connection: no Lean server
;; is needed.  `nael-rpc-test-with-fake' replaces the jsonrpc functions
;; that `nael-rpc' calls, records what would have been sent, and lets a
;; test answer requests in any order.

;;; Code:

(require 'ert)
(require 'nael-rpc)

(defconst nael-rpc-test-fixtures
  (expand-file-name "fixtures/"
                    (file-name-directory
                     (or load-file-name buffer-file-name))))

(defun nael-rpc-test-read-fixture (name)
  "Parse the JSON fixture NAME as jsonrpc.el would."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name nael-rpc-test-fixtures))
    (json-parse-buffer :object-type 'plist :array-type 'array
                       :null-object nil :false-object :json-false)))

(defvar nael-rpc-test-requests nil
  "Requests sent to the fake connection, newest first.
Each is a plist with `:id', `:method', `:params', `:success-fn',
`:error-fn' and `:timeout'.")

(defvar nael-rpc-test-notifications nil
  "Notifications sent to the fake connection, newest first.")

(defvar nael-rpc-test-next-id 0)

(defmacro nael-rpc-test-with-fake (&rest body)
  "Run BODY with a fake Lean server that speaks wire format v1."
  (declare (indent 0) (debug t))
  `(let ((nael-rpc-test-requests nil)
         (nael-rpc-test-notifications nil)
         (nael-rpc-test-next-id 0)
         (nael-rpc--sessions (make-hash-table :test #'equal)))
     (cl-letf (((symbol-function 'jsonrpc-async-request)
                (lambda (_connection method params &rest keys)
                  (let ((id (cl-incf nael-rpc-test-next-id)))
                    (push (list :id id :method method :params params
                                :success-fn (plist-get keys :success-fn)
                                :error-fn (plist-get keys :error-fn)
                                :timeout (plist-get keys :timeout))
                          nael-rpc-test-requests)
                    (list id nil))))
               ((symbol-function 'jsonrpc-notify)
                (lambda (_connection method params)
                  (push (cons method params) nael-rpc-test-notifications)))
               ((symbol-function 'jsonrpc-running-p) #'always)
               ((symbol-function 'nael-eglot-compat-capabilities)
                (lambda (_connection)
                  '(:experimental (:rpcProvider (:rpcWireFormat "v1"))))))
       (unwind-protect (progn ,@body)
         (nael-rpc-close-all)))))

(defun nael-rpc-test-find (method &optional n)
  "Return the Nth oldest request with METHOD, a keyword or RPC method name."
  (nth (or n 0)
       (seq-filter (lambda (request)
                     (or (eq (plist-get request :method) method)
                         (equal (plist-get (plist-get request :params) :method)
                                method)))
                   (reverse nael-rpc-test-requests))))

(defun nael-rpc-test-answer (request result)
  "Answer REQUEST with RESULT."
  (funcall (plist-get request :success-fn) result))

(defun nael-rpc-test-fail (request code)
  "Answer REQUEST with an error of CODE."
  (funcall (plist-get request :error-fn)
           (list :code code :message "fake error")))

(defun nael-rpc-test-handle (buffer)
  "Return a handle for BUFFER on the fake connection, not yet connected."
  (nael-rpc--handle-create
   :session (nael-rpc--session-for 'fake "file:///A.lean")
   :position '(:textDocument (:uri "file:///A.lean")
               :position (:line 3 :character 2))
   :buffer buffer))

(defun nael-rpc-test-connect ()
  "Answer the oldest pending `$/lean/rpc/connect' with a session id."
  (nael-rpc-test-answer (nael-rpc-test-find :$/lean/rpc/connect)
                        '(:sessionId "4711")))

(defun nael-rpc-test-released ()
  "Return all references released so far, oldest release first."
  (seq-mapcat (lambda (notification)
                (when (eq (car notification) :$/lean/rpc/release)
                  (append (plist-get (cdr notification) :refs) nil)))
              (reverse nael-rpc-test-notifications)))

;;;; References:

(ert-deftest nael-rpc-test-ref-p ()
  (should (nael-rpc-ref-p '(:__rpcref "1") :__rpcref))
  (should-not (nael-rpc-ref-p '(:__rpcref 1) :__rpcref))
  (should-not (nael-rpc-ref-p '(:__rpcref "1" :x 2) :__rpcref))
  (should-not (nael-rpc-ref-p '(:p "1") :__rpcref))
  (should (nael-rpc-ref-p '(:p "1") :p)))

(ert-deftest nael-rpc-test-collect-refs-fixture ()
  "Every reference in a recorded goal result is found: hyps, ctx, target."
  (let ((refs (nael-rpc-collect-refs
               (nael-rpc-test-read-fixture "goals-iris-spinlock.json")
               :__rpcref)))
    (should (= (length refs) 66))
    (should (= (length (seq-uniq refs)) 66))))

(ert-deftest nael-rpc-test-collect-refs-repeated ()
  "A reference that occurs twice is collected twice."
  (should (equal (nael-rpc-collect-refs
                  [(:__rpcref "1") (:a (:__rpcref "1") :b "x") nil 3]
                  :__rpcref)
                 '((:__rpcref "1") (:__rpcref "1")))))

;;;; Sessions and calls:

(ert-deftest nael-rpc-test-queued-until-connected ()
  "Calls made during the handshake are sent once it settles."
  (nael-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (nael-rpc-test-handle (current-buffer)))
            results)
        (nael-rpc-get-interactive-goals handle (lambda (r) (push r results)))
        (nael-rpc-get-interactive-goals handle (lambda (r) (push r results)))
        (should-not (nael-rpc-test-find :$/lean/rpc/call))
        (nael-rpc-test-connect)
        (let ((first (nael-rpc-test-find :$/lean/rpc/call 0))
              (second (nael-rpc-test-find :$/lean/rpc/call 1)))
          (should (equal (plist-get (plist-get first :params) :sessionId)
                         "4711"))
          (should (equal (plist-get (plist-get first :params) :method)
                         "Lean.Widget.getInteractiveGoals"))
          ;; Goal calls wait for elaboration: no timeout.
          (should-not (plist-get first :timeout))
          (nael-rpc-test-answer first '(:goals []))
          (nael-rpc-test-answer second '(:goals [])))
        (should (= (length results) 2))))))

(ert-deftest nael-rpc-test-retry-on-dead-session ()
  "A call failing with -32900 is retried once on a new session."
  (nael-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (nael-rpc-test-handle (current-buffer)))
            result failure)
        (nael-rpc-get-interactive-goals
         handle (lambda (r) (setq result r))
         :failure (lambda (e) (setq failure e)))
        (nael-rpc-test-connect)
        (nael-rpc-test-fail (nael-rpc-test-find :$/lean/rpc/call)
                            nael-rpc-needs-reconnect)
        (should-not failure)
        ;; A second handshake, then the call again.
        (nael-rpc-test-answer (nael-rpc-test-find :$/lean/rpc/connect 1)
                              '(:sessionId "4712"))
        (let ((retry (nael-rpc-test-find :$/lean/rpc/call 1)))
          (should (equal (plist-get (plist-get retry :params) :sessionId)
                         "4712"))
          (nael-rpc-test-fail retry nael-rpc-needs-reconnect))
        ;; Only one retry.
        (should (equal (plist-get failure :code) nael-rpc-needs-reconnect))
        (should-not result)))))

(ert-deftest nael-rpc-test-retry-on-content-modified ()
  "A call aborted by an edit is asked again on the same session."
  (nael-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (nael-rpc-test-handle (current-buffer)))
            result)
        (nael-rpc-get-interactive-goals handle (lambda (r) (setq result r)))
        (nael-rpc-test-connect)
        (nael-rpc-test-fail (nael-rpc-test-find :$/lean/rpc/call)
                            nael-rpc-content-modified)
        (let ((retry (nael-rpc-test-find :$/lean/rpc/call 1)))
          (should (equal (plist-get (plist-get retry :params) :sessionId)
                         "4711"))
          (nael-rpc-test-answer retry '(:goals [])))
        (should (equal result '(:goals [])))
        (should-not (nael-rpc-test-find :$/lean/rpc/connect 1))))))

(ert-deftest nael-rpc-test-no-retry-for-references ()
  "A call with a reference is not retried: the reference died too."
  (nael-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (nael-rpc-test-handle (current-buffer)))
            failure)
        (nael-rpc-info-to-interactive
         handle '(:__rpcref "3") #'ignore
         :failure (lambda (e) (setq failure e)))
        (nael-rpc-test-connect)
        (nael-rpc-test-fail (nael-rpc-test-find :$/lean/rpc/call)
                            nael-rpc-needs-reconnect)
        (should (equal (plist-get failure :code) nael-rpc-needs-reconnect))
        (should-not (nael-rpc-test-find :$/lean/rpc/connect 1))))))

(ert-deftest nael-rpc-test-call-sync ()
  "A synchronous call returns the result, or signals the error."
  (nael-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (nael-rpc-test-handle (current-buffer))))
        (nael-rpc-test-connect)
        (run-with-timer 0 nil (lambda ()
                                (nael-rpc-test-answer
                                 (nael-rpc-test-find :$/lean/rpc/call)
                                 [1 2])))
        (should (equal (nael-rpc-call-sync handle "m" nil) [1 2]))
        (run-with-timer 0 nil (lambda ()
                                (nael-rpc-test-fail
                                 (nael-rpc-test-find :$/lean/rpc/call 1)
                                 nael-rpc-needs-reconnect)))
        (let ((error (should-error (nael-rpc-call-sync handle "m" nil
                                                       :retry nil)
                                   :type 'nael-rpc-error)))
          (should (equal (nth 1 error) "m"))
          (should (equal (plist-get (nth 2 error) :code)
                         nael-rpc-needs-reconnect)))
        ;; Not retried, as asked.
        (should-not (nael-rpc-test-find :$/lean/rpc/connect 1))))))

(ert-deftest nael-rpc-test-slot-supersedes ()
  "A call in a slot cancels the previous one; its late answer is dropped."
  (nael-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (nael-rpc-test-handle (current-buffer)))
            results)
        (nael-rpc-test-connect)
        (nael-rpc-get-interactive-goals
         handle (lambda (r) (push (cons 'old r) results)) :slot 'goals)
        (nael-rpc-get-interactive-goals
         handle (lambda (r) (push (cons 'new r) results)) :slot 'goals)
        (let ((old (nael-rpc-test-find :$/lean/rpc/call 0))
              (new (nael-rpc-test-find :$/lean/rpc/call 1)))
          ;; The server is asked to stop working on the old call.
          (should (member (cons :$/cancelRequest
                                (list :id (plist-get old :id)))
                          nael-rpc-test-notifications))
          ;; Answers in the wrong order: new first, then old.  The old
          ;; answer is null, as Lean sends after an edit.
          (nael-rpc-test-answer new '(:goals []))
          (nael-rpc-test-answer old nil))
        (should (equal results '((new :goals []))))))))

(ert-deftest nael-rpc-test-cancel-without-returned-id ()
  "A call is cancelled on the server also when jsonrpc returns no id."
  (nael-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (nael-rpc-test-handle (current-buffer))))
        (nael-rpc-test-connect)
        (cl-letf* ((request (symbol-function 'jsonrpc-async-request))
                   ((symbol-function 'jsonrpc-async-request)
                    (lambda (&rest args) (apply request args) nil))
                   ((symbol-function 'nael-eglot-compat-last-request-id)
                    (lambda (_connection) nael-rpc-test-next-id)))
          (nael-rpc-cancel (nael-rpc-get-interactive-goals handle #'ignore)))
        (should (member (cons :$/cancelRequest
                              (list :id (plist-get
                                         (nael-rpc-test-find :$/lean/rpc/call)
                                         :id)))
                        nael-rpc-test-notifications))))))

(ert-deftest nael-rpc-test-cancelled-result-refs-released ()
  "References in the answer to a cancelled call are released."
  (nael-rpc-test-with-fake
    (with-temp-buffer
      (let* ((handle (nael-rpc-test-handle (current-buffer)))
             (request (progn (nael-rpc-test-connect)
                             (nael-rpc-get-interactive-goals handle #'ignore))))
        (nael-rpc-cancel request)
        (nael-rpc-test-answer (nael-rpc-test-find :$/lean/rpc/call)
                              '(:goals [(:ctx (:__rpcref "8"))]))
        (nael-rpc--flush-garbage (nael-rpc-handle-session handle))
        (should (equal (nael-rpc-test-released) '((:__rpcref "8"))))))))

(ert-deftest nael-rpc-test-cancel-queued-call ()
  "A call cancelled before the handshake is never sent."
  (nael-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (nael-rpc-test-handle (current-buffer))))
        (nael-rpc-cancel (nael-rpc-get-interactive-goals handle #'ignore))
        (nael-rpc-test-connect)
        (should-not (nael-rpc-test-find :$/lean/rpc/call))))))

(ert-deftest nael-rpc-test-close-buffer ()
  "Killing the buffer cancels its slotted calls and closes the session."
  (nael-rpc-test-with-fake
    (let* ((buffer (generate-new-buffer " *nael-rpc-test*"))
           (handle (nael-rpc-test-handle buffer))
           delivered)
      (with-current-buffer buffer
        (setq nael-rpc--uri "file:///A.lean")
        (add-hook 'kill-buffer-hook #'nael-rpc-close-buffer nil t))
      (nael-rpc-test-connect)
      (nael-rpc-get-interactive-goals
       handle (lambda (_) (setq delivered t)) :slot 'goals)
      (kill-buffer buffer)
      (should (nael-rpc-session-closed (nael-rpc-handle-session handle)))
      (nael-rpc-test-answer (nael-rpc-test-find :$/lean/rpc/call) '(:goals []))
      (should-not delivered))))

;;;; Owners:

(ert-deftest nael-rpc-test-owner-release ()
  "An owner releases exactly the references of its results, batched."
  (nael-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (nael-rpc-test-handle (current-buffer)))
            (owner (nael-rpc-make-owner))
            (goals (nael-rpc-test-read-fixture "goals-iris-spinlock.json")))
        (nael-rpc-test-connect)
        (nael-rpc-get-interactive-goals handle #'ignore :owner owner)
        (nael-rpc-get-interactive-term-goal handle #'ignore :owner owner)
        (nael-rpc-test-answer (nael-rpc-test-find :$/lean/rpc/call 0) goals)
        (nael-rpc-test-answer (nael-rpc-test-find :$/lean/rpc/call 1)
                              '(:type (:tag [(:info (:__rpcref "99")) (:text "Nat")])
                                :ctx (:__rpcref "100")))
        (should (= (length (nael-rpc-owner-refs owner)) 68))
        ;; Nothing is released while the rendering is shown.
        (should-not (nael-rpc-test-released))
        (nael-rpc-owner-release owner)
        ;; Batched: sent after `nael-rpc-release-delay'.
        (should-not (nael-rpc-test-released))
        (nael-rpc--flush-garbage (nael-rpc-handle-session handle))
        (should (= (length (nael-rpc-test-released)) 68))
        (should-not (nael-rpc-owner-refs owner))))))

(ert-deftest nael-rpc-test-owner-late-result ()
  "A result arriving after its owner was released is released at once."
  (nael-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (nael-rpc-test-handle (current-buffer)))
            (owner (nael-rpc-make-owner))
            delivered)
        (nael-rpc-test-connect)
        (nael-rpc-get-interactive-goals handle (lambda (_) (setq delivered t))
                                        :owner owner)
        (nael-rpc-owner-release owner)
        (nael-rpc-test-answer (nael-rpc-test-find :$/lean/rpc/call)
                              '(:goals [(:ctx (:__rpcref "5"))]))
        (should delivered)
        (nael-rpc--flush-garbage (nael-rpc-handle-session handle))
        (should (equal (nael-rpc-test-released) '((:__rpcref "5"))))))))

(ert-deftest nael-rpc-test-owner-late-result-of-dead-session ()
  "A late result through a dead session leaves the owner's newer references."
  (nael-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (nael-rpc-test-handle (current-buffer)))
            (owner (nael-rpc-make-owner)))
        (nael-rpc-test-connect)
        (nael-rpc-get-interactive-goals handle #'ignore :owner owner)
        (nael-rpc-get-interactive-term-goal handle #'ignore :owner owner)
        ;; The first call finds the session dead and succeeds on a new one.
        (nael-rpc-test-fail (nael-rpc-test-find :$/lean/rpc/call 0)
                            nael-rpc-needs-reconnect)
        (nael-rpc-test-answer (nael-rpc-test-find :$/lean/rpc/connect 1)
                              '(:sessionId "4712"))
        (nael-rpc-test-answer (nael-rpc-test-find :$/lean/rpc/call 2)
                              '(:goals [(:ctx (:__rpcref "9"))]))
        ;; Then the second call answers through the dead one.
        (nael-rpc-test-answer (nael-rpc-test-find :$/lean/rpc/call 1)
                              '(:ctx (:__rpcref "8")))
        (should (eq (nael-rpc-owner-session owner)
                    (nael-rpc-handle-session handle)))
        (should (equal (nael-rpc-owner-refs owner) '((:__rpcref "9"))))))))

(ert-deftest nael-rpc-test-owner-dead-session ()
  "References of a session that died are dropped, never released."
  (nael-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (nael-rpc-test-handle (current-buffer)))
            (owner (nael-rpc-make-owner)))
        (nael-rpc-test-connect)
        (nael-rpc-get-interactive-goals handle #'ignore :owner owner)
        (nael-rpc-test-answer (nael-rpc-test-find :$/lean/rpc/call)
                              '(:goals [(:ctx (:__rpcref "5"))]))
        (should (nael-rpc-owner-refs owner))
        (nael-rpc--close (nael-rpc-handle-session handle))
        (nael-rpc-owner-release owner)
        (should-not (nael-rpc-test-released))))))

;;;; Keep-alive:

(ert-deftest nael-rpc-test-keepalive ()
  "Keep-alives carry the session id and stop when the session closes."
  (nael-rpc-test-with-fake
    (with-temp-buffer
      (let* ((handle (nael-rpc-test-handle (current-buffer)))
             (session (nael-rpc-handle-session handle)))
        (nael-rpc-test-connect)
        (should (timerp (nael-rpc-session-keepalive-timer session)))
        (should (= (timer--repeat-delay
                    (nael-rpc-session-keepalive-timer session))
                   10))
        (nael-rpc--keepalive session)
        (should (equal (car nael-rpc-test-notifications)
                       (cons :$/lean/rpc/keepAlive
                             (list :uri "file:///A.lean"
                                   :sessionId "4711"))))
        (nael-rpc--close session)
        (should-not (nael-rpc-session-keepalive-timer session))))))

(ert-deftest nael-rpc-test-messages-methods ()
  "Messages are asked for by line; trace children are not retried."
  (nael-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (nael-rpc-test-handle (current-buffer)))
            failure)
        (nael-rpc-get-interactive-diagnostics handle 7 #'ignore)
        (nael-rpc-lazy-trace-children handle '(:__rpcref "3") #'ignore
                                      :failure (lambda (e) (setq failure e)))
        (nael-rpc-test-connect)
        (let ((call (plist-get (nael-rpc-test-find
                                "Lean.Widget.getInteractiveDiagnostics")
                               :params)))
          (should (equal (plist-get call :params)
                         '(:lineRange (:start 7 :end 8)))))
        (let ((call (nael-rpc-test-find
                     "Lean.Widget.lazyTraceChildrenToInteractive")))
          (should (equal (plist-get (plist-get call :params) :params)
                         '(:__rpcref "3")))
          (nael-rpc-test-fail call nael-rpc-needs-reconnect))
        (should (equal (plist-get failure :code) nael-rpc-needs-reconnect))
        (should-not (nael-rpc-test-find :$/lean/rpc/connect 1))))))

(provide 'nael-rpc-tests)

;;; nael-rpc-tests.el ends here
