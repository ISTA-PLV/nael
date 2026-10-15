;;; lena-rpc-tests.el --- Tests for lena-rpc  -*- lexical-binding: t; -*-

;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: GPL-3.0-only

;; This is licensed under GNU General Public License (version 3 only),
;; see lena/LICENSE.GPL3.

;;; Commentary:

;; Unit tests for `lena-rpc' against a fake connection: no Lean server
;; is needed.  `lena-rpc-test-with-fake' replaces the jsonrpc functions
;; that `lena-rpc' calls, records what would have been sent, and lets a
;; test answer requests in any order.

;;; Code:

(require 'ert)
(require 'lena-rpc)
(require 'lena-test-helpers)

(defvar lena-rpc-test-requests nil
  "Requests sent to the fake connection, newest first.
Each is a plist with `:id', `:method', `:params', `:success-fn',
`:error-fn' and `:timeout'.")

(defvar lena-rpc-test-notifications nil
  "Notifications sent to the fake connection, newest first.")

(defvar lena-rpc-test-next-id 0)

(defmacro lena-rpc-test-with-fake (&rest body)
  "Run BODY with a fake Lean server that speaks wire format v1."
  (declare (indent 0) (debug t))
  `(let ((lena-rpc-test-requests nil)
         (lena-rpc-test-notifications nil)
         (lena-rpc-test-next-id 0)
         (lena-rpc--sessions (make-hash-table :test #'equal)))
     (cl-letf (((symbol-function 'jsonrpc-async-request)
                (lambda (_connection method params &rest keys)
                  (let ((id (cl-incf lena-rpc-test-next-id)))
                    (push (list :id id :method method :params params
                                :success-fn (plist-get keys :success-fn)
                                :error-fn (plist-get keys :error-fn)
                                :timeout (plist-get keys :timeout))
                          lena-rpc-test-requests)
                    (list id nil))))
               ((symbol-function 'jsonrpc-notify)
                (lambda (_connection method params)
                  (push (cons method params) lena-rpc-test-notifications)))
               ((symbol-function 'jsonrpc-running-p) #'always)
               ((symbol-function 'lena-eglot-compat-capabilities)
                (lambda (_connection)
                  '(:experimental (:rpcProvider (:rpcWireFormat "v1"))))))
       (unwind-protect (progn ,@body)
         (lena-rpc-close-all)))))

(defun lena-rpc-test-find (method &optional n)
  "Return the Nth oldest request with METHOD, a keyword or RPC method name."
  (nth (or n 0)
       (seq-filter (lambda (request)
                     (or (eq (plist-get request :method) method)
                         (equal (plist-get (plist-get request :params) :method)
                                method)))
                   (reverse lena-rpc-test-requests))))

(defun lena-rpc-test-answer (request result)
  "Answer REQUEST with RESULT."
  (funcall (plist-get request :success-fn) result))

(defun lena-rpc-test-fail (request code)
  "Answer REQUEST with an error of CODE."
  (funcall (plist-get request :error-fn)
           (list :code code :message "fake error")))

(defun lena-rpc-test-handle (buffer)
  "Return a handle for BUFFER on the fake connection, not yet connected."
  (lena-rpc--handle-create
   :session (lena-rpc--session-for 'fake "file:///A.lean")
   :position '(:textDocument (:uri "file:///A.lean")
               :position (:line 3 :character 2))
   :buffer buffer))

(defun lena-rpc-test-connect ()
  "Answer the oldest pending `$/lean/rpc/connect' with a session id."
  (lena-rpc-test-answer (lena-rpc-test-find :$/lean/rpc/connect)
                        '(:sessionId "4711")))

(defun lena-rpc-test-released ()
  "Return all references released so far, oldest release first."
  (seq-mapcat (lambda (notification)
                (when (eq (car notification) :$/lean/rpc/release)
                  (append (plist-get (cdr notification) :refs) nil)))
              (reverse lena-rpc-test-notifications)))

;;;; References:

(ert-deftest lena-rpc-test-ref-p ()
  (should (lena-rpc-ref-p '(:__rpcref "1") :__rpcref))
  (should-not (lena-rpc-ref-p '(:__rpcref 1) :__rpcref))
  (should-not (lena-rpc-ref-p '(:__rpcref "1" :x 2) :__rpcref))
  (should-not (lena-rpc-ref-p '(:p "1") :__rpcref))
  (should (lena-rpc-ref-p '(:p "1") :p)))

(ert-deftest lena-rpc-test-collect-refs-fixture ()
  "Every reference in a recorded goal result is found: hyps, ctx, target."
  (let ((refs (lena-rpc-collect-refs
               (lena-test-read-fixture "goals-iris-spinlock.json")
               :__rpcref)))
    (should (= (length refs) 66))
    (should (= (length (seq-uniq refs)) 66))))

(ert-deftest lena-rpc-test-collect-refs-repeated ()
  "A reference that occurs twice is collected twice."
  (should (equal (lena-rpc-collect-refs
                  [(:__rpcref "1") (:a (:__rpcref "1") :b "x") nil 3]
                  :__rpcref)
                 '((:__rpcref "1") (:__rpcref "1")))))

;;;; Sessions and calls:

(ert-deftest lena-rpc-test-queued-until-connected ()
  "Calls made during the handshake are sent once it settles."
  (lena-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (lena-rpc-test-handle (current-buffer)))
            results)
        (lena-rpc-get-interactive-goals handle (lambda (r) (push r results)))
        (lena-rpc-get-interactive-goals handle (lambda (r) (push r results)))
        (should-not (lena-rpc-test-find :$/lean/rpc/call))
        (lena-rpc-test-connect)
        (let ((first (lena-rpc-test-find :$/lean/rpc/call 0))
              (second (lena-rpc-test-find :$/lean/rpc/call 1)))
          (should (equal (plist-get (plist-get first :params) :sessionId)
                         "4711"))
          (should (equal (plist-get (plist-get first :params) :method)
                         "Lean.Widget.getInteractiveGoals"))
          ;; Goal calls wait for elaboration: no timeout.
          (should-not (plist-get first :timeout))
          (lena-rpc-test-answer first '(:goals []))
          (lena-rpc-test-answer second '(:goals [])))
        (should (= (length results) 2))))))

(ert-deftest lena-rpc-test-connect-failed ()
  "Calls waiting for a failed handshake fail; a new handle connects again."
  (lena-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (lena-rpc-test-handle (current-buffer)))
            failures)
        (dotimes (_ 2)
          (lena-rpc-get-interactive-goals
           handle #'ignore :failure (lambda (e) (push e failures))))
        (lena-rpc-test-fail (lena-rpc-test-find :$/lean/rpc/connect) -32603)
        (should (equal (mapcar #'lena-rpc-error-code failures)
                       '(-32603 -32603)))
        (should-not (lena-rpc-test-find :$/lean/rpc/call))
        (setf (lena-rpc-handle-session handle)
              (lena-rpc--session-for 'fake "file:///A.lean"))
        (should (lena-rpc-test-find :$/lean/rpc/connect 1))))))

(ert-deftest lena-rpc-test-retry-on-dead-session ()
  "A call failing with -32900 is retried once on a new session."
  (lena-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (lena-rpc-test-handle (current-buffer)))
            result failure)
        (lena-rpc-get-interactive-goals
         handle (lambda (r) (setq result r))
         :failure (lambda (e) (setq failure e)))
        (lena-rpc-test-connect)
        (lena-rpc-test-fail (lena-rpc-test-find :$/lean/rpc/call)
                            lena-rpc-needs-reconnect)
        (should-not failure)
        ;; A second handshake, then the call again.
        (lena-rpc-test-answer (lena-rpc-test-find :$/lean/rpc/connect 1)
                              '(:sessionId "4712"))
        (let ((retry (lena-rpc-test-find :$/lean/rpc/call 1)))
          (should (equal (plist-get (plist-get retry :params) :sessionId)
                         "4712"))
          (lena-rpc-test-fail retry lena-rpc-needs-reconnect))
        ;; Only one retry.
        (should (equal (plist-get failure :code) lena-rpc-needs-reconnect))
        (should-not result)))))

(ert-deftest lena-rpc-test-retry-on-content-modified ()
  "A call aborted by an edit is asked again on the same session."
  (lena-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (lena-rpc-test-handle (current-buffer)))
            result)
        (lena-rpc-get-interactive-goals handle (lambda (r) (setq result r)))
        (lena-rpc-test-connect)
        (lena-rpc-test-fail (lena-rpc-test-find :$/lean/rpc/call)
                            lena-rpc-content-modified)
        (let ((retry (lena-rpc-test-find :$/lean/rpc/call 1)))
          (should (equal (plist-get (plist-get retry :params) :sessionId)
                         "4711"))
          (lena-rpc-test-answer retry '(:goals [])))
        (should (equal result '(:goals [])))
        (should-not (lena-rpc-test-find :$/lean/rpc/connect 1))))))

(ert-deftest lena-rpc-test-no-retry-for-references ()
  "A call with a reference is not retried: the reference died too."
  (lena-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (lena-rpc-test-handle (current-buffer)))
            failure)
        (lena-rpc-info-to-interactive
         handle '(:__rpcref "3") #'ignore
         :failure (lambda (e) (setq failure e)))
        (lena-rpc-test-connect)
        (lena-rpc-test-fail (lena-rpc-test-find :$/lean/rpc/call)
                            lena-rpc-needs-reconnect)
        (should (equal (plist-get failure :code) lena-rpc-needs-reconnect))
        (should-not (lena-rpc-test-find :$/lean/rpc/connect 1))))))

(ert-deftest lena-rpc-test-call-sync ()
  "A synchronous call returns the result, or signals the error."
  (lena-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (lena-rpc-test-handle (current-buffer))))
        (lena-rpc-test-connect)
        (run-with-timer 0 nil (lambda ()
                                (lena-rpc-test-answer
                                 (lena-rpc-test-find :$/lean/rpc/call)
                                 [1 2])))
        (should (equal (lena-rpc-call-sync handle "m" nil) [1 2]))
        (run-with-timer 0 nil (lambda ()
                                (lena-rpc-test-fail
                                 (lena-rpc-test-find :$/lean/rpc/call 1)
                                 lena-rpc-needs-reconnect)))
        (let ((error (should-error (lena-rpc-call-sync handle "m" nil
                                                       :retry nil)
                                   :type 'lena-rpc-error)))
          (should (equal (nth 1 error) "m"))
          (should (equal (plist-get (nth 2 error) :code)
                         lena-rpc-needs-reconnect)))
        ;; Not retried, as asked.
        (should-not (lena-rpc-test-find :$/lean/rpc/connect 1))))))

(ert-deftest lena-rpc-test-slot-supersedes ()
  "A call in a slot cancels the previous one; its late answer is dropped."
  (lena-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (lena-rpc-test-handle (current-buffer)))
            results)
        (lena-rpc-test-connect)
        (lena-rpc-get-interactive-goals
         handle (lambda (r) (push (cons 'old r) results)) :slot 'goals)
        (lena-rpc-get-interactive-goals
         handle (lambda (r) (push (cons 'new r) results)) :slot 'goals)
        (let ((old (lena-rpc-test-find :$/lean/rpc/call 0))
              (new (lena-rpc-test-find :$/lean/rpc/call 1)))
          ;; The server is asked to stop working on the old call.
          (should (member (cons :$/cancelRequest
                                (list :id (plist-get old :id)))
                          lena-rpc-test-notifications))
          ;; Answers in the wrong order: new first, then old.  The old
          ;; answer is null, as Lean sends after an edit.
          (lena-rpc-test-answer new '(:goals []))
          (lena-rpc-test-answer old nil))
        (should (equal results '((new :goals []))))))))

(ert-deftest lena-rpc-test-cancel-without-returned-id ()
  "A call is cancelled on the server also when jsonrpc returns no id."
  (lena-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (lena-rpc-test-handle (current-buffer))))
        (lena-rpc-test-connect)
        (cl-letf* ((request (symbol-function 'jsonrpc-async-request))
                   ((symbol-function 'jsonrpc-async-request)
                    (lambda (&rest args) (apply request args) nil))
                   ((symbol-function 'lena-eglot-compat-last-request-id)
                    (lambda (_connection) lena-rpc-test-next-id)))
          (lena-rpc-cancel (lena-rpc-get-interactive-goals handle #'ignore)))
        (should (member (cons :$/cancelRequest
                              (list :id (plist-get
                                         (lena-rpc-test-find :$/lean/rpc/call)
                                         :id)))
                        lena-rpc-test-notifications))))))

(ert-deftest lena-rpc-test-cancelled-result-refs-released ()
  "References in the answer to a cancelled call are released."
  (lena-rpc-test-with-fake
    (with-temp-buffer
      (let* ((handle (lena-rpc-test-handle (current-buffer)))
             (request (progn (lena-rpc-test-connect)
                             (lena-rpc-get-interactive-goals handle #'ignore))))
        (lena-rpc-cancel request)
        (lena-rpc-test-answer (lena-rpc-test-find :$/lean/rpc/call)
                              '(:goals [(:ctx (:__rpcref "8"))]))
        (lena-rpc--flush-garbage (lena-rpc-handle-session handle))
        (should (equal (lena-rpc-test-released) '((:__rpcref "8"))))))))

(ert-deftest lena-rpc-test-cancel-queued-call ()
  "A call cancelled before the handshake is never sent."
  (lena-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (lena-rpc-test-handle (current-buffer))))
        (lena-rpc-cancel (lena-rpc-get-interactive-goals handle #'ignore))
        (lena-rpc-test-connect)
        (should-not (lena-rpc-test-find :$/lean/rpc/call))))))

(ert-deftest lena-rpc-test-close-buffer ()
  "Killing the buffer cancels its slotted calls and closes the session."
  (lena-rpc-test-with-fake
    (let* ((buffer (generate-new-buffer " *lena-rpc-test*"))
           (handle (lena-rpc-test-handle buffer))
           delivered)
      (with-current-buffer buffer
        (setq lena-rpc--uri "file:///A.lean")
        (add-hook 'kill-buffer-hook #'lena-rpc-close-buffer nil t))
      (lena-rpc-test-connect)
      (lena-rpc-get-interactive-goals
       handle (lambda (_) (setq delivered t)) :slot 'goals)
      (kill-buffer buffer)
      (should (lena-rpc-session-closed (lena-rpc-handle-session handle)))
      (lena-rpc-test-answer (lena-rpc-test-find :$/lean/rpc/call) '(:goals []))
      (should-not delivered))))

;;;; Owners:

(ert-deftest lena-rpc-test-owner-release ()
  "An owner releases exactly the references of its results, batched."
  (lena-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (lena-rpc-test-handle (current-buffer)))
            (owner (lena-rpc-make-owner))
            (goals (lena-test-read-fixture "goals-iris-spinlock.json")))
        (lena-rpc-test-connect)
        (lena-rpc-get-interactive-goals handle #'ignore :owner owner)
        (lena-rpc-get-interactive-term-goal handle #'ignore :owner owner)
        (lena-rpc-test-answer (lena-rpc-test-find :$/lean/rpc/call 0) goals)
        (lena-rpc-test-answer (lena-rpc-test-find :$/lean/rpc/call 1)
                              '(:type (:tag [(:info (:__rpcref "99")) (:text "Nat")])
                                :ctx (:__rpcref "100")))
        (should (= (length (lena-rpc-owner-refs owner)) 68))
        ;; Nothing is released while the rendering is shown.
        (should-not (lena-rpc-test-released))
        (lena-rpc-owner-release owner)
        ;; Batched: sent after `lena-rpc-release-delay'.
        (should-not (lena-rpc-test-released))
        (lena-rpc--flush-garbage (lena-rpc-handle-session handle))
        (should (= (length (lena-rpc-test-released)) 68))
        (should-not (lena-rpc-owner-refs owner))))))

(ert-deftest lena-rpc-test-owner-late-result ()
  "A result arriving after its owner was released is released at once."
  (lena-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (lena-rpc-test-handle (current-buffer)))
            (owner (lena-rpc-make-owner))
            delivered)
        (lena-rpc-test-connect)
        (lena-rpc-get-interactive-goals handle (lambda (_) (setq delivered t))
                                        :owner owner)
        (lena-rpc-owner-release owner)
        (lena-rpc-test-answer (lena-rpc-test-find :$/lean/rpc/call)
                              '(:goals [(:ctx (:__rpcref "5"))]))
        (should delivered)
        (lena-rpc--flush-garbage (lena-rpc-handle-session handle))
        (should (equal (lena-rpc-test-released) '((:__rpcref "5"))))))))

(ert-deftest lena-rpc-test-owner-late-result-of-dead-session ()
  "A late result through a dead session leaves the owner's newer references."
  (lena-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (lena-rpc-test-handle (current-buffer)))
            (owner (lena-rpc-make-owner)))
        (lena-rpc-test-connect)
        (lena-rpc-get-interactive-goals handle #'ignore :owner owner)
        (lena-rpc-get-interactive-term-goal handle #'ignore :owner owner)
        ;; The first call finds the session dead and succeeds on a new one.
        (lena-rpc-test-fail (lena-rpc-test-find :$/lean/rpc/call 0)
                            lena-rpc-needs-reconnect)
        (lena-rpc-test-answer (lena-rpc-test-find :$/lean/rpc/connect 1)
                              '(:sessionId "4712"))
        (lena-rpc-test-answer (lena-rpc-test-find :$/lean/rpc/call 2)
                              '(:goals [(:ctx (:__rpcref "9"))]))
        ;; Then the second call answers through the dead one.
        (lena-rpc-test-answer (lena-rpc-test-find :$/lean/rpc/call 1)
                              '(:ctx (:__rpcref "8")))
        (should (eq (lena-rpc-owner-session owner)
                    (lena-rpc-handle-session handle)))
        (should (equal (lena-rpc-owner-refs owner) '((:__rpcref "9"))))))))

(ert-deftest lena-rpc-test-owner-dead-session ()
  "References of a session that died are dropped, never released."
  (lena-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (lena-rpc-test-handle (current-buffer)))
            (owner (lena-rpc-make-owner)))
        (lena-rpc-test-connect)
        (lena-rpc-get-interactive-goals handle #'ignore :owner owner)
        (lena-rpc-test-answer (lena-rpc-test-find :$/lean/rpc/call)
                              '(:goals [(:ctx (:__rpcref "5"))]))
        (should (lena-rpc-owner-refs owner))
        (lena-rpc--close (lena-rpc-handle-session handle))
        (lena-rpc-owner-release owner)
        (should-not (lena-rpc-test-released))))))

;;;; Keep-alive:

(ert-deftest lena-rpc-test-keepalive ()
  "Keep-alives carry the session id and stop when the session closes."
  (lena-rpc-test-with-fake
    (with-temp-buffer
      (let* ((handle (lena-rpc-test-handle (current-buffer)))
             (session (lena-rpc-handle-session handle)))
        (lena-rpc-test-connect)
        (should (timerp (lena-rpc-session-keepalive-timer session)))
        (should (= (timer--repeat-delay
                    (lena-rpc-session-keepalive-timer session))
                   10))
        (lena-rpc--keepalive session)
        (should (equal (car lena-rpc-test-notifications)
                       (cons :$/lean/rpc/keepAlive
                             (list :uri "file:///A.lean"
                                   :sessionId "4711"))))
        (lena-rpc--close session)
        (should-not (lena-rpc-session-keepalive-timer session))))))

(ert-deftest lena-rpc-test-messages-methods ()
  "Messages are asked for by line; trace children are not retried."
  (lena-rpc-test-with-fake
    (with-temp-buffer
      (let ((handle (lena-rpc-test-handle (current-buffer)))
            failure)
        (lena-rpc-get-interactive-diagnostics handle 7 #'ignore)
        (lena-rpc-lazy-trace-children handle '(:__rpcref "3") #'ignore
                                      :failure (lambda (e) (setq failure e)))
        (lena-rpc-test-connect)
        (let ((call (plist-get (lena-rpc-test-find
                                "Lean.Widget.getInteractiveDiagnostics")
                               :params)))
          (should (equal (plist-get call :params)
                         '(:lineRange (:start 7 :end 8)))))
        (let ((call (lena-rpc-test-find
                     "Lean.Widget.lazyTraceChildrenToInteractive")))
          (should (equal (plist-get (plist-get call :params) :params)
                         '(:__rpcref "3")))
          (lena-rpc-test-fail call lena-rpc-needs-reconnect))
        (should (equal (plist-get failure :code) lena-rpc-needs-reconnect))
        (should-not (lena-rpc-test-find :$/lean/rpc/connect 1))))))

(provide 'lena-rpc-tests)

;;; lena-rpc-tests.el ends here
