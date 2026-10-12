;;; lena-rpc.el --- Lean RPC sessions for Lena  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jan Mas Rovira
;; Copyright (C) 2026 Doug Torrance
;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: MIT

;; This file is licensed under the MIT License, see LICENSE.MIT.

;; Derived from `lean4-rpc.el' of lean4-mode
;; <https://github.com/d-torrance/lean4-mode> at commit 0c2216dd43, which
;; is itself derived from `neo-lean-rpc.el' of neo-lean-mode
;; <https://github.com/janmasrovira/neo-lean-mode> by Jan Mas Rovira.
;; Changed for Lena: names; Eglot internals via `lena-eglot-compat';
;; keep-alive every 10 seconds; pending edits are sent before a call is
;; made; per-call timeouts, none for calls that wait for elaboration;
;; requests that can be cancelled and superseded in slots; references
;; are owned by renderings (`lena-rpc-owner') and found by a strict
;; walk of the whole result; references of cancelled calls are released;
;; sessions are closed with their buffer.

;;; Commentary:

;; Lean's interactive features -- goals whose subterms can be hovered and
;; jumped to, collapsible traces, goal diffs, user widgets -- are not
;; plain LSP requests.  They are calls into the running file worker, made
;; over a session established with `$/lean/rpc/connect' and carried by
;; `$/lean/rpc/call'.  This library is that session layer; it renders
;; nothing.
;;
;; Sessions expire.  The server drops a session it has not heard from for
;; 30 seconds and checks this only when a message arrives, so keep-alives
;; are sent every 10 seconds.  A session also dies whenever the file
;; worker restarts, which happens routinely -- editing an import does it
;; -- and is reported as one of four error codes.  Callers should not
;; have to care, so a dead session is reconnected once, transparently,
;; and the call retried.
;;
;; Calls carry a position.  `$/lean/rpc/call' extends
;; TextDocumentPositionParams because Lean resolves user-registered RPC
;; methods against the environment as it stands at that point in the
;; file.  Hence `lena-rpc-open' returns a position-bound handle rather
;; than a bare session.  It also sends pending edits first: Eglot holds
;; them back for a while, and the position must refer to text the server
;; has seen.
;;
;; Calls can be superseded.  A call made in a slot of a buffer cancels
;; the previous call in that slot, and an answer to a cancelled call is
;; never delivered.  Lean answers a call whose region was edited
;; meanwhile with null rather than an error, so dropping stale answers is
;; necessary, not just tidy.
;;
;; Results contain references to server-side objects, one for every
;; subterm of a goal.  The server keeps each until it is told, once per
;; time it was served, that the client is done with it.  An owner
;; (`lena-rpc-make-owner') collects the references of the results that
;; one rendering shows; releasing the owner when the rendering is
;; replaced releases them all, batched.  References of a session that
;; died are gone with it and are never released.

;;; Code:

(require 'cl-lib)
(require 'jsonrpc)

(require 'lena-eglot)

;;;; References:

;; A reference is a JSON object with exactly one member whose value is a
;; string: {"__rpcref": "17"} in wire format v1, {"p": "17"} in v0.  The
;; server announces the format it uses; see
;; `lena-eglot-rpc-wire-format'.

(defconst lena-rpc--ref-key-v1 :__rpcref)
(defconst lena-rpc--ref-key-v0 :p)

(defun lena-rpc--negotiated-ref-key (connection)
  "Return the plist key that CONNECTION uses for references."
  (let ((format (thread-first
                  (lena-eglot-compat-capabilities connection)
                  (plist-get :experimental)
                  (plist-get :rpcProvider)
                  (plist-get :rpcWireFormat))))
    (if (equal format "v1") lena-rpc--ref-key-v1 lena-rpc--ref-key-v0)))

(defun lena-rpc-ref-p (object key)
  "Return non-nil if OBJECT is a reference encoded with KEY."
  (and (consp object)
       (eq (car object) key)
       (consp (cdr object))
       (stringp (cadr object))
       (null (cddr object))))

(defun lena-rpc-collect-refs (object key)
  "Return the references in OBJECT, a parsed JSON value, in order.
KEY is the plist key of references.  Every occurrence counts, because
the server counts every time it serves a reference."
  (let (refs)
    (cl-labels ((walk (x)
                  (cond ((lena-rpc-ref-p x key) (push x refs))
                        ((and (consp x) (keywordp (car x)))
                         (cl-loop for (_key value) on x by #'cddr
                                  do (walk value)))
                        ((vectorp x) (mapc #'walk x)))))
      (walk object))
    (nreverse refs)))

;;;; Error codes:

;; From Lean's `Lean/Data/JsonRpc.lean'.  The last three mean that the
;; session no longer exists and every reference obtained through it is
;; worthless.  Lean also answers `content-modified' when an edit aborts
;; a single request, so that one is only worth asking again: should the
;; worker be gone, the next answer is one of the others.
(defconst lena-rpc-content-modified -32801)
(defconst lena-rpc-needs-reconnect -32900)
(defconst lena-rpc-worker-exited -32901)
(defconst lena-rpc-worker-crashed -32902)

(defconst lena-rpc--dead-codes
  (list lena-rpc-needs-reconnect
        lena-rpc-worker-exited
        lena-rpc-worker-crashed)
  "Error codes meaning that the RPC session must be re-established.")

(defun lena-rpc-dead-code-p (code)
  "Return non-nil if CODE says that the RPC session is gone."
  (and (integerp code) (memq code lena-rpc--dead-codes) t))

(defun lena-rpc-error-code (error)
  "Return the JSON-RPC code of ERROR, however jsonrpc.el wrapped it."
  (or (plist-get error :code)
      (plist-get (plist-get error :error) :code)))

;;;; Sessions:

(defconst lena-rpc-keepalive-interval 10
  "Seconds between `$/lean/rpc/keepAlive' notifications.
The server expires a session after 30 seconds of silence.  Emacs timers
do not fire during long commands, hence the margin.")

(defconst lena-rpc-release-batch-size 100
  "Number of references to accumulate before releasing them.")

(defconst lena-rpc-release-delay 0.1
  "Seconds to wait before releasing accumulated references.")

(cl-defstruct (lena-rpc-session (:constructor lena-rpc--session-create)
                                (:copier nil))
  "An RPC session bound to one document on one server connection."
  connection                            ; the Eglot server
  uri                                   ; document URI
  ref-key                               ; plist key of references
  id                                    ; server-assigned session id
  (settled nil)                         ; handshake resolved, either way
  error                                 ; non-nil if the handshake failed
  keepalive-timer
  (pending nil)                         ; thunks queued until settled
  (garbage nil)                         ; references awaiting release
  release-timer
  (closed nil))

(defvar lena-rpc--sessions (make-hash-table :test #'equal)
  "Sessions, keyed by document URI.")

(defun lena-rpc-connection-live-p (connection)
  "Return non-nil if CONNECTION can still carry a request."
  (and connection (jsonrpc-running-p connection)))

(defun lena-rpc--flush-pending (session)
  "Run and clear the thunks queued on SESSION."
  (let ((thunks (nreverse (lena-rpc-session-pending session))))
    (setf (lena-rpc-session-pending session) nil)
    (mapc #'funcall thunks)))

(defun lena-rpc--keepalive (session)
  "Send a keep-alive for SESSION, or close it if its server is gone."
  (cond ((lena-rpc-session-closed session)
         (lena-rpc--stop-keepalive session))
        ((not (lena-rpc-connection-live-p
               (lena-rpc-session-connection session)))
         (lena-rpc--close session))
        ((lena-rpc-session-id session)
         (ignore-errors
           (jsonrpc-notify (lena-rpc-session-connection session)
                           :$/lean/rpc/keepAlive
                           (list :uri (lena-rpc-session-uri session)
                                 :sessionId (lena-rpc-session-id session)))))))

(defun lena-rpc--start-keepalive (session)
  "Begin sending keep-alive notifications for SESSION."
  (setf (lena-rpc-session-keepalive-timer session)
        (run-with-timer lena-rpc-keepalive-interval
                        lena-rpc-keepalive-interval
                        #'lena-rpc--keepalive session)))

(defun lena-rpc--stop-keepalive (session)
  "Cancel the keep-alive timer of SESSION."
  (when (timerp (lena-rpc-session-keepalive-timer session))
    (cancel-timer (lena-rpc-session-keepalive-timer session)))
  (setf (lena-rpc-session-keepalive-timer session) nil))

(defun lena-rpc--close (session)
  "Close SESSION: stop its timers and drop it from the registry.
References awaiting release are abandoned: the server has discarded
everything belonging to a dead session."
  (setf (lena-rpc-session-closed session) t)
  (lena-rpc--stop-keepalive session)
  (when (timerp (lena-rpc-session-release-timer session))
    (cancel-timer (lena-rpc-session-release-timer session)))
  (setf (lena-rpc-session-release-timer session) nil
        (lena-rpc-session-garbage session) nil)
  (when (eq session (gethash (lena-rpc-session-uri session)
                             lena-rpc--sessions))
    (remhash (lena-rpc-session-uri session) lena-rpc--sessions)))

(defun lena-rpc--connect (connection uri)
  "Open a session for URI on CONNECTION and register it at once.
Registering before the handshake resolves lets callers queue calls on a
session that does not exist yet, which is the common case: the first
call is what triggers the connect."
  (let ((session (lena-rpc--session-create
                  :connection connection
                  :uri uri
                  :ref-key (lena-rpc--negotiated-ref-key connection))))
    (puthash uri session lena-rpc--sessions)
    (cl-flet ((fail (error)
                (setf (lena-rpc-session-error session) error
                      (lena-rpc-session-settled session) t)
                (lena-rpc--close session)
                ;; Queued calls must fail rather than hang.
                (lena-rpc--flush-pending session)))
      ;; The handshake can signal if the server exited meanwhile.
      (condition-case error
          (jsonrpc-async-request
           connection :$/lean/rpc/connect (list :uri uri)
           :success-fn
           (lambda (result)
             (setf (lena-rpc-session-id session)
                   (plist-get result :sessionId)
                   (lena-rpc-session-settled session) t)
             (unless (lena-rpc-session-closed session)
               (lena-rpc--start-keepalive session))
             (lena-rpc--flush-pending session))
           :error-fn #'fail
           :timeout-fn
           (lambda ()
             (fail (list :code lena-rpc-worker-exited
                         :message "Lean RPC connect timed out"))))
        (error (fail (list :code lena-rpc-worker-exited
                           :message (error-message-string error))))))
    session))

(defun lena-rpc--session-for (connection uri)
  "Return a usable session for URI on CONNECTION, creating one if needed.
A session of a previous connection is discarded: after a server restart
its id means nothing."
  (let ((existing (gethash uri lena-rpc--sessions)))
    (if (and existing
             (not (lena-rpc-session-closed existing))
             (eq (lena-rpc-session-connection existing) connection)
             (lena-rpc-connection-live-p connection))
        existing
      (when existing (lena-rpc--close existing))
      (lena-rpc--connect connection uri))))

(defun lena-rpc-close-all (&optional connection)
  "Close every session, or only those of CONNECTION."
  (let (doomed)
    (maphash (lambda (_uri session)
               (when (or (null connection)
                         (eq (lena-rpc-session-connection session)
                             connection))
                 (push session doomed)))
             lena-rpc--sessions)
    (mapc #'lena-rpc--close doomed)))

;;;; Releasing references:

(defun lena-rpc-release (session refs)
  "Tell the server of SESSION that REFS are no longer needed.
The release is batched: a goal has a reference per subterm, and a
notification per reference would be thousands per keystroke."
  (when (and refs (not (lena-rpc-session-closed session)))
    (setf (lena-rpc-session-garbage session)
          (append refs (lena-rpc-session-garbage session)))
    (if (>= (length (lena-rpc-session-garbage session))
            lena-rpc-release-batch-size)
        (lena-rpc--flush-garbage session)
      (unless (timerp (lena-rpc-session-release-timer session))
        (setf (lena-rpc-session-release-timer session)
              (run-with-timer lena-rpc-release-delay nil
                              #'lena-rpc--flush-garbage session))))))

(defun lena-rpc--flush-garbage (session)
  "Send `$/lean/rpc/release' for the references SESSION accumulated."
  (when (timerp (lena-rpc-session-release-timer session))
    (cancel-timer (lena-rpc-session-release-timer session)))
  (setf (lena-rpc-session-release-timer session) nil)
  (let ((refs (lena-rpc-session-garbage session)))
    (setf (lena-rpc-session-garbage session) nil)
    (when (and refs
               (not (lena-rpc-session-closed session))
               (lena-rpc-session-id session))
      (ignore-errors
        (jsonrpc-notify (lena-rpc-session-connection session)
                        :$/lean/rpc/release
                        (list :uri (lena-rpc-session-uri session)
                              :sessionId (lena-rpc-session-id session)
                              :refs (vconcat refs)))))))

(cl-defstruct (lena-rpc-owner (:constructor lena-rpc-make-owner)
                              (:copier nil))
  "The references that one rendering keeps alive.
Create one owner per rendering, pass it as `:owner' to the calls whose
results the rendering shows, and release it with `lena-rpc-owner-release'
once the rendering is replaced or gone."
  session                               ; session the refs belong to
  (refs nil)
  (released nil))

(defun lena-rpc-owner-adopt (owner session result)
  "Make OWNER keep the references in RESULT, obtained through SESSION."
  (let ((refs (lena-rpc-collect-refs result
                                     (lena-rpc-session-ref-key session))))
    (cond
     ;; A late result for a rendering that is already gone.
     ((lena-rpc-owner-released owner)
      (lena-rpc-release session refs))
     ;; A late result through a session that has since died: its
     ;; references are worthless, and the owner may hold newer ones.
     ((lena-rpc-session-closed session))
     (t
      ;; References of an earlier session died with that session.
      (unless (eq session (lena-rpc-owner-session owner))
        (setf (lena-rpc-owner-session owner) session
              (lena-rpc-owner-refs owner) nil))
      (setf (lena-rpc-owner-refs owner)
            (nconc refs (lena-rpc-owner-refs owner)))))))

(defun lena-rpc-owner-release (owner)
  "Release the references that OWNER keeps.  OWNER cannot be reused."
  (unless (lena-rpc-owner-released owner)
    (setf (lena-rpc-owner-released owner) t)
    (when-let* ((session (lena-rpc-owner-session owner)))
      (lena-rpc-release session (lena-rpc-owner-refs owner)))
    (setf (lena-rpc-owner-refs owner) nil)))

;;;; Handles:

(cl-defstruct (lena-rpc-handle (:constructor lena-rpc--handle-create)
                               (:copier nil))
  "A position-bound handle for making RPC calls."
  session
  position                              ; TextDocumentPositionParams
  buffer)                               ; the source buffer

(defvar-local lena-rpc--uri nil
  "URI of the RPC session of the current buffer, if any was opened.")

(defvar-local lena-rpc--slots nil
  "Alist mapping slot names to the latest request in that slot.")

(defun lena-rpc-close-buffer ()
  "Cancel the slotted calls and close the RPC session of this buffer."
  (dolist (entry lena-rpc--slots)
    (lena-rpc-cancel (cdr entry)))
  (setq lena-rpc--slots nil)
  (when-let* ((uri lena-rpc--uri)
              (session (gethash uri lena-rpc--sessions)))
    (lena-rpc--close session)))

(defun lena-rpc--managed-mode-changed ()
  "Close the RPC session of this buffer when Eglot stops managing it."
  (unless (eglot-managed-p)
    (lena-rpc-close-buffer)))

(defun lena-rpc-open (&optional pos)
  "Return an RPC handle bound to POS, defaulting to point.
Pending edits of the current buffer are sent to the server first, so
that the position refers to text that the server has seen."
  (let ((server (lena-eglot-server-or-lose)))
    (lena-eglot-flush-changes)
    (let* ((position (lena-eglot-compat-position-params pos))
           (uri (plist-get (plist-get position :textDocument) :uri)))
      (setq lena-rpc--uri uri)
      (add-hook 'kill-buffer-hook #'lena-rpc-close-buffer nil t)
      (add-hook 'eglot-managed-mode-hook
                #'lena-rpc--managed-mode-changed nil t)
      (lena-rpc--handle-create
       :session (lena-rpc--session-for server uri)
       :position position
       :buffer (current-buffer)))))

(defun lena-rpc-handle-live-p (handle)
  "Return non-nil if HANDLE can still be used."
  (and handle
       (let ((session (lena-rpc-handle-session handle)))
         (and (not (lena-rpc-session-closed session))
              (lena-rpc-connection-live-p
               (lena-rpc-session-connection session))))))

;;;; Calls:

(cl-defstruct (lena-rpc-request (:constructor lena-rpc--request-create)
                                (:copier nil))
  "An RPC call, possibly asked again once."
  handle method params success failure owner timeout retry
  id                                    ; JSON-RPC id, if known
  session                               ; session it was sent on
  (cancelled nil)
  (done nil))

(defun lena-rpc--deliver-failure (request error)
  "Pass ERROR to the failure function of REQUEST, unless it is moot."
  (unless (or (lena-rpc-request-cancelled request)
              (lena-rpc-request-done request))
    (setf (lena-rpc-request-done request) t)
    (funcall (lena-rpc-request-failure request) error)))

(defun lena-rpc--succeed (request session result)
  "Deliver RESULT of REQUEST, which was answered through SESSION."
  (cond
   ((or (lena-rpc-request-cancelled request)
        (lena-rpc-request-done request))
    ;; Nobody will show this result, but the server counts its
    ;; references as served.
    (lena-rpc-release session (lena-rpc-collect-refs
                               result (lena-rpc-session-ref-key session))))
   (t
    (setf (lena-rpc-request-done request) t)
    (when-let* ((owner (lena-rpc-request-owner request)))
      (lena-rpc-owner-adopt owner session result))
    (funcall (lena-rpc-request-success request) result))))

(defun lena-rpc--fail (request session error)
  "Handle ERROR of REQUEST, which was sent through SESSION."
  (let ((code (lena-rpc-error-code error))
        (handle (lena-rpc-request-handle request)))
    (when (lena-rpc-dead-code-p code)
      (lena-rpc--close session))
    (cond
     ((or (lena-rpc-request-cancelled request)
          (lena-rpc-request-done request)))
     ((and (or (lena-rpc-dead-code-p code)
               (eql code lena-rpc-content-modified))
           (lena-rpc-request-retry request)
           (buffer-live-p (lena-rpc-handle-buffer handle)))
      (setf (lena-rpc-request-retry request) nil
            (lena-rpc-handle-session handle)
            (lena-rpc--session-for (lena-rpc-session-connection session)
                                   (lena-rpc-session-uri session)))
      (lena-rpc--attempt request))
     (t (lena-rpc--deliver-failure request error)))))

(defun lena-rpc--send (request)
  "Send REQUEST on the session of its handle, which has settled."
  (let* ((handle (lena-rpc-request-handle request))
         (session (lena-rpc-handle-session handle))
         (connection (lena-rpc-session-connection session)))
    (cond
     ((lena-rpc-request-cancelled request))
     ((lena-rpc-session-error session)
      (lena-rpc--fail request session (lena-rpc-session-error session)))
     ((lena-rpc-session-closed session)
      (lena-rpc--fail request session
                      (list :code lena-rpc-needs-reconnect
                            :message "Lean RPC session is closed")))
     (t
      (setf (lena-rpc-request-session request) session)
      (condition-case error
          (let ((sent (jsonrpc-async-request
                       connection :$/lean/rpc/call
                       (append (lena-rpc-handle-position handle)
                               (list :sessionId (lena-rpc-session-id session)
                                     :method (lena-rpc-request-method request)
                                     :params (or (lena-rpc-request-params
                                                  request)
                                                 (make-hash-table))))
                       :success-fn
                       (lambda (result)
                         (lena-rpc--succeed request session result))
                       :error-fn
                       (lambda (error)
                         (lena-rpc--fail request session error))
                       :timeout (lena-rpc-request-timeout request)
                       :timeout-fn
                       (lambda ()
                         (lena-rpc--cancel-on-server request)
                         (lena-rpc--deliver-failure
                          request (list :code nil :timeout t
                                        :message "Lean RPC call timed out"))))))
            ;; Since jsonrpc 1.0.26, the result starts with the id.
            (setf (lena-rpc-request-id request)
                  (or (car-safe sent)
                      (lena-eglot-compat-last-request-id connection))))
        ;; A server that has exited takes its sessions with it.  Report
        ;; it like a dead session instead of letting jsonrpc signal from
        ;; a timer or hook.
        (error
         (lena-rpc--close session)
         (lena-rpc--deliver-failure
          request (list :code lena-rpc-worker-exited
                        :message (error-message-string error)))))))))

(defun lena-rpc--attempt (request)
  "Send REQUEST now, or once the handshake of its session has settled."
  (let ((session (lena-rpc-handle-session (lena-rpc-request-handle request))))
    (if (lena-rpc-session-settled session)
        (lena-rpc--send request)
      (push (lambda () (lena-rpc--send request))
            (lena-rpc-session-pending session)))))

(defun lena-rpc--cancel-on-server (request)
  "Ask the server to stop working on REQUEST, if it was sent."
  (when-let* ((id (lena-rpc-request-id request))
              (session (lena-rpc-request-session request))
              ((not (lena-rpc-session-closed session)))
              (connection (lena-rpc-session-connection session)))
    (ignore-errors
      (jsonrpc-notify connection :$/cancelRequest (list :id id)))))

(defun lena-rpc-cancel (request)
  "Cancel REQUEST: its success and failure functions will not be called.
If REQUEST was already sent, the server is asked to stop working on it.
That needs jsonrpc 1.0.26 or later, which reports request ids."
  (unless (or (lena-rpc-request-cancelled request)
              (lena-rpc-request-done request))
    (setf (lena-rpc-request-cancelled request) t)
    (lena-rpc--cancel-on-server request)))

(cl-defun lena-rpc-call (handle method params success
                                &key failure owner slot
                                (timeout jsonrpc-default-request-timeout)
                                (retry t))
  "Call the Lean RPC METHOD with PARAMS through HANDLE.
Return the request, which `lena-rpc-cancel' accepts.

SUCCESS receives the result.  FAILURE, if non-nil, receives the error
plist; otherwise errors are dropped, which is usually right for a
display that will be refreshed anyway.  A timeout is reported to
FAILURE as an error with a non-nil `:timeout'.

OWNER, if non-nil, keeps the references in the result; see
`lena-rpc-make-owner'.  Without OWNER, releasing them is up to the
caller.

SLOT, if non-nil, is a symbol naming the purpose of the call, such as
`goals'.  A call in a slot of the buffer of HANDLE cancels the previous
call in the same slot, so that a stale answer is never delivered.

TIMEOUT is the number of seconds to wait for an answer, or nil to wait
indefinitely; it defaults to `jsonrpc-default-request-timeout'.
Calls for goals and expected types wait until the server has
elaborated the file up to their position, which can take arbitrarily
long; they have no timeout.

A call made while the session is still connecting is queued.  If RETRY
is non-nil, as by default, a call that fails because the session died
is retried once on a new session."
  (let ((request (lena-rpc--request-create
                  :handle handle :method method :params params
                  :success success :failure (or failure #'ignore)
                  :owner owner :timeout timeout :retry retry)))
    (when slot
      (with-current-buffer (lena-rpc-handle-buffer handle)
        (when-let* ((previous (alist-get slot lena-rpc--slots)))
          (lena-rpc-cancel previous))
        (setf (alist-get slot lena-rpc--slots) request)))
    (lena-rpc--attempt request)
    request))

(defconst lena-rpc-sync-timeout 3
  "Seconds to wait for a synchronous RPC call before giving up.")

(define-error 'lena-rpc-error "Lean RPC call failed")

(defun lena-rpc-call-sync (handle method params &rest keys)
  "Call METHOD with PARAMS through HANDLE and wait for the result.
Return the result.  If the call fails or does not answer within
`lena-rpc-sync-timeout', signal `lena-rpc-error' with the method and
the error plist, as passed to the FAILURE function of `lena-rpc-call'.
Only for commands invoked by the user, where an answer is required
synchronously, such as `xref'.  KEYS are as for `lena-rpc-call', but
for SUCCESS, FAILURE and TIMEOUT.  Releasing references in the result
is up to the caller."
  (let ((deadline (+ (float-time) lena-rpc-sync-timeout))
        result failure done)
    (let ((request (apply #'lena-rpc-call handle method params
                          (lambda (value) (setq result value done t))
                          :failure (lambda (error) (setq failure error done t))
                          :timeout lena-rpc-sync-timeout
                          keys)))
      (while (and (not done) (< (float-time) deadline))
        (accept-process-output nil 0.02))
      (unless done
        (lena-rpc-cancel request)
        (setq failure (list :code nil :timeout t
                            :message "Lean RPC call timed out"))))
    (if failure
        (signal 'lena-rpc-error (list method failure))
      result)))

;;;; Methods:

;; Registered with `registerBuiltinRpcProcedure' in Lean's
;; `Lean/Server/FileWorker/WidgetRequests.lean' and
;; `Lean/Server/FileWorker/RequestHandling.lean'.

(defun lena-rpc-get-interactive-goals (handle success &rest keys)
  "Request the tactic goals at the position of HANDLE.
SUCCESS receives the goals or nil; KEYS are as for `lena-rpc-call',
whose TIMEOUT defaults to none here."
  (apply #'lena-rpc-call handle "Lean.Widget.getInteractiveGoals"
         (lena-rpc-handle-position handle) success
         (append keys (list :timeout nil))))

(defun lena-rpc-get-interactive-term-goal (handle success &rest keys)
  "Request the expected type at the position of HANDLE.
SUCCESS receives the term goal or nil; KEYS are as for `lena-rpc-call',
whose TIMEOUT defaults to none here."
  (apply #'lena-rpc-call handle "Lean.Widget.getInteractiveTermGoal"
         (lena-rpc-handle-position handle) success
         (append keys (list :timeout nil))))

(defun lena-rpc-info-to-interactive (handle info success &rest keys)
  "Request type and documentation of INFO, a subterm reference.
INFO must come from a result of the session of HANDLE.  SUCCESS
receives the result; KEYS are as for `lena-rpc-call'.  A call that
fails because the session died is not retried: INFO died with it."
  (apply #'lena-rpc-call handle
         "Lean.Widget.InteractiveDiagnostics.infoToInteractive"
         info success (append keys (list :retry nil))))

(defun lena-rpc-get-interactive-diagnostics (handle line success &rest keys)
  "Request the messages that touch LINE, 0-based, in the file of HANDLE.
The server answers at once with the messages it has, whose full range
touches LINE.  SUCCESS receives a vector of `InteractiveDiagnostic's;
KEYS are as for `lena-rpc-call'."
  (apply #'lena-rpc-call handle "Lean.Widget.getInteractiveDiagnostics"
         (list :lineRange (list :start line :end (1+ line)))
         success keys))

(defun lena-rpc-lazy-trace-children (handle children success &rest keys)
  "Request the children of a trace whose CHILDREN are a reference.
CHILDREN is the `lazy' reference of a trace in a message from the
session of HANDLE.  SUCCESS receives a vector of messages; KEYS are as
for `lena-rpc-call'.  A call that fails because the session died is
not retried."
  (apply #'lena-rpc-call handle "Lean.Widget.lazyTraceChildrenToInteractive"
         children success (append keys (list :retry nil))))

(provide 'lena-rpc)

;;; lena-rpc.el ends here
