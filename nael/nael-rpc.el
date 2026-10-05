;;; nael-rpc.el --- Lean RPC sessions for Nael  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jan Mas Rovira
;; Copyright (C) 2026 Doug Torrance
;; Copyright © 2026 Klaus Kraßnitzer

;; SPDX-License-Identifier: MIT

;; This file is licensed under the MIT License, see LICENSE.MIT.

;; Derived from `lean4-rpc.el' of lean4-mode
;; <https://github.com/d-torrance/lean4-mode> at commit 0c2216dd43, which
;; is itself derived from `neo-lean-rpc.el' of neo-lean-mode
;; <https://github.com/janmasrovira/neo-lean-mode> by Jan Mas Rovira.
;; Changed for Nael: names; Eglot internals via `nael-eglot-compat';
;; keep-alive every 10 seconds; pending edits are sent before a call is
;; made; per-call timeouts, none for calls that wait for elaboration;
;; requests that can be cancelled and superseded in slots; references
;; are owned by renderings (`nael-rpc-owner') and found by a strict
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
;; file.  Hence `nael-rpc-open' returns a position-bound handle rather
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
;; (`nael-rpc-make-owner') collects the references of the results that
;; one rendering shows; releasing the owner when the rendering is
;; replaced releases them all, batched.  References of a session that
;; died are gone with it and are never released.

;;; Code:

(require 'cl-lib)
(require 'jsonrpc)

(require 'nael-eglot)

(defgroup nael-rpc nil
  "Lean RPC sessions for `nael-mode'."
  :group 'nael-eglot
  :link '(emacs-library-link :tag "Source Lisp File" "nael-rpc.el")
  :prefix "nael-rpc-")

(defcustom nael-rpc-timeout 10
  "Seconds to wait for an RPC call that does not wait for elaboration.
Calls for goals and expected types wait until the server has elaborated
the file up to their position, which can take arbitrarily long; they
have no timeout."
  :type 'number
  :group 'nael-rpc)

;;;; References:

;; A reference is a JSON object with exactly one member whose value is a
;; string: {"__rpcref": "17"} in wire format v1, {"p": "17"} in v0.  The
;; server announces the format it uses; see
;; `nael-eglot-rpc-wire-format'.

(defconst nael-rpc--ref-key-v1 :__rpcref)
(defconst nael-rpc--ref-key-v0 :p)

(defun nael-rpc--negotiated-ref-key (connection)
  "Return the plist key that CONNECTION uses for references."
  (let ((format (thread-first
                  (nael-eglot-compat-capabilities connection)
                  (plist-get :experimental)
                  (plist-get :rpcProvider)
                  (plist-get :rpcWireFormat))))
    (if (equal format "v1") nael-rpc--ref-key-v1 nael-rpc--ref-key-v0)))

(defun nael-rpc-ref-p (object key)
  "Return non-nil if OBJECT is a reference encoded with KEY."
  (and (consp object)
       (eq (car object) key)
       (consp (cdr object))
       (stringp (cadr object))
       (null (cddr object))))

(defun nael-rpc-collect-refs (object key)
  "Return the references in OBJECT, a parsed JSON value, in order.
KEY is the plist key of references.  Every occurrence counts, because
the server counts every time it serves a reference."
  (let (refs)
    (cl-labels ((walk (x)
                  (cond ((nael-rpc-ref-p x key) (push x refs))
                        ((and (consp x) (keywordp (car x)))
                         (cl-loop for (_key value) on x by #'cddr
                                  do (walk value)))
                        ((vectorp x) (mapc #'walk x)))))
      (walk object))
    (nreverse refs)))

;;;; Error codes:

;; From Lean's `Lean/Data/JsonRpc.lean'.  Each of the first four means
;; that the session no longer exists and every reference obtained
;; through it is worthless.
(defconst nael-rpc-needs-reconnect -32900)
(defconst nael-rpc-content-modified -32801)
(defconst nael-rpc-worker-exited -32901)
(defconst nael-rpc-worker-crashed -32902)

(defconst nael-rpc--dead-codes
  (list nael-rpc-needs-reconnect
        nael-rpc-content-modified
        nael-rpc-worker-exited
        nael-rpc-worker-crashed)
  "Error codes meaning that the RPC session must be re-established.")

(defun nael-rpc-dead-code-p (code)
  "Return non-nil if CODE says that the RPC session is gone."
  (and (integerp code) (memq code nael-rpc--dead-codes) t))

(defun nael-rpc-error-code (error)
  "Return the JSON-RPC code of ERROR, however jsonrpc.el wrapped it."
  (or (plist-get error :code)
      (plist-get (plist-get error :error) :code)))

;;;; Sessions:

(defconst nael-rpc-keepalive-interval 10
  "Seconds between `$/lean/rpc/keepAlive' notifications.
The server expires a session after 30 seconds of silence.  Emacs timers
do not fire during long commands, hence the margin.")

(defconst nael-rpc-release-batch-size 100
  "Number of references to accumulate before releasing them.")

(defconst nael-rpc-release-delay 0.1
  "Seconds to wait before releasing accumulated references.")

(cl-defstruct (nael-rpc-session (:constructor nael-rpc--session-create)
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

(defvar nael-rpc--sessions (make-hash-table :test #'equal)
  "Sessions, keyed by document URI.")

(defun nael-rpc-connection-live-p (connection)
  "Return non-nil if CONNECTION can still carry a request."
  (and connection (jsonrpc-running-p connection)))

(defun nael-rpc--flush-pending (session)
  "Run and clear the thunks queued on SESSION."
  (let ((thunks (nreverse (nael-rpc-session-pending session))))
    (setf (nael-rpc-session-pending session) nil)
    (mapc #'funcall thunks)))

(defun nael-rpc--keepalive (session)
  "Send a keep-alive for SESSION, or close it if its server is gone."
  (cond ((nael-rpc-session-closed session)
         (nael-rpc--stop-keepalive session))
        ((not (nael-rpc-connection-live-p
               (nael-rpc-session-connection session)))
         (nael-rpc--close session))
        ((nael-rpc-session-id session)
         (ignore-errors
           (jsonrpc-notify (nael-rpc-session-connection session)
                           :$/lean/rpc/keepAlive
                           (list :uri (nael-rpc-session-uri session)
                                 :sessionId (nael-rpc-session-id session)))))))

(defun nael-rpc--start-keepalive (session)
  "Begin sending keep-alive notifications for SESSION."
  (setf (nael-rpc-session-keepalive-timer session)
        (run-with-timer nael-rpc-keepalive-interval
                        nael-rpc-keepalive-interval
                        #'nael-rpc--keepalive session)))

(defun nael-rpc--stop-keepalive (session)
  "Cancel the keep-alive timer of SESSION."
  (when (timerp (nael-rpc-session-keepalive-timer session))
    (cancel-timer (nael-rpc-session-keepalive-timer session)))
  (setf (nael-rpc-session-keepalive-timer session) nil))

(defun nael-rpc--close (session)
  "Close SESSION: stop its timers and drop it from the registry.
References awaiting release are abandoned: the server has discarded
everything belonging to a dead session."
  (setf (nael-rpc-session-closed session) t)
  (nael-rpc--stop-keepalive session)
  (when (timerp (nael-rpc-session-release-timer session))
    (cancel-timer (nael-rpc-session-release-timer session)))
  (setf (nael-rpc-session-release-timer session) nil
        (nael-rpc-session-garbage session) nil)
  (when (eq session (gethash (nael-rpc-session-uri session)
                             nael-rpc--sessions))
    (remhash (nael-rpc-session-uri session) nael-rpc--sessions)))

(defun nael-rpc--connect (connection uri)
  "Open a session for URI on CONNECTION and register it at once.
Registering before the handshake resolves lets callers queue calls on a
session that does not exist yet, which is the common case: the first
call is what triggers the connect."
  (let ((session (nael-rpc--session-create
                  :connection connection
                  :uri uri
                  :ref-key (nael-rpc--negotiated-ref-key connection))))
    (puthash uri session nael-rpc--sessions)
    (cl-flet ((fail (error)
                (setf (nael-rpc-session-error session) error
                      (nael-rpc-session-settled session) t)
                (nael-rpc--close session)
                ;; Queued calls must fail rather than hang.
                (nael-rpc--flush-pending session)))
      ;; The handshake can signal if the server exited meanwhile.
      (condition-case error
          (jsonrpc-async-request
           connection :$/lean/rpc/connect (list :uri uri)
           :success-fn
           (lambda (result)
             (setf (nael-rpc-session-id session)
                   (plist-get result :sessionId)
                   (nael-rpc-session-settled session) t)
             (unless (nael-rpc-session-closed session)
               (nael-rpc--start-keepalive session))
             (nael-rpc--flush-pending session))
           :error-fn #'fail
           :timeout-fn
           (lambda ()
             (fail (list :code nael-rpc-worker-exited
                         :message "Lean RPC connect timed out"))))
        (error (fail (list :code nael-rpc-worker-exited
                           :message (error-message-string error))))))
    session))

(defun nael-rpc--session-for (connection uri)
  "Return a usable session for URI on CONNECTION, creating one if needed.
A session of a previous connection is discarded: after a server restart
its id means nothing."
  (let ((existing (gethash uri nael-rpc--sessions)))
    (if (and existing
             (not (nael-rpc-session-closed existing))
             (not (nael-rpc-session-error existing))
             (eq (nael-rpc-session-connection existing) connection)
             (nael-rpc-connection-live-p connection))
        existing
      (when existing (nael-rpc--close existing))
      (nael-rpc--connect connection uri))))

(defun nael-rpc-close-all (&optional connection)
  "Close every session, or only those of CONNECTION."
  (let (doomed)
    (maphash (lambda (_uri session)
               (when (or (null connection)
                         (eq (nael-rpc-session-connection session)
                             connection))
                 (push session doomed)))
             nael-rpc--sessions)
    (mapc #'nael-rpc--close doomed)))

;;;; Releasing references:

(defun nael-rpc-release (session refs)
  "Tell the server of SESSION that REFS are no longer needed.
The release is batched: a goal has a reference per subterm, and a
notification per reference would be thousands per keystroke."
  (when (and refs (not (nael-rpc-session-closed session)))
    (setf (nael-rpc-session-garbage session)
          (append refs (nael-rpc-session-garbage session)))
    (if (>= (length (nael-rpc-session-garbage session))
            nael-rpc-release-batch-size)
        (nael-rpc--flush-garbage session)
      (unless (timerp (nael-rpc-session-release-timer session))
        (setf (nael-rpc-session-release-timer session)
              (run-with-timer nael-rpc-release-delay nil
                              #'nael-rpc--flush-garbage session))))))

(defun nael-rpc--flush-garbage (session)
  "Send `$/lean/rpc/release' for the references SESSION accumulated."
  (when (timerp (nael-rpc-session-release-timer session))
    (cancel-timer (nael-rpc-session-release-timer session)))
  (setf (nael-rpc-session-release-timer session) nil)
  (let ((refs (nael-rpc-session-garbage session)))
    (setf (nael-rpc-session-garbage session) nil)
    (when (and refs
               (not (nael-rpc-session-closed session))
               (nael-rpc-session-id session))
      (ignore-errors
        (jsonrpc-notify (nael-rpc-session-connection session)
                        :$/lean/rpc/release
                        (list :uri (nael-rpc-session-uri session)
                              :sessionId (nael-rpc-session-id session)
                              :refs (vconcat refs)))))))

(cl-defstruct (nael-rpc-owner (:constructor nael-rpc-make-owner)
                              (:copier nil))
  "The references that one rendering keeps alive.
Create one owner per rendering, pass it as `:owner' to the calls whose
results the rendering shows, and release it with `nael-rpc-owner-release'
once the rendering is replaced or gone."
  session                               ; session the refs belong to
  (refs nil)
  (released nil))

(defun nael-rpc-owner-adopt (owner session result)
  "Make OWNER keep the references in RESULT, obtained through SESSION."
  (let ((refs (nael-rpc-collect-refs result
                                     (nael-rpc-session-ref-key session))))
    (cond
     ;; A late result for a rendering that is already gone.
     ((nael-rpc-owner-released owner)
      (nael-rpc-release session refs))
     (t
      ;; References of an earlier session died with that session.
      (unless (eq session (nael-rpc-owner-session owner))
        (setf (nael-rpc-owner-session owner) session
              (nael-rpc-owner-refs owner) nil))
      (setf (nael-rpc-owner-refs owner)
            (nconc refs (nael-rpc-owner-refs owner)))))))

(defun nael-rpc-owner-release (owner)
  "Release the references that OWNER keeps.  OWNER cannot be reused."
  (unless (nael-rpc-owner-released owner)
    (setf (nael-rpc-owner-released owner) t)
    (when-let* ((session (nael-rpc-owner-session owner)))
      (nael-rpc-release session (nael-rpc-owner-refs owner)))
    (setf (nael-rpc-owner-refs owner) nil)))

;;;; Handles:

(cl-defstruct (nael-rpc-handle (:constructor nael-rpc--handle-create)
                               (:copier nil))
  "A position-bound handle for making RPC calls."
  session
  position                              ; TextDocumentPositionParams
  buffer)                               ; the source buffer

(defvar-local nael-rpc--uri nil
  "URI of the RPC session of the current buffer, if any was opened.")

(defvar-local nael-rpc--slots nil
  "Alist mapping slot names to the latest request in that slot.")

(defun nael-rpc-close-buffer ()
  "Cancel the slotted calls and close the RPC session of this buffer."
  (dolist (entry nael-rpc--slots)
    (nael-rpc-cancel (cdr entry)))
  (setq nael-rpc--slots nil)
  (when-let* ((uri nael-rpc--uri)
              (session (gethash uri nael-rpc--sessions)))
    (nael-rpc--close session)))

(defun nael-rpc--managed-mode-changed ()
  "Close the RPC session of this buffer when Eglot stops managing it."
  (unless (eglot-managed-p)
    (nael-rpc-close-buffer)))

(defun nael-rpc-open (&optional pos)
  "Return an RPC handle bound to POS, defaulting to point.
Pending edits of the current buffer are sent to the server first, so
that the position refers to text that the server has seen."
  (let ((server (nael-eglot-server-or-lose)))
    (nael-eglot-flush-changes)
    (let* ((position (nael-eglot-compat-position-params pos))
           (uri (plist-get (plist-get position :textDocument) :uri)))
      (setq nael-rpc--uri uri)
      (add-hook 'kill-buffer-hook #'nael-rpc-close-buffer nil t)
      (add-hook 'eglot-managed-mode-hook
                #'nael-rpc--managed-mode-changed nil t)
      (nael-rpc--handle-create
       :session (nael-rpc--session-for server uri)
       :position position
       :buffer (current-buffer)))))

(defun nael-rpc-handle-live-p (handle)
  "Return non-nil if HANDLE can still be used."
  (and handle
       (let ((session (nael-rpc-handle-session handle)))
         (and (not (nael-rpc-session-closed session))
              (nael-rpc-connection-live-p
               (nael-rpc-session-connection session))))))

;;;; Calls:

(cl-defstruct (nael-rpc-request (:constructor nael-rpc--request-create)
                                (:copier nil))
  "An RPC call, possibly retried once on a new session."
  handle method params success failure owner timeout retry
  id                                    ; JSON-RPC id, if known
  session                               ; session it was sent on
  (cancelled nil)
  (done nil))

(defun nael-rpc--deliver-failure (request error)
  "Pass ERROR to the failure function of REQUEST, unless it is moot."
  (unless (or (nael-rpc-request-cancelled request)
              (nael-rpc-request-done request))
    (setf (nael-rpc-request-done request) t)
    (funcall (nael-rpc-request-failure request) error)))

(defun nael-rpc--succeed (request session result)
  "Deliver RESULT of REQUEST, which was answered through SESSION."
  (cond
   ((or (nael-rpc-request-cancelled request)
        (nael-rpc-request-done request))
    ;; Nobody will show this result, but the server counts its
    ;; references as served.
    (nael-rpc-release session (nael-rpc-collect-refs
                               result (nael-rpc-session-ref-key session))))
   (t
    (setf (nael-rpc-request-done request) t)
    (when-let* ((owner (nael-rpc-request-owner request)))
      (nael-rpc-owner-adopt owner session result))
    (funcall (nael-rpc-request-success request) result))))

(defun nael-rpc--fail (request session error)
  "Handle ERROR of REQUEST, which was sent through SESSION."
  (let ((code (nael-rpc-error-code error))
        (handle (nael-rpc-request-handle request)))
    (when (nael-rpc-dead-code-p code)
      (nael-rpc--close session))
    (cond
     ((or (nael-rpc-request-cancelled request)
          (nael-rpc-request-done request)))
     ((and (nael-rpc-dead-code-p code)
           (nael-rpc-request-retry request)
           (buffer-live-p (nael-rpc-handle-buffer handle)))
      (setf (nael-rpc-request-retry request) nil
            (nael-rpc-handle-session handle)
            (nael-rpc--session-for (nael-rpc-session-connection session)
                                   (nael-rpc-session-uri session)))
      (nael-rpc--attempt request))
     (t (nael-rpc--deliver-failure request error)))))

(defun nael-rpc--send (request)
  "Send REQUEST on the session of its handle, which has settled."
  (let* ((handle (nael-rpc-request-handle request))
         (session (nael-rpc-handle-session handle))
         (connection (nael-rpc-session-connection session)))
    (cond
     ((nael-rpc-request-cancelled request))
     ((nael-rpc-session-error session)
      (nael-rpc--fail request session (nael-rpc-session-error session)))
     ((nael-rpc-session-closed session)
      (nael-rpc--fail request session
                      (list :code nael-rpc-needs-reconnect
                            :message "Lean RPC session is closed")))
     ;; A server that has exited takes its sessions with it.  Report it
     ;; like a dead session instead of letting jsonrpc signal from a
     ;; timer or hook.
     ((not (nael-rpc-connection-live-p connection))
      (nael-rpc--close session)
      (nael-rpc--deliver-failure
       request (list :code nael-rpc-worker-exited
                     :message "Lean server is not running")))
     (t
      (setf (nael-rpc-request-session request) session)
      (condition-case error
          (let ((sent (jsonrpc-async-request
                       connection :$/lean/rpc/call
                       (append (nael-rpc-handle-position handle)
                               (list :sessionId (nael-rpc-session-id session)
                                     :method (nael-rpc-request-method request)
                                     :params (or (nael-rpc-request-params
                                                  request)
                                                 (make-hash-table))))
                       :success-fn
                       (lambda (result)
                         (nael-rpc--succeed request session result))
                       :error-fn
                       (lambda (error)
                         (nael-rpc--fail request session error))
                       :timeout (nael-rpc-request-timeout request)
                       :timeout-fn
                       (lambda ()
                         (nael-rpc--cancel-on-server request)
                         (nael-rpc--deliver-failure
                          request (list :code nil :timeout t
                                        :message "Lean RPC call timed out"))))))
            ;; Since jsonrpc 1.0.26, the result starts with the id.
            (setf (nael-rpc-request-id request) (car-safe sent)))
        (error
         (nael-rpc--close session)
         (nael-rpc--deliver-failure
          request (list :code nael-rpc-worker-exited
                        :message (error-message-string error)))))))))

(defun nael-rpc--attempt (request)
  "Send REQUEST now, or once the handshake of its session has settled."
  (let ((session (nael-rpc-handle-session (nael-rpc-request-handle request))))
    (if (nael-rpc-session-settled session)
        (nael-rpc--send request)
      (push (lambda () (nael-rpc--send request))
            (nael-rpc-session-pending session)))))

(defun nael-rpc--cancel-on-server (request)
  "Ask the server to stop working on REQUEST, if it was sent."
  (when-let* ((id (nael-rpc-request-id request))
              (session (nael-rpc-request-session request))
              ((not (nael-rpc-session-closed session)))
              (connection (nael-rpc-session-connection session))
              ((nael-rpc-connection-live-p connection)))
    (ignore-errors
      (jsonrpc-notify connection :$/cancelRequest (list :id id)))))

(defun nael-rpc-cancel (request)
  "Cancel REQUEST: its success and failure functions will not be called.
If REQUEST was already sent, the server is asked to stop working on it.
That needs jsonrpc 1.0.26 or later, which reports request ids."
  (unless (or (nael-rpc-request-cancelled request)
              (nael-rpc-request-done request))
    (setf (nael-rpc-request-cancelled request) t)
    (nael-rpc--cancel-on-server request)))

(cl-defun nael-rpc-call (handle method params success
                                &key failure owner slot
                                (timeout nael-rpc-timeout) (retry t))
  "Call the Lean RPC METHOD with PARAMS through HANDLE.
Return the request, which `nael-rpc-cancel' accepts.

SUCCESS receives the result.  FAILURE, if non-nil, receives the error
plist; otherwise errors are dropped, which is usually right for a
display that will be refreshed anyway.  A timeout is reported to
FAILURE as an error with a non-nil `:timeout'.

OWNER, if non-nil, keeps the references in the result; see
`nael-rpc-make-owner'.  Without OWNER, releasing them is up to the
caller.

SLOT, if non-nil, is a symbol naming the purpose of the call, such as
`goals'.  A call in a slot of the buffer of HANDLE cancels the previous
call in the same slot, so that a stale answer is never delivered.

TIMEOUT is the number of seconds to wait for an answer, or nil to wait
indefinitely; it defaults to `nael-rpc-timeout'.

A call made while the session is still connecting is queued.  If RETRY
is non-nil, as by default, a call that fails because the session died
is retried once on a new session."
  (let ((request (nael-rpc--request-create
                  :handle handle :method method :params params
                  :success success :failure (or failure #'ignore)
                  :owner owner :timeout timeout :retry retry)))
    (when slot
      (with-current-buffer (nael-rpc-handle-buffer handle)
        (when-let* ((previous (alist-get slot nael-rpc--slots)))
          (nael-rpc-cancel previous))
        (setf (alist-get slot nael-rpc--slots) request)))
    (nael-rpc--attempt request)
    request))

(defconst nael-rpc-sync-timeout 3
  "Seconds to wait for a synchronous RPC call before giving up.")

(define-error 'nael-rpc-error "Lean RPC call failed")

(defun nael-rpc-call-sync (handle method params &rest keys)
  "Call METHOD with PARAMS through HANDLE and wait for the result.
Return the result.  If the call fails or does not answer within
`nael-rpc-sync-timeout', signal `nael-rpc-error' with the method and
the error plist, as passed to the FAILURE function of `nael-rpc-call'.
Only for commands invoked by the user, where an answer is required
synchronously, such as `xref'.  KEYS are as for `nael-rpc-call', but
for SUCCESS, FAILURE and TIMEOUT.  Releasing references in the result
is up to the caller."
  (let ((deadline (+ (float-time) nael-rpc-sync-timeout))
        result failure done)
    (let ((request (apply #'nael-rpc-call handle method params
                          (lambda (value) (setq result value done t))
                          :failure (lambda (error) (setq failure error done t))
                          :timeout nael-rpc-sync-timeout
                          keys)))
      (while (and (not done) (< (float-time) deadline))
        (accept-process-output nil 0.02))
      (unless done
        (nael-rpc-cancel request)
        (setq failure (list :code nil :timeout t
                            :message "Lean RPC call timed out"))))
    (if failure
        (signal 'nael-rpc-error (list method failure))
      result)))

;;;; Methods:

;; Registered with `registerBuiltinRpcProcedure' in Lean's
;; `Lean/Server/FileWorker/WidgetRequests.lean' and
;; `Lean/Server/FileWorker/RequestHandling.lean'.

(defun nael-rpc-get-interactive-goals (handle success &rest keys)
  "Request the tactic goals at the position of HANDLE.
SUCCESS receives the goals or nil; KEYS are as for `nael-rpc-call',
whose TIMEOUT defaults to none here."
  (apply #'nael-rpc-call handle "Lean.Widget.getInteractiveGoals"
         (nael-rpc-handle-position handle) success
         (append keys (list :timeout nil))))

(defun nael-rpc-get-interactive-term-goal (handle success &rest keys)
  "Request the expected type at the position of HANDLE.
SUCCESS receives the term goal or nil; KEYS are as for `nael-rpc-call',
whose TIMEOUT defaults to none here."
  (apply #'nael-rpc-call handle "Lean.Widget.getInteractiveTermGoal"
         (nael-rpc-handle-position handle) success
         (append keys (list :timeout nil))))

(defun nael-rpc-info-to-interactive (handle info success &rest keys)
  "Request type and documentation of INFO, a subterm reference.
INFO must come from a result of the session of HANDLE.  SUCCESS
receives the result; KEYS are as for `nael-rpc-call'.  A call that
fails because the session died is not retried: INFO died with it."
  (apply #'nael-rpc-call handle
         "Lean.Widget.InteractiveDiagnostics.infoToInteractive"
         info success (append keys (list :retry nil))))

(defun nael-rpc-get-interactive-diagnostics (handle line success &rest keys)
  "Request the messages that touch LINE, 0-based, in the file of HANDLE.
The server answers at once with the messages it has, whose full range
touches LINE.  SUCCESS receives a vector of `InteractiveDiagnostic's;
KEYS are as for `nael-rpc-call'."
  (apply #'nael-rpc-call handle "Lean.Widget.getInteractiveDiagnostics"
         (list :lineRange (list :start line :end (1+ line)))
         success keys))

(defun nael-rpc-lazy-trace-children (handle children success &rest keys)
  "Request the children of a trace whose CHILDREN are a reference.
CHILDREN is the `lazy' reference of a trace in a message from the
session of HANDLE.  SUCCESS receives a vector of messages; KEYS are as
for `nael-rpc-call'.  A call that fails because the session died is
not retried."
  (apply #'nael-rpc-call handle "Lean.Widget.lazyTraceChildrenToInteractive"
         children success (append keys (list :retry nil))))

(provide 'nael-rpc)

;;; nael-rpc.el ends here
