#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen.clients.irc)

(defvar *send-length-limit* 512)
(defvar *connections* (make-hash-table :test 'equalp))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass connection ()
    ((name :initarg :name :accessor name)
     (nickname :initarg :nickname :accessor nickname)
     (username :initarg :username :accessor username)
     (password :initarg :password :accessor password)
     (realname :initarg :realname :accessor realname)
     (host :initarg :host :accessor host)
     (port :initarg :port :accessor port)
     (socket :initform NIL :accessor socket)
     (encoding :initarg :encoding :accessor encoding)
     (read-thread :initform NIL :accessor read-thread))
    (:default-initargs
     :name (error "NAME required.")
     :nickname (machine-instance)
     :username (machine-instance)
     :password NIL
     :realname (machine-instance)
     :host (error "HOST required.")
     :port 6667
     :encoding :UTF-8)
    (:metaclass deeds::cached-slots-class)))

(defmethod initialize-instance :after ((connection connection) &key)
  (when (gethash (name connection) *connections*)
    ;; Error
    )
  (setf (gethash (name connection) *connections*) connection))

(defmethod print-object ((connection connection) stream)
  (print-unreadable-object (connection stream :type T)
    (format stream "~s ~s ~s ~a:~a"
            :name (name connection) :host (host connection) (port connection))))

(defmethod initiate-connection ((connection connection))
  (deeds:with-fuzzy-slot-bindings (nickname username password realname host port socket encoding read-thread) (connection connection)
    (with-default-encoding (encoding)
      (setf socket (usocket:socket-connect host port))
      (setf read-thread (bt:make-thread (lambda () (handle-connection connection))))
      (when password (irc:pass connection password))
      (irc:nick connection nickname)
      (irc:user connection username 0 "*" realname)))
  connection)

(defmethod close-connection ((connection connection))
  (deeds:with-fuzzy-slot-bindings (socket read-thread) (connection connection)
    (when (open-stream-p (usocket:socket-stream socket))
      (irc:quit connection))
    (when (bt:thread-alive-p read-thread)
      (bt:interrupt-thread read-thread (lambda () (invoke-restart 'abort))))
    (usocket:socket-close socket)
    (setf socket NIL)
    (setf read-thread NIL))
  connection)

(defmethod send-connection ((connection connection) message)
  (let ((stream (usocket:socket-stream (socket connection)))
        (message (format NIL "~a~c~c" message #\Return #\Linefeed)))
    (when (< *send-length-limit* (length (babel:string-to-octets message :encoding (encoding connection))))
      (warn 'message-too-long-warning :message message))
    (write-string message stream)
    (finish-output stream))
  connection)

(defmethod handle-connection ((connection connection))
  (with-simple-restart (abort "Exit the connection handling.")
    (handler-bind (((or usocket:ns-try-again-condition 
                        usocket:timeout-error 
                        usocket:shutdown-error
                        usocket:connection-reset-error
                        usocket:connection-aborted-error
                        cl:end-of-file)
                     (lambda (err)
                       (v:warn :colleen.client.irc.connection err)
                       (handle-connection-failure connection))))
      (loop for message = (read-connection connection)
            for events = (when (and message (string/= message ""))
                           (parse-reply connection message))
            do (dolist (event events)
                 (deeds:issue event *event-loop*)))))
  connection)

(defmethod handle-connection-failure ((connection connection))
  (v:info :colleen.client.irc.connection "~a Closing connection due to failure." connection)
  (handler-case (close-connection connection)
    (error (err)
      (v:log :error :colleen.client.irc.connection err)))
  (sleep 5) ;; Eventually write a backoff
  (v:info :colleen.client.irc.connection "~a Attempting to recover from failure." connection)
  (initiate-connection connection))

(defmethod read-connection ((connection connection))
  (handler-bind (#+sbcl (sb-int:stream-decoding-error
                          (lambda (err)
                            (v:log :warning :colleen.clients.irc err)
                            (invoke-restart 'sb-int:attempt-resync))))
    (with-output-to-string (out)
      ;; Slow character by character copy, but since we have to do
      ;; CRLF detection portably, we cannot use READ-LINE.
      (loop with stream = (usocket:socket-stream (socket connection))
            for char = (read-char stream)
            do (case char
                 (#\Return
                  (when (eql (peek-char NIL stream NIL NIL) #\Newline)
                    (return)))
                 (T (write-char char out)))))))
