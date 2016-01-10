#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen.clients.irc)

(defvar *send-length-limit* 512)

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
   :encoding :UTF-8))

(defmethod print-object ((connection connection) stream)
  (print-unreadable-object (connection stream :type T)
    (format stream "~s ~s ~s ~a:~a"
            :name (name connection) :host (host connection) (port connection))))

(defmethod initiate-connection ((connection connection))
  (with-fuzzy-slot-bindings (nickname username password realname host port socket encoding read-thread) (connection connection)
    (with-default-encoding (encoding)
      (setf socket (usocket:socket-connect host port))
      (setf read-thread (bt:make-thread (lambda () (handle-connection connection))))
      (when password (pass password))
      (nick nickname)
      (user username 0 "*" realname)))
  connection)

(defmethod close-connection ((connection connection))
  (with-fuzzy-slot-bindings (socket read-thread) (connection connection)
    (when (open-stream-p (usocket:socket-stream socket))
      (quit))
    (when (bt:thread-alive-p read-thread)
      (bt:interrupt-thread read-thread (lambda () (invoke-restart 'abort))))
    (usocket:socket-close socket)
    (setf socket NIL)
    (setf read-thread NIL))
  connection)

(defmethod send-connection ((connection connection) format-string &rest args)
  (let ((stream (usocket:socket-stream (socket connection)))
        (message (apply #'format NIL format-string args)))
    (when (< *send-length-limit* (length (babel:string-to-octets message :encoding (encoding connection))))
      (warn 'message-too-long-warning :message message))
    (write-string message stream)
    (write-char #\Return stream)
    (write-char #\Linefeed stream)
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
                     (lambda () (handle-connection-failure connection))))
      (loop for message = (read-connection connection)
            do (when message
                 (issue (parse-event connection message) *standard-event-loop*)))))
  connection)

(defmethod handle-connection-failure ((connection connection))
  (sleep 5) ;; Eventually write a backoff
  (handler-case (close-connection connection)
    (error (err)
      (v:log :error :colleen.client.irc err)))
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
            for char = (read-char stream NIL NIL)
            do (case char
                 (#\Return
                  (when (eql (peek-char NIL stream NIL NIL) #\Newline)
                    (return)))
                 ((NIL) (return))
                 (T (write-char char out)))))))

(defmethod parse-event ((connection connection) message)
  (cl-ppcre:register-groups-bind (NIL prefix command NIL arguments) ("^(:([^ ]+) +)?([^ ]+)( +(.+))?$" message)
    (let* ((colon (search " :" arguments))
           (arguments (append (cl-ppcre:split " +" arguments :end (or colon (length arguments)))
                              (when colon (list (subseq arguments (+ 2 colon)))))))
      (v:info :colleen.client.irc ":PREFIX ~a :COMMAND ~a :ARGUMENTS ~a" prefix command arguments)
      (make-instance 'irc-event :origin connection))))
