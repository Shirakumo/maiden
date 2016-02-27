#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(defgeneric authenticate (sender client))
(defgeneric client-connected-p (client))
(defgeneric close-connection (client))
(defgeneric initiate-connection (client))
(defgeneric handle-connection (client))
(defgeneric handle-connection-failure (err client))
(defgeneric handle-connection-idle (tcp-client))
(defgeneric process (message tcp-client))
(defgeneric send (thing tcp-client))
(defgeneric receive (tcp-client))
(defgeneric accept (socket tcp-server))
(defgeneric make-tcp-server-client (server socket))

(define-consumer client () ())

(define-consumer user-client (client)
  ())

(defmethod authenticate (sender (client user-client))
  NIL)

(define-consumer remote-client (client)
  ())

(defmethod print-object ((client remote-client) stream)
  (print-unreadable-object (client stream :type T)
    (format stream "~a~@[ ~s~]" (name client) (when (client-connected-p client) :connected))))

(defmethod start :after ((client remote-client))
  (unless (client-connected-p client)
    (initiate-connection client)))

(defmethod stop :before ((client remote-client))
  (when (client-connected-p client)
    (close-connection client)))

(define-consumer ip-client (remote-client)
  ((host :initarg :host :accessor host)
   (port :initarg :port :accessor port))
  (:default-initargs
   :host (error "HOST required.")))

(defmethod print-object ((client ip-client) stream)
  (print-unreadable-object (client stream :type T)
    (format stream "~@[~a~]~@[ ~s~] ~s ~a:~a"
            (name client) (when (client-connected-p client) :connected) :host (host client) (port client))))

(define-consumer socket-client (ip-client)
  ((socket :initform NIL :accessor socket)
   (read-thread :initform NIL :accessor read-thread)
   (lock :initform (bt:make-lock) :accessor lock)))

(defmethod client-connected-p ((client socket-client))
  (socket client))

(defmethod initiate-connection :around ((client socket-client))
  (with-slots (read-thread) client
    (call-next-method)
    (unless (and read-thread (bt:thread-alive-p read-thread))
      (setf read-thread (bt:make-thread (lambda () (handle-connection client))))))
  client)

(defmethod close-connection ((client socket-client))
  (with-slots (socket read-thread) client
    (unwind-protect
         (when (and read-thread (bt:thread-alive-p read-thread))
           (cond ((eql (bt:current-thread) read-thread)
                  (when (find-restart 'abort)
                    (abort)))
                 (T
                  (bt:interrupt-thread read-thread (lambda () (invoke-restart 'abort)))
                  (setf read-thread NIL))))
      (when socket
        (usocket:socket-close socket)
        (setf socket NIL))))
  client)

(defmethod handle-connection :around ((client client))
  (unwind-protect
       (handler-bind (((or usocket:ns-try-again-condition 
                           usocket:timeout-error 
                           usocket:shutdown-error
                           usocket:connection-reset-error
                           usocket:connection-aborted-error
                           cl:end-of-file
                           colleen:client-timeout-error)
                        (lambda (err)
                           (handle-connection-failure err client))))
         (with-simple-restart (abort "Exit the connection handling.")
           (call-next-method)))
    (close-connection client)))

(defmethod receive :around ((client socket-client))
  (bt:with-lock-held ((lock client))
    (call-next-method)))

(defmethod send :around (message (client socket-client))
  (bt:with-lock-held ((lock client))
    (call-next-method)))

(define-consumer reconnecting-client (remote-client)
  ((failures :initform 0 :accessor failures)
   (max-failures :initarg :max-failures :accessor max-failures)
   (backoff :initarg :backoff :accessor backoff)
   (interval :initarg :interval :accessor interval))
  (:default-initargs
   :max-failures 6
   :backoff :exponential
   :interval 2))

(defmethod handle-connection-failure (err (client reconnecting-client))
  (handler-case (close-connection client)
    (error (err)
      (v:log :error :colleen.client.reconnection err)))
  (loop
    (when (< (max-failures client) (failures client))
      (error 'client-reconnection-exceeded-error :client client))
    (incf (failures client))
    (sleep (ecase (backoff client)
             (:constant (interval client))
             (:linear (* (interval client) (failures client)))
             (:exponential (expt (interval client) (failures client)))))
    (when (handler-case (initiate-connection client)
            (error (err)
              (v:log :error :colleen.client.reconnection err)
              NIL))
      (setf (failures client) 0)
      (return))))

(define-consumer ping-client (remote-client)
  ((ping-interval :initarg :ping-interval :accessor ping-interval)
   (ping-timeout :initarg :ping-timeout :accessor ping-timeout)
   (pong-time :initform NIL :accessor pong-time))
  (:default-initargs
   :ping-interval 5
   :ping-timeout 120))

(defmethod handle-connection-idle :before ((client ping-client))
  (when (and (pong-time client) (< (ping-timeout client) (- (get-universal-time) (pong-time client))))
    (error 'client-timeout-error :timeout (- (get-universal-time) (pong-time client)) :client client)))

(define-consumer text-connection-client (socket-client)
  ((encoding :initarg :encoding :accessor encoding)
   (buffer :initarg :buffer :accessor buffer))
  (:default-initargs
   :encoding :utf-8
   :buffer :line))

(defmethod initiate-connection :around ((client text-connection-client))
  (with-default-encoding ((encoding client))
    (call-next-method)))

(defmethod receive ((client text-connection-client))
  (etypecase (buffer client)
    ((eql :line) (read-line (usocket:socket-stream (socket client))))
    (string (read-sequence (buffer client) (usocket:socket-stream (socket client))))))

(defmethod send ((message string) (client text-connection-client))
  (write-string message (usocket:socket-stream (socket client))))

(define-consumer tcp-client (socket-client)
  ())

(defmethod client-connected-p ((client tcp-client))
  (and (call-next-method)
       (usocket:socket-stream (socket client))
       (open-stream-p (usocket:socket-stream (socket client)))))

(defmethod initiate-connection ((client tcp-client))
  (with-slots (socket host port) client
    (unless socket
      (setf socket (usocket:socket-connect host port :element-type '(unsigned-byte 8))))))

(defmethod handle-connection ((client tcp-client))
  (with-simple-restart (continue "Discard the message and continue.")
    (loop for message = (receive client)
          do (process message client)
             (loop for input-available = (nth-value 1 (usocket:wait-for-input (socket client) :timeout 5))
                   until input-available
                   do (handle-connection-idle client)))))

(defmethod handle-connection-idle ((client tcp-client))
  client)

(define-consumer tcp-server (socket-client)
  ((clients :initform () :accessor clients)))

(defmethod (setf clients) :around (clients (server tcp-server))
  (bt:with-lock-held ((lock server))
    (call-next-method)))

(defmethod initiate-connection ((server tcp-server))
  (with-slots (socket host port) server
    (setf socket (usocket:socket-listen host port))))

(defmethod close-connection :after ((server tcp-server))
  (loop for client = (car (clients server))
        while client
        do (loop while (client-connected-p client)
                 do (sleep 0.1))
           (pop (clients server))))

(defmethod handle-connection ((server tcp-server))
  (loop for socket = (usocket:socket-accept (socket server) :element-type '(unsigned-byte 8))
        do (accept socket server)))

(defmethod accept (socket (server tcp-server))
  (initiate-connection (make-tcp-server-client server socket)))

(defmethod make-tcp-server-client ((server tcp-server) socket)
  (make-instance 'tcp-server-client
                 :server server :socket socket
                 :host (usocket:get-peer-address socket)
                 :port (usocket:get-peer-port socket)
                 :name NIL))

(define-consumer tcp-server-client (tcp-client)
  ((server :initarg :server :accessor server)
   (socket :initarg :socket :accessor socket))
  (:default-initargs
   :server (error "SERVER required.")))

(defmethod handle-connection-error (err (client tcp-server-client))
  (abort))

(defmethod initialize-instance :after ((client tcp-server-client) &key)
  (push client (clients (server client))))

(defmethod close-connection :after ((client tcp-server-client))
  ())

(defmethod handle-connection-idle :before ((client tcp-server-client))
  (unless (client-connected-p (server client))
    (close-connection client)))
