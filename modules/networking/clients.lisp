#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.modules.networking)

(defgeneric client-connected-p (client))
(defgeneric close-connection (client))
(defgeneric initiate-connection (client))
(defgeneric handle-connection (client))
(defgeneric handle-connection-error (err client))
(defgeneric handle-connection-idle (tcp-client))
(defgeneric process (message tcp-client))
(defgeneric send (thing tcp-client))
(defgeneric receive (tcp-client))
(defgeneric accept (socket tcp-server))
(defgeneric make-tcp-server-client (server socket))

(define-consumer remote-client (client)
  ())

(defmethod print-object ((client remote-client) stream)
  (print-unreadable-object (client stream :type T)
    (format stream "~:[~a~;~:*~a~*~]~@[ ~s~]" (name client) (id client) (when (client-connected-p client) :connected))))

(defmethod start :after ((client remote-client))
  (unless (client-connected-p client)
    (initiate-connection client)))

(defmethod stop :before ((client remote-client))
  (when (client-connected-p client)
    (close-connection client)))

(defmethod initiate-connection :around ((client remote-client))
  (let (connection-successful)
    (unwind-protect
         (prog1 (call-next-method)
           (setf connection-successful T))
      (unless connection-successful
        (ignore-errors
         (close-connection client))))))

(defmethod initiate-connection :after ((client remote-client))
  (broadcast (cores client) 'connection-initiated :client client))

(defmethod close-connection :after ((client remote-client))
  (broadcast (cores client) 'connection-closed :client client))

(define-consumer ip-client (remote-client)
  ((host :initarg :host :accessor host)
   (port :initarg :port :accessor port))
  (:default-initargs
   :host (error "HOST required.")))

(defmethod print-object ((client ip-client) stream)
  (print-unreadable-object (client stream :type T)
    (format stream "~:[~a~;~:*~a~*~] ~@[~s ~]~s ~a:~a"
            (name client) (id client) (when (client-connected-p client) :connected) :host (host client) (port client))))

(define-consumer socket-client (ip-client)
  ((socket :initform NIL :accessor socket)
   (read-thread :initform NIL :accessor read-thread)))

(defmethod client-connected-p ((client socket-client))
  (socket client))

(defmethod initiate-connection :after ((client socket-client))
  (with-slots (read-thread) client
    (unless (and read-thread (bt:thread-alive-p read-thread))
      (setf read-thread (bt:make-thread (lambda () (handle-connection client)))))))

(defmethod close-connection :around ((client socket-client))
  (handler-bind ((error (lambda (err)
                          (warn 'client-connection-closed-uncleanly-warning :client client :closing-error err)
                          (when (find-restart 'continue)
                            (continue)))))
    (call-next-method)))

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
        (ignore-errors (usocket:socket-close socket))
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
                           client-timeout-error)
                        (lambda (err)
                          (handle-connection-error err client))))
         (with-retry-restart (continue "Retry handling the connection.")
           (with-simple-restart (abort "Exit the connection handling.")
             (call-next-method))))
    (close-connection client)))

(defmethod receive :around ((client socket-client))
  (bt:with-recursive-lock-held ((lock client))
    (call-next-method)))

(defmethod send :around (message (client socket-client))
  (bt:with-recursive-lock-held ((lock client))
    (call-next-method)))

(define-consumer reconnecting-client (socket-client)
  ((failures :initform 0 :accessor failures)
   (max-failures :initarg :max-failures :accessor max-failures)
   (backoff :initarg :backoff :accessor backoff)
   (interval :initarg :interval :accessor interval))
  (:default-initargs
   :max-failures 6
   :backoff :exponential
   :interval 2))

(defmethod handle-connection-error (err (client reconnecting-client))
  (v:log :debug :maiden.client.reconnection err)
  (v:warn :maiden.client.reconnection "~a Encountered a connection error. Attempting to reconnect..." client)
  (cond ((eq (bt:current-thread) (read-thread client))
         ;; We cannot call CLOSE-CONNECTION as it would end
         ;; our own thread with the restart invocation.
         ;; We don't care if it fails to close gracefully.
         (ignore-errors (usocket:socket-close (socket client)))
         (setf (socket client) NIL)
         (broadcast (cores client) 'connection-closed :client client))
        (T
         (close-connection client)))
  (loop
    (when (< (max-failures client) (failures client))
      (error 'client-reconnection-exceeded-error :client client))
    (incf (failures client))
    (sleep (ecase (backoff client)
             (:constant (interval client))
             (:linear (* (interval client) (failures client)))
             (:exponential (expt (interval client) (failures client)))))
    (handler-case
        (progn (initiate-connection client)
               (setf (failures client) 0)
               (continue))
      (error (err)
        (v:error :maiden.client.reconnection "~a Failed to reconnect: ~a" client err)
        NIL))))

(define-consumer timeout-client (remote-client)
  ((timeout :initarg :timeout :accessor timeout)
   (last-received-time :initform NIL :accessor last-received-time))
  (:default-initargs
   :timeout 120))

(defmethod receive :after ((client timeout-client))
  (setf (last-received-time client) (get-universal-time)))

(defmethod handle-connection-idle :before ((client timeout-client))
  (when (and (last-received-time client) (< (timeout client) (- (get-universal-time) (last-received-time client))))
    (error 'client-timeout-error :timeout (- (get-universal-time) (last-received-time client)) :client client)))

(define-consumer text-client (socket-client)
  ((encoding :initarg :encoding :accessor encoding)
   (buffer :initarg :buffer :accessor buffer))
  (:default-initargs
   :encoding :utf-8
   :buffer :line))

(defmethod initiate-connection :around ((client text-client))
  (with-default-encoding ((encoding client))
    (call-next-method)))

(defmethod receive :around ((client text-client))
  (handler-bind (#+sbcl (sb-int:stream-decoding-error
                          (lambda (err)
                            (v:log :warning :maiden.client.receive err)
                            (invoke-restart 'sb-int:attempt-resync))))
    (call-next-method)))

(defmethod receive ((client text-client))
  (etypecase (buffer client)
    ((eql :line) (read-line (usocket:socket-stream (socket client))))
    (string (read-sequence (buffer client) (usocket:socket-stream (socket client))))))

(defmethod send ((message string) (client text-client))
  (let ((stream (usocket:socket-stream (socket client))))
    (write-string message stream)
    (finish-output stream)))

(define-consumer tcp-client (socket-client)
  ((element-type :initform '(unsigned-byte 8) :reader element-type)
   (idle-interval :initarg :idle-interval :accessor idle-interval))
  (:default-initargs
   :idle-interval 10))

(defmethod client-connected-p ((client tcp-client))
  (and (call-next-method)
       (usocket:socket-stream (socket client))
       (open-stream-p (usocket:socket-stream (socket client)))))

(defmethod initiate-connection ((client tcp-client))
  (with-slots (socket host port) client
    (unless socket
      (setf socket (usocket:socket-connect host port :element-type (element-type client))))))

(defmethod handle-connection ((client tcp-client))
  (with-retry-restart (continue "Discard the message and continue.")
    (loop (loop until (nth-value 1 (usocket:wait-for-input (socket client) :timeout (idle-interval client)))
                do (handle-connection-idle client))
          (process (receive client) client))))

(defmethod handle-connection-idle ((client tcp-client))
  client)

(define-consumer text-tcp-client (text-client tcp-client)
  ((element-type :initform 'character :reader element-type)))

(define-consumer tcp-server (socket-client)
  ((clients :initform () :accessor clients)))

(defmethod (setf clients) :around (clients (server tcp-server))
  (bt:with-recursive-lock-held ((lock server))
    (call-next-method)))

(defmethod initiate-connection ((server tcp-server))
  (with-slots (socket host port) server
    (setf socket (usocket:socket-listen host port))))

(defmethod close-connection :after ((server tcp-server))
  (loop for client = (car (clients server))
        while client
        do (close-connection client)
           (loop while (client-connected-p client)
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
  (bt:with-lock-held ((lock (server client)))
    (setf (clients (server client)) (remove client (clients (server client))))))
