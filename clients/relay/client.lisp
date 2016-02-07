#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen.clients.relay)

(define-client base-client (server-client)
  ((socket :initform NIL :accessor socket)))

(defmethod client-connected-p ((client base-client))
  (and (socket client)
       (usocket:socket-stream (socket client))
       (open-stream-p (usocket:socket-stream (socket client)))))

(defmethod initiate-connection ((base-client base-client))
  (deeds:with-fuzzy-slot-bindings (read-thread) (base-client base-client)
    (call-next-method)
    (unless (and read-thread (bt:thread-alive-p read-thread))
      (setf read-thread (bt:make-thread (lambda () (handle-connection base-client))))))
  client)

(defmethod close-connection ((base-client base-client))
  (deeds:with-fuzzy-slot-bindings (socket read-thread) (base-client base-client)
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
        (setf socket NIL)))))

(defmethod handle-connection :around ((base-client base-client))
  (unwind-protect
       (with-simple-restart (abort "Exit the connection handling.")
         (call-next-method))
    (close-connection base-client)))

(define-client server (base-client)
  ())

(defmethod initiate-connection ((server server))
  (deeds:with-fuzzy-slot-bindings (socket host port) (server server)
    (setf socket (usocket:socket-listen host port))))

(defmethod handle-connection ((server server))
  (loop for socket = (socket-accept (socket server))
        do (v:info :colleen.client.relay.connection "Accepting new relay client ~a" socket)
           (initiate-connection (make-instance 'client :server server :socket socket))))

(define-client client (base-client)
  ((server :initarg :server :accessor server))
  (:default-initargs :server NIL))

(defmethod initiate-connection ((client client))
  (deeds:with-fuzzy-slot-bindings (socket host port) (client client)
    (unless socket
      (setf socket (usocket:socket-connect host port)))))

(defmethod close-connection :after ((client client))
  (do-issue relay-connection-closed :client client))

(defmethod initiate-connection :after ((client client))
  (do-issue relay-connection-initiated :client client))

(defmethod handle-connection ((client client))
  (loop for message = (read-connection client)
        do (v:info :colleen.client.relay.connection "New message ~s" message)
           (loop for input-available = (nth-value 1 (usocket:wait-for-input (socket client) :timeout 1))
                 until input-available
                 do (when (and (server client) (not (client-connected-p (server client))))
                      (close-connection client)))))

(define-event relay-connection-initiated (client-event)
  ())

(define-event relay-connection-closed (client-event)
  ())

(defmethod read-connection ((client client))
  (read-line (usocket:socket-stream (socket client))))

(defmethod send-connection ((client client) message)
  (let ((stream (usocket:socket-stream (socket client))))
    (write-line message stream)
    (finish-output stream)))
