#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen.clients.relay)

(define-consumer base-client (server-client)
  ((socket :initform NIL :accessor socket)
   (read-thread :initform NIL :accessor read-thread)))

(defmethod client-connected-p ((client base-client))
  (socket client))

(defmethod initiate-connection :around ((base-client base-client))
  (with-slots (read-thread) base-client
    (call-next-method)
    (unless (and read-thread (bt:thread-alive-p read-thread))
      (setf read-thread (bt:make-thread (lambda () (handle-connection base-client))))))
  base-client)

(defmethod close-connection ((base-client base-client))
  (with-slots (socket read-thread) base-client
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

(define-consumer server (base-client)
  ())

(defmethod initiate-connection ((server server))
  (with-slots (socket host port) server
    (setf socket (usocket:socket-listen host port))))

(defmethod handle-connection ((server server))
  (v:info :colleen.client.relay.server "Waiting for new clients...")
  (loop for socket = (usocket:socket-accept (socket server) :element-type '(unsigned-byte 8))
        do (v:info :colleen.client.relay.server "Accepting new relay client ~a" socket)
           (initiate-connection (make-instance 'client :server server :socket socket
                                                       :host (usocket:get-peer-address socket)
                                                       :port (usocket:get-peer-port socket)
                                                       :name NIL))))

(define-consumer client (base-client)
  ((server :initarg :server :accessor server)
   (socket :initarg :socket :accessor socket))
  (:default-initargs :server NIL :socket NIL))

(defmethod client-connected-p ((client client))
  (and (call-next-method)
       (usocket:socket-stream (socket client))
       (open-stream-p (usocket:socket-stream (socket client)))))

(defmethod initiate-connection ((client client))
  (with-slots (socket host port) client
    (unless socket
      (setf socket (usocket:socket-connect host port :element-type '(unsigned-byte 8))))))

(defmethod handle-connection ((client client))
  (with-simple-restart (continue "Discard the message and continue.")
    (loop for message = (read-connection client)
          do (v:info :colleen.client.relay.client "New message ~s" message)
             (loop for input-available = (nth-value 1 (usocket:wait-for-input (socket client) :timeout 1))
                   until input-available
                   do (when (and (server client) (not (client-connected-p (server client))))
                        (close-connection client))))))

(define-event relay-connection-initiated (client-event)
  ())

(define-event relay-connection-closed (client-event)
  ())

(defmethod read-connection ((client client))
  (colleen-serialize::deserialize (usocket:socket-stream (socket client))))

(defmethod send-connection ((client client) message)
  (colleen-serialize::serialize message (usocket:socket-stream (socket client))))

#|
(ql:quickload :colleen-relay)
(in-package :colleen-relay)
(defvar *server* (make-instance 'server :host "127.0.0.1" :port 2222 :name "server"))
(defvar *client* (make-instance 'client :host "127.0.0.1" :port 2222 :name "client"))
(initiate-connection *server*)
(initiate-connection *client*)
|#
