#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen.clients.relay)

(define-consumer relay (tcp-server agent)
  ((network :initform NIL :accessor network)
   (watchers :initform NIL :accessor watchers)))

(defmethod make-tcp-server-client ((server tcp-server) socket)
  (make-instance 'relay-client
                 :server server :socket socket
                 :host (usocket:get-peer-address socket)
                 :port (usocket:get-peer-port socket)
                 :name NIL))

(defmethod relay ((event client-event) (relay realy))
  (let* ((target (find-target (client event) (network relay)))
         (core (find target (cores relay) :key #'name))
         (client (find target (clients relay) :key #'remote)))
    (when core
      (deeds:with-immutable-slots-unlocked ()
        (setf (client event) (consumer target core))))
    (cond (core (issue event core))
          (client (issue event client))
          (T (error "Don't know")))))

(defmethod relay ((transport transport) (relay relay))
  (let* ((target (find-target (target event) (network relay)))
         (core (find target (cores relay) :test #'matches))
         (client (find target (clients relay) :key #'remote)))
    (cond (core (issue (event transport) core))
          (client (issue transport client))
          (T (error "Don't know.")))))

(define-handler (relay relay event) (relay event)
  (loop for (test target) in (watchers relay)
        do (when (watching event test)
             (relay event relay))))

(define-consumer relay-client (tcp-server-client)
  ())

(defmethod ping ((client relay-client))
  (send :ping client))

(defmethod process (message (client relay-client))
  (typecase message
    ((eql :ping))
    (T
     (relay message (server client)))))

(defmethod receive ((client relay-client))
  (colleen-serialize::deserialize (usocket:socket-stream (socket client))))

(defmethod send (message (client relay-client))
  (colleen-serialize::serialize message (usocket:socket-stream (socket client))))

(defmethod issue (thing (client relay-client))
  (send thing client))

(defmethod initiate-connection :after ((client relay-client))
  (broadcast 'connection-initiated :client client))

(defmethod close-connection :after ((client relay-client))
  (broadcast 'connection-closed :client client))

(define-event connection-initiated (client-event)
  ())

(define-event connection-closed (client-event)
  ())

(define-event reachable-clients (client-event)
  ((clients :initarg :clients :accessor clients)))

(define-event unreachable-clients (client-event)
  ((clients :initarg :clients :accessor clients)))

(defclass transport ()
  ((event :initarg :event :accessor event)
   (target :initarg :target :accessor target)))

#|
(ql:quickload :colleen-relay)
(in-package :colleen-relay)
(defvar *relay* (make-instance 'relay :host "127.0.0.1" :port 2222 :name "relay"))
(defvar *client* (make-instance 'client :host "127.0.0.1" :port 2222 :name "client"))
(initiate-connection *relay*)
(initiate-connection *client*)
|#
