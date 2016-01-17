#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen.clients.relay)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass server (server-client)
    ((socket :initform NIL :accessor socket))
    (:metaclass deeds:cached-slots-class))
  
  (defclass client (server-client)
    ((socket :initform NIL :accessor socket))
    (:metaclass deeds:cached-slots-class)))

(defmethod client-connected-p ((client client))
  (and (socket client)
       (usocket:socket-stream (socket client))
       (open-stream-p (usocket:socket-stream (socket client)))))

(defmethod initiate-connection ((client client))
  (deeds:with-fuzzy-slot-bindings (host port socket spawn-thread) (client client)
    (setf socket (usocket:socket-listen host port))
    (unless (and spawn-thread (bt:thread-alive-p spawn-thread))
      (setf spawn-thread (bt:make-thread (lambda () (handle-connection client))))))
  client)

(defmethod close-connection ((client client))
  (deeds:with-fuzzy-slot-bindings (socket spawn-thread) (client client)
    (when (and spawn-thread (bt:thread-alive-p spawn-thread) (not (eql (bt:current-thread) spawn-thread)))
      (bt:interrupt-thread spawn-thread (lambda () (invoke-restart 'abort)))
      (setf spawn-thread NIL))
    (deeds:stop (deeds:handler 'spawner *event-loop*))
    (usocket:socket-close socket)
    (setf socket NIL)))

(defmethod handle-connection ((client client))
  (with-simple-restart (abort "Exit the connection handling.")
    (loop for socket = (socket-accept (socket client))
          do (do-issue new-relay-client :client client :socket socket))))

(define-event new-relay-client (client-event)
  ((socket :initarg :socket :reader socket))
  (:default-initargs
   :socket (error "SOCKET required.")))

(define-handler (spawner new-relay-client) (ev socket)
  :class 'deeds:parallel-handler
  (unwind-protect
       (loop for message = (read-connection client)
             for event = (when (and message (string/= message ""))
                            (parse-event client message))
             do (deeds:issue event *event-loop*)
                (loop for input-available = (nth-value 1 (usocket:wait-for-input (socket client) :timeout 1))
                      until input-available
                      do (ping-connection client)))
    (usocket:socket-close socket)))
