#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen.clients.remote)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass client (server-client)
    ((socket :initform NIL :accessor socket))
    (:metaclass deeds:cached-slots-class)))

(defmethod client-connected-p ((client client))
  (and (socket client)
       (usocket:socket-stream (socket client))
       (open-stream-p (usocket:socket-stream (socket client)))))

(defmethod initiate-connection ((client client))
  (deeds:with-fuzzy-slot-bindings (host port socket spawn-thread) (client client)
    (setf socket (usocket:socket-connect host port))
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
    (handler-bind (((or usocket:ns-try-again-condition 
                        usocket:timeout-error 
                        usocket:shutdown-error
                        usocket:connection-reset-error
                        usocket:connection-aborted-error
                        cl:end-of-file)
                     (lambda (err)
                       (v:error :colleen.client.remote.connection err)
                       (handle-connection-failure client)
                       (invoke-restart 'continue))))
      (loop (with-simple-restart (continue "Continue reading messages.")
              (loop do (loop for input-available = (nth-value 1 (usocket:wait-for-input (socket client) :timeout 1))
                             until input-available)
                       (dolist (event (read-connection client))
                         (deeds:issue event *event-loop*))))))))

(defmethod read-connection ((client client))
  )
