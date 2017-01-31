#|
 This file is a part of Maiden
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.clients.lichat)

(defclass lichat-user (simple-user)
  ())

(defmethod reply ((user lichat-user) message &rest args)
  (lichat-cmd:message (client user) (name user) (apply #'format NIL message args)))

(defclass lichat-channel (simple-channel)
  ())

(defmethod reply ((channel lichat-channel) message &rest args)
  (lichat-cmd:message (client channel) (name channel) (apply #'format NIL message args)))

(defclass lichat-client (tcp-client reconnecting-client timeout-client simple-user-channel-client)
  ((username :initarg :username :accessor username)
   (password :initarg :password :accessor password))
  (:default-initargs
   :username "Maiden"
   :password NIL))

(defmethod initialize-instance :after ((client lichat-client) &key channels)
  (dolist (channel channels)
    (setf (find-channel channel client)
          (make-instance 'lichat-channel :client client :name channel)))
  (unless (name client)
    (setf (name client) (host client))))

(defmethod initiate-connection :after ((client lichat-client))
  (lichat:connect client (password client)))

(defmethod close-connection :before ((client lichat-client))
  (when (client-connected-p client)
    (lichat:disconnect client)))

(defmethod handle-connection :around ((client lichat-client))
  (with-simple-restart (abort "Exit the connection handling.")
    (handler-bind ((lichat-protocol:wire-condition
                     (lambda (err)
                       (v:error :maiden.client.lichat.connection "Parse error: ~a" err)
                       (invoke-restart 'continue))))
      (call-next-method))))

(defmethod process ((object update) (client lichat-client))
  (dolist (core (cores client))
    (issue event core)))

(defmethod send ((list list) (client lichat-client))
  (dolist (item list list)
    (send item client)))

(defmethod send ((object lichat-protocol:update) (client lichat-client))
  (print-event object (usocket:socket-stream (socket client))))

(defmethod receive ((client lichat-client))
  (parse-event (usocket:socket-stream (socket client))))

(defmethod handle-connection-idle ((client lichat-client)))

(defmethod authenticate ((user lichat-user) (client lichat-client))
  (let ((message (make-instance 'lichat-rpl:user-info
                                :client client
                                :from (username client)
                                :target (name user))))
    (with-awaiting (lichat-rpl:user-info ev registered)
        ((first (cores client))
         :filter `(equal id ,(id message))
         :timeout 2)
        (send message client)
      registered)))

(defmethod ensure-user ((name string) (client lichat-client))
  (or (find-user name client)
      (setf (find-user name client)
            (make-instance 'lichat-user :name name :client client))))

(defmethod ensure-channel ((name string) (client lichat-client))
  (or (find-channel name client)
      (setf (find-channel name client)
            (make-instance 'lichat-channel :name name :client client))))

(define-handler (lichat-client handle-init lichat-rpl:connect) (c ev)
  (loop for channel being the hash-keys of (channel-map client)
        do (lichat:join c channel)))

(define-handler (lichat-client send (and update active-event)) (c ev)
  (send ev c))

(define-handler (lichat-client track-join lichat-rpl:join) (client ev channel user)
  :match-consumer 'client
  :class deeds:locally-blocking-handler
  (cond ((matches user (username client))
         (setf (gethash (name channel) (channel-map client)) channel))
        (T
         (setf (find-channel (name channel) user) channel)
         (setf (find-user (name user) channel) user))))

(define-handler (lichat-client track-leave irc:msg-part) (client ev channel user)
  :match-consumer 'client
  :class deeds:locally-blocking-handler
  (cond ((matches user (username client))
         (remove-channel channel client))
        (T
         (remove-user (name user) channel)))
  ;; Prune empty
  (loop for user being the hash-values of (user-map client)
        do (when (= 0 (hash-table-count (channel-map client)))
             (remove-user user client)))
  (loop for channel being the hash-values of (channel-map client)
        do (when (= 0 (hash-table-count (user-map client)))
             (remove-user user client))))
