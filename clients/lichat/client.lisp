#|
 This file is a part of Maiden
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.clients.lichat)

(defclass lichat-user (simple-user)
  ())

(defun find-conversation-channel (user)
  (loop for channel in (channels user)
        when (and (anonymous-p channel)
                  (every (lambda (u)
                           (or (matches u user)
                               (matches u (username (client user)))))
                         (users channel)))
        do (return channel)))

(defun make-anonymous-channel (client &rest users)
  (let ((message (make-instance 'lichat-cmd:create :channel NIL :from (username client))))
    (with-awaiting (client lichat-rpl:join) (ev channel)
        (send message client)
      :filter `(= id ,(slot-value message 'id))
      :timeout 5
      (dolist (user users)
        (lichat-cmd:pull client user channel))
      channel)))

(defmethod reply ((user lichat-user) message &rest args)
  (let ((channel (or (find-conversation-channel user)
                     (make-anonymous-channel (client user) user))))
    (lichat-cmd:message (client user) channel (apply #'format NIL message args))))

(defclass lichat-channel (simple-channel)
  ())

(defmethod anonymous-p ((channel lichat-channel))
  (char= #\@ (char (name channel) 0)))

(defmethod reply ((channel lichat-channel) message &rest args)
  (lichat-cmd:message (client channel) channel (apply #'format NIL message args)))

(define-consumer lichat-client (text-tcp-client reconnecting-client timeout-client simple-user-channel-client)
  ((servername :initform NIL :accessor servername)
   (username :initarg :username :accessor username)
   (password :initarg :password :accessor password))
  (:default-initargs
   :username "Maiden"
   :password NIL
   :port 1111))

(defmethod initialize-instance :after ((client lichat-client) &key channels)
  (dolist (channel channels)
    (setf (find-channel channel client)
          (make-instance 'lichat-channel :client client :name channel)))
  (unless (name client)
    (setf (name client) (host client))))

(defmethod initiate-connection :after ((client lichat-client))
  (with-awaiting (client lichat-rpl:update) (ev)
      (lichat-cmd:connect client (password client))
    :timeout 30
    (cond ((typep ev 'lichat-rpl:connect)
           (setf (servername client) (name (slot-value ev 'user))))
          (T (error "Failed to connect: ~a" ev)))))

(defmethod close-connection :before ((client lichat-client))
  (when (client-connected-p client)
    ;; Can't use dispatch mechanism here since the connection will be
    ;; gone once our handler picks the event up for sending.
    (ignore-errors
     (send (make-instance 'lichat-cmd:disconnect :from (username client)) client))))

(defmethod handle-connection :around ((client lichat-client))
  (with-simple-restart (abort "Exit the connection handling.")
    (handler-bind ((lichat-protocol:wire-condition
                     (lambda (err)
                       (v:error :maiden.client.lichat.connection "Parse error: ~a" err)
                       (invoke-restart 'continue))))
      (call-next-method))))

(defmethod process ((update update) (client lichat-client))
  (dolist (core (cores client))
    (issue update core)))

(defmethod send ((list list) (client lichat-client))
  (dolist (item list list)
    (send item client)))

(defmethod send ((object update) (client lichat-client))
  (let ((stream (usocket:socket-stream (socket client))))
    (print-event object stream)
    (finish-output stream)))

(defmethod receive ((client lichat-client))
  (let ((update (parse-event client (usocket:socket-stream (socket client)))))
    (v:trace :maiden.client.lichat.connection "Received update: ~a" update)
    update))

(defmethod handle-connection-idle ((client lichat-client)))

(defmethod authenticate ((user lichat-user) (client lichat-client))
  (let ((message (make-instance 'lichat-rpl:user-info
                                :client client
                                :from (username client)
                                :target (name user))))
    (with-awaiting (client lichat-rpl:user-info) (ev registered)
        (send message client)
      :filter `(equal id ,(id message))
      :timeout 2
      registered)))

;; FIXME: Currently we are very "trusting" in that we hope the server won't
;;        deliver any malicious or bogus events from channels or users we
;;        cannot know about. Such messages would confuse this system and
;;        potentially lead to a memory exhaustion.
(defmethod ensure-user ((name string) (client lichat-client))
  (or (find-user name client)
      (setf (find-user name client)
            (make-instance 'lichat-user :name name :client client))))

(defmethod ensure-channel ((name string) (client lichat-client))
  (or (find-channel name client)
      (setf (find-channel name client)
            (make-instance 'lichat-channel :name name :client client))))

(define-handler (lichat-client handle-init lichat-rpl:connect) (client ev)
  :match-consumer 'client
  (loop for channel being the hash-keys of (channel-map client)
        do (lichat-cmd:join client channel)))

(define-handler (lichat-client send lichat-cmd:update) (client ev)
  :match-consumer 'client
  (send ev client))

(define-handler (lichat-client pong lichat-rpl:ping) (client ev)
  (lichat-cmd:pong client))

(define-handler (lichat-client track-join lichat-rpl:join) (client ev channel user)
  :match-consumer 'client
  (cond ((matches user (username client))
         (setf (gethash (name channel) (channel-map client)) channel))
        (T
         (setf (find-channel (name channel) user) channel)
         (setf (find-user (name user) channel) user))))

(define-handler (lichat-client track-leave lichat-rpl:leave) (client ev channel user)
  :match-consumer 'client
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
