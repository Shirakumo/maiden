#|
 This file is a part of Maiden
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.clients.twitter)

(define-consumer twitter-client (simple-user-channel-client)
  ((user :initform NIL :accessor user)
   (running-p :initform NIL :accessor running-p)
   (read-thread :initform NIL :accessor read-thread)
   (api-key :initarg :api-key :accessor api-key)
   (api-secret :initarg :api-secret :accessor api-secret)
   (access-token :initarg :access-token :accessor access-token)
   (access-secret :initarg :access-secret :accessor access-secret))
  (:default-initargs
   :api-key "D1pMCK17gI10bQ6orBPS0w"
   :api-secret "BfkvKNRRMoBPkEtDYAAOPW4s2G9U8Z7u3KAf0dBUA"
   :access-token NIL
   :access-secret NIL))

(defmacro with-chirp ((client) &body body)
  (let ((clientg (gensym "CLIENT")))
    `(let* ((,clientg ,client)
            (chirp:*oauth-api-key* (api-key ,clientg))
            (chirp:*oauth-api-secret* (api-secret ,clientg))
            (chirp:*oauth-access-token* (access-token ,clientg))
            (chirp:*oauth-access-secret* (access-secret ,clientg)))
       ,@body)))

(defmethod login ((client twitter-client) &key api-key api-secret)
  (when api-key (setf (api-key client) api-key))
  (when api-secret (setf (api-secret client) api-secret))
  (with-chirp (client)
    (format *query-io* "~&
Please visit
  ~a
and enter the PIN:
> " (chirp:initiate-authentication))
    (chirp:complete-authentication (read-line *query-io*))
    (setf (access-token client) chirp:*oauth-access-token*)
    (setf (access-secret client) chirp:*oauth-access-secret*)
    client))

(defmethod username ((client twitter-client))
  (chirp:screen-name (user client)))

(defmethod ensure-user ((name string) (client twitter-client))
  (or (find-user name client)
      (ensure-user (chirp:users/lookup :screen-name name) client)))

(defmethod ensure-user ((user chirp:user) (client twitter-client))
  (setf (find-user (chirp:screen-name user) client)
        (change-class user 'user :id (princ-to-string (uuid:make-v4-uuid))
                                 :name (chirp:screen-name user)
                                 :client client)))

(defmethod ensure-channel ((name string) (client twitter-client))
  (or (find-channel name client)
      (make-instance 'channel :name name :client client)))

(defmethod ensure-channel ((user chirp:user) (client twitter-client))
  (setf (find-channel (chirp:screen-name user) client)
        (make-instance 'channel :name (chirp:screen-name user) :client client)))

(defmethod start :before ((client twitter-client))
  (with-chirp (client)
    (setf (user client) (ensure-user (chirp:account/verify-credentials) client))))

(defmethod start :after ((client twitter-client))
  (setf (running-p client) T)
  (setf (read-thread client) (deeds::make-thread
                              (lambda ()
                                (unwind-protect
                                     (handle-connection client)
                                  (setf (read-thread client) NIL)))
                              (princ-to-string client))))

(defmethod stop :after ((client twitter-client))
  (setf (running-p client) NIL)
  (when (eql (bt:current-thread) (read-thread client))
    (invoke-restart 'exit-handling))
  (loop repeat 100
        do (unless (read-thread client) (return))
           (sleep 0.01)
        finally (bt:interrupt-thread (read-thread client)
                                     (lambda () (invoke-restart 'exit-handling)))))

(defmethod handle-connection :around ((client twitter-client))
  (with-chirp (client)
    (call-next-method)))

(defmethod handle-connection ((client twitter-client))
  (with-simple-restart (exit-handling "Exit the handling.")
    (loop while (running-p client)
          do (chirp:stream/user (lambda (update)
                                  (process update client)
                                  (running-p client))))))

(defmethod process (thing (client twitter-client)))

(defmethod process :around (thing (client twitter-client))
  (with-simple-restart (continue "Ignore the update.")
    (handler-bind ((error (lambda (err)
                            (maybe-invoke-debugger err 'continue))))
      (call-next-method))))

(define-event send-event (client-event active-event)
  ())

(defmethod send :around (update (client twitter-client))
  (with-chirp (client)
    (call-next-method)))

(define-handler (twitter-client sender send-event) (client ev)
  :match-consumer 'client
  (send ev client))

(defclass user (simple-user chirp:user)
  ())

(defmethod authenticate ((user user) client)
  T)

(defmethod ensure-user ((user user) (client twitter-client))
  user)

(defclass channel (simple-channel)
  ())

(defmethod reply ((channel channel) fmt &rest args)
  (do-issue (first (cores (client channel))) update-channel
    :client (client channel)
    :user (user (client channel))
    :channel channel
    :message (apply #'format NIL fmt args)))

(defmethod ensure-channel ((channel channel) (client twitter-client))
  channel)

(define-event update-channel (message-event channel-event send-event)
  ((message :initarg :message :accessor message :mutable T)))

(defmethod send ((update update-channel) client)
  (chirp:direct-messages/new (message update)
                             :screen-name (name (maiden-client-entities:channel update))))

(define-event update-status (message-event send-event)
  ((message :initarg :message :accessor message :mutable T)
   (reply-to :initarg :reply-to :reader reply-to)))

(defmethod send ((update update-status) client)
  (chirp:reply (reply-to update) (message update)))

(define-event status (message-event passive-event)
  ((object :initarg :object :reader object)))

(defmethod process ((status chirp:status) (client twitter-client))
  (do-issue (first (cores client)) status
    :client client
    :object status
    :user (ensure-user (chirp:user status) client)
    :message (chirp:text status)))

(defmethod reply ((status status) fmt &rest args)
  (do-issue (first (cores (client status))) update-status
    :client (client status)
    :user (user (client status))
    :reply-to (object status)
    :message (apply #'format NIL fmt args)))

(define-event direct-message (message-event channel-event passive-event)
  ((object :initarg :object :reader object)))

(defmethod process ((message chirp:direct-message) (client twitter-client))
  (do-issue (first (cores client)) direct-message
    :client client
    :object message
    :user (ensure-user (chirp:sender message) client)
    :channel (ensure-channel
              (if (string= (username client)
                           (chirp:screen-name (chirp:sender message)))
                  (chirp:recipient message)
                  (chirp:sender message))
              client)
    :message (chirp:text message)))
