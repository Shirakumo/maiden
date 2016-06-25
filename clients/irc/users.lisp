#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.clients.irc)

;; FIXME: We need to lock up the user and channel tables //everywhere//
;;        during updates to ensure consistency during changes. Otherwise
;;        things like rapid nicks or parts/joins could end up confusing
;;        the system.

(defclass irc-server (client-entity)
  ())

(defmethod send-to ((server irc-server) message &rest args)
  (send (apply #'format NIL message args) (client server)))

(defclass irc-user (user)
  ((user :initarg :user :reader user)
   (host :initarg :host :reader host))
  (:default-initargs
   :user ""
   :host ""))

(defmethod send-to ((user irc-user) message &rest args)
  (irc:privmsg (client user) (name user) (apply #'format NIL message args)))

(defmethod reply ((user irc-user) message &rest args)
  (irc:privmsg (client user) (name user) (apply #'format NIL message args)))

(defclass irc-channel (channel)
  ((users :initform (make-hash-table :test 'equalp) :accessor user-map)))

(defmethod send-to ((channel irc-channel) message &rest args)
  (irc:privmsg (client channel) (name channel) (apply #'format NIL message args)))

(defmethod reply ((channel irc-channel) message &rest args)
  (irc:privmsg (client channel) (name channel) (apply #'format NIL message args)))

(defmethod users ((client irc-client))
  (loop for v being the hash-values of (user-map client) collect v))

(defmethod users ((channel channel))
  (loop for v being the hash-values of (user-map channel) collect v))

(defmethod channels ((user irc-user))
  (loop for channel being the hash-values of (channel-map (client user))
        when (find-user user channel)
        collect channel))

(defmethod channels ((client irc-client))
  (loop for v being the hash-values of (channel-map client) collect v))

(defmethod find-user ((user user) thing)
  (find-user (name user) thing))

(defmethod find-user ((name string) (client irc-client))
  (gethash name (user-map client)))

(defmethod find-user ((name string) (channel channel))
  (gethash name (user-map channel)))

(defmethod find-channel ((channel channel) thing)
  (find-channel (name channel) thing))

(defmethod find-channel ((name string) (client irc-client))
  (gethash name (channel-map client)))

(defmethod find-channel ((name string) (user irc-user))
  (find name (channels user) :test #'matches))

(defmethod ensure-user ((name string) (client irc-client))
  (or (find-user name client)
      (make-instance 'irc-user :name name :client client)))

(defmethod ensure-channel ((name string) (client irc-client))
  (or (find-channel name client)
      (make-instance 'irc-channel :name name :client client)))

(defmethod remove-user ((name string) (channel irc-channel))
  (remhash name (user-map channel)))

(defmethod remove-user ((user irc-user) (channel irc-channel))
  (remove-user (name user) channel))

(defmethod remove-user ((name string) (client irc-client))
  (remove-user (find-user name client) client))

(defmethod remove-user ((user irc-user) (client irc-client))
  (dolist (channel (channels user))
    (remove-user user channel))
  (remhash (name user) (user-map client)))

(defmethod remove-channel ((name string) (client irc-client))
  (remhash name (channel-map client)))

(defmethod remove-channel ((name string) (user irc-user))
  (remove-channel (find-channel name user) user))

(defmethod remove-channel ((channel irc-channel) (user irc-user))
  (remove-user user channel))

(defmethod remove-channel ((channel irc-channel) (client irc-client))
  (remove-channel (name channel) client))

(defun prune-users (client)
  (loop for user being the hash-values of (user-map client)
        do (unless (channels user)
             (remove-user user client))))

(defun coerce-irc-object (name user host client)
  (let ((name (string-left-trim "=+*@" name)))
    (cond ((find #\. name)
           (make-instance 'irc-server :name name :client client))
          ((find #\# name)
           (or (find-channel name client)
               (make-instance 'irc-channel :name name :client client)))
          (T
           (or (find-user name client)
               (make-instance 'irc-user :name name
                                        :user (or user "")
                                        :host (or host "")
                                        :client client))))))

(define-handler (irc-client track-join irc:msg-join) (client ev channel user)
  :match-consumer 'client
  (cond ((matches user (nickname client))
         (setf (gethash (name channel) (channel-map client)) channel))
        (T
         ;; Note!! We have to be 100% fukin' sure that this is either the ONLY instance
         ;;        of this particular user, or one that already exists in the map.
         ;;        Otherwise we might run into inconsistency issues where some parts of
         ;;        the system might have a dupe of a user with a different object
         ;;        identity! This can actually happen no matter what if we are particularly
         ;;        unlucky about concurrency and other parts of the system retaining user
         ;;        objects for a prolonged period of time.
         (setf (gethash (name user) (user-map client)) user)
         (setf (gethash (name user) (user-map channel)) user))))

(define-handler (irc-client track-namreply irc:rpl-namreply) (client ev channel info)
  :match-consumer 'client
  (dolist (user (cl-ppcre:split " +" info))
    (let ((object (coerce-irc-object user NIL NIL client)))
      (setf (gethash (name object) (user-map channel)) object)
      ;; We somehow missed a JOIN, but we trust NAMREPLY more.
      (unless (find-user object client)
        (setf (gethash (name object) (user-map client)) object)))))

(define-handler (irc-client track-nick irc:msg-nick) (client ev user nickname)
  :match-consumer 'client
  :after '(nick-change)
  (let ((old-nick (name user)))
    (unless (matches nickname (nickname client))
      (dolist (channel (channels user))
        (remhash (name user) (user-map channel))
        (setf (gethash nickname (user-map channel)) user))
      (when (find-user user client)
        (remhash (name user) (user-map client))
        (setf (gethash nickname (user-map client)) user))
      (setf (name user) nickname))
    ;; Issue nick change event
    (do-issue (core event) 'user-name-changed :client client :user user :old-nick old-nick)))

(define-handler (irc-client track-leave irc:msg-part) (client ev channel user)
  :match-consumer 'client
  (cond ((matches user (nickname client))
         (remove-channel channel client))
        (T
         (remhash (name user) (user-map channel))))
  (prune-users client))

(define-handler (irc-client track-kick irc:msg-kick) (client ev channel nickname)
  :match-consumer 'client
  (cond ((matches nickname (nickname client))
         (remove-channel channel client))
        (T
         (remhash nickname (user-map channel))))
  (prune-users client))

(define-handler (irc-client track-quit irc:msg-quit) (client ev user)
  :match-consumer 'client
  (remove-user user client))

(define-handler (irc-client track-kill irc:msg-kill) (client ev nickname)
  :match-consumer 'client
  (let ((user (find-user nickname client)))
    (when user (remove-user user client))))
