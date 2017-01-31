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

(defclass irc-user (simple-user)
  ((user :initarg :user :reader user)
   (host :initarg :host :reader host))
  (:default-initargs
   :user ""
   :host ""))

(defmethod reply ((user irc-user) message &rest args)
  (irc:privmsg (client user) (name user) (apply #'format NIL message args)))

(defclass irc-channel (simple-channel)
  ())

(defmethod reply ((channel irc-channel) message &rest args)
  (irc:privmsg (client channel) (name channel) (apply #'format NIL message args)))

(defmethod ensure-user ((name string) (client irc-client))
  (or (find-user name client)
      (make-instance 'irc-user :name name :client client)))

(defmethod ensure-channel ((name string) (client irc-client))
  (or (find-channel name client)
      (make-instance 'irc-channel :name name :client client)))

(defun prune-users (client)
  (loop for user being the hash-values of (user-map client)
        do (unless (channels user)
             (remove-user user client))))

(defun coerce-irc-object (name user host client)
  (let ((name (string-left-trim "~=+*@" name)))
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
         (setf (find-channel (name channel) user) channel)
         (setf (find-user (name user) client) user)
         (setf (find-user (name user) channel) user))))

(define-handler (irc-client track-namreply irc:rpl-namreply) (client ev channel info)
  :match-consumer 'client
  (dolist (user (cl-ppcre:split " +" info))
    (let ((object (coerce-irc-object user NIL NIL client)))
      (setf (find-user (name object) channel) object)
      ;; We somehow missed a JOIN, but we trust NAMREPLY more.
      (unless (find-user object client)
        (setf (find-user (name object) client) object)))))

(define-handler (irc-client track-nick irc:msg-nick) (client ev user nickname)
  :match-consumer 'client
  :after '(nick-change)
  (let ((old-nick (name user)))
    (unless (matches nickname (nickname client))
      (dolist (channel (channels user))
        (remove-user (name user) channel)
        (setf (find-user nickname channel) user))
      (when (find-user user client)
        (remove-user (name user) client)
        (setf (find-user nickname client) user))
      (setf (name user) nickname))
    ;; Issue nick change event
    (do-issue (core ev) user-name-changed :client client :user user :old-name old-nick)))

(define-handler (irc-client track-leave irc:msg-part) (client ev channel user)
  :match-consumer 'client
  (cond ((matches user (nickname client))
         (remove-channel channel client))
        (T
         (remove-channel channel user)
         (remove-user (name user) channel)))
  (prune-users client))

(define-handler (irc-client track-kick irc:msg-kick) (client ev channel nickname)
  :match-consumer 'client
  (cond ((matches nickname (nickname client))
         (remove-channel channel client))
        (T
         (remove-channel channel user)
         (remove-user nickname channel)))
  (prune-users client))

(define-handler (irc-client track-quit irc:msg-quit) (client ev user)
  :match-consumer 'client
  (remove-user user client))

(define-handler (irc-client track-kill irc:msg-kill) (client ev nickname)
  :match-consumer 'client
  (let ((user (find-user nickname client)))
    (when user (remove-user user client))))

(define-handler (irc-client track-topic irc:rpl-topic) (client ev channel topic)
  :match-consumer 'client
  (let ((channel (ensure-channel channel client)))
    (setf (topic channel) topic)))
