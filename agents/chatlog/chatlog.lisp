#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.chatlog)

(define-consumer chatlog (agent)
  ())

(define-handler (chatlog message message-event) (c ev user message)
  (let ((channel (if (typep ev 'channel-event) (channel ev) user)))
    (maybe-record-message :message channel user message)))

(define-handler (chatlog enter user-entered) (c ev user channel)
  (maybe-record-message :enter channel user "** JOIN"))

(define-handler (chatlog leave user-left) (c ev user channel)
  (maybe-record-message :leave channel user "** PART"))

;; (define-handler (chatlog kick user-kick) (c ev user channel target)
;;   (maybe-record-message :kick channel user "** KICKED ~a" target))

(define-handler (chatlog remove user-removed) (c ev user)
  (dolist (channel (channels user))
    (maybe-record-message :disconnect channel user "** QUIT")))

(define-handler (chatlog name-change user-name-changed) (c ev user old-name)
  (dolist (channel (channels user))
    (maybe-record-message :name channel old-name "** NICK ~a" (name user))))

(define-handler (chatlog topic channel-topic-changed) (c ev channel topic)
  (let ((user (if (typep ev 'user-event) (user ev) channel)))
    (maybe-record-message :name channel user  "** TOPIC ~a" topic)))

(define-command (chatlog activate) (c ev client channel)
  :command "activate chatlog on"
  :advice ((not public))
  (let ((channel (cons client channel)))
    (add-channel channel)
    (reply ev "Activated logging for ~a." channel)))

(define-command (chatlog initialize) (c ev &key (host "localhost") (db "chatlog") (user "chatlog") password (port 5432))
  :advice ((not public))
  (initialize-database :host host
                       :database db
                       :user user
                       :password password
                       :port (etypecase port
                               (integer port)
                               (string (parse-integer port))
                               (null 5432)))
  (reply ev "Initialised the chatlog database."))
