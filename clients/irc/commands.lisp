#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen.clients.irc)

(defvar *connection*)

(define-event command (irc-event)
  ())

(define-command connect (ev name &key
                            (nick "Colleen")
                            (host "irc.freenode.net")
                            (port 6667)
                            password
                            (realname (machine-instance))))

(define-command disconnect (ev name))


(defmacro define-irc-command (name args &body body)
  (let ((name (intern (string name) '#:org.shirakumo.colleen.clients.irc.commands)))
    (multiple-value-bind (kargs body) (deeds::parse-into-kargs-and-body body)
      `(define-command ,name (ev ,@args)
         ,@kargs
         (send-connection *connection* ,@body)))))

(define-irc-command pass (password)
  "PASS ~a" password)

(define-irc-command nick (nickname &key hopcount)
  "NICK ~a~@[ ~a~]" nickname hopcount)

(define-irc-command user (username hostname servername realname)
  "USER ~a ~a ~a :~a" username hostname servername realname)

(define-command server (servername hopcount info))

(define-command oper (user password))

(define-irc-command quit (&optional comment)
  "QUIT~@[ :~a~]" comment)

(define-command squit (server comment))

(define-command join (&rest channels))

(define-command part (&rest channels))

(define-command mode (target mode &key limit user ban-mask))

(define-command topic (channel &optional topic))

(define-command names (&rest channels))

(define-command list (channels &key server))

(define-command invite (nickname channel))

(define-command kick (channel user &optional comment))

(define-command version (&key server))

(define-command stats (&key query server))

(define-command links (&key remote-server server-mask))

(define-command time (&key server))

(define-command trace (&key server))

(define-command admin (&key server))

(define-command info (&key server))

(define-command privmsg (message &rest receivers))

(define-command notice (nickname text))

(define-command who (&key name opers-only))

(define-command whois (nickmasks &key server))

(define-command whowas (nickname &key count server))

(define-command kill (nickname comment))

(define-command ping (server &optional other-server))

(define-command pong (daemon &optional other-daemon))

(define-command error (message))

(define-command away (&optional message))

(define-command rehash ())

(define-command restart ())

(define-command summon (user &key server))

(define-command users (&key server))

(define-command wallops (message))

(define-command userhost (&rest nicknames))

(define-command ison (&rest nicknames))
