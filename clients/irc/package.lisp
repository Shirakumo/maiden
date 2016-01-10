#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:colleen-irc-commands
  (:nicknames #:org.shirakumo.colleen.clients.irc.commands)
  (:use)
  ;;
  (:export
   #:connect
   #:disconnect
   #:pass
   #:nick
   #:user
   #:server
   #:oper
   #:quit
   #:squit
   #:join
   #:part
   #:mode
   #:topic
   #:names
   #:list
   #:invite
   #:kick
   #:version
   #:stats
   #:links
   #:time
   #:trace
   #:admin
   #:info
   #:privmsg
   #:notice
   #:who
   #:whois
   #:whowas
   #:ping
   #:pong
   #:error
   #:away
   #:rehash
   #:restart
   #:summon
   #:users
   #:wallops
   #:userhost
   #:ison))

(defpackage #:colleen-irc
  (:nicknames #:org.shirakumo.colleen.clients.irc)
  (:use #:cl #:colleen #:deeds #:org.shirakumo.colleen.clients.irc.commands)
  ;; 
  (:export))
