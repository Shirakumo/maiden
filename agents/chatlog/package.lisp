(in-package #:maiden-user)
(defpackage #:maiden-chatlog
  (:nicknames #:org.shirakumo.maiden.agents.chatlog)
  (:local-nicknames (#:v #:org.shirakumo.verbose))
  (:use #:cl #:maiden #:maiden-storage #:maiden-commands #:maiden-client-entities)
  ;; chatlog.lisp
  (:export
   #:chatlog
   #:activate
   #:activate-on
   #:deactivate
   #:deactivate-on
   #:initialize)
  ;; database.lisp
  (:export
   #:connection
   #:with-db
   #:prepared-statement
   #:initialize-database
   #:add-channel
   #:del-channel
   #:record-message
   #:process-back-queue
   #:maybe-record-message))
