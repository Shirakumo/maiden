#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-chatlog
  (:nicknames #:org.shirakumo.maiden.agents.chatlog)
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
