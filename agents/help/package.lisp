#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-help
  (:nicknames #:org.shirakumo.maiden.agents.help)
  (:use #:cl #:maiden #:maiden-commands #:maiden-client-entities)
  ;; help.lisp
  (:export
   #:help
   #:about
   #:about-self
   #:about-uptime
   #:about-command
   #:list-consumers
   #:about-consumer
   #:about-term))
