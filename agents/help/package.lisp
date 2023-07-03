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
