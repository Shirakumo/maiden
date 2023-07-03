(in-package #:maiden-user)
(defpackage #:maiden-activatable
  (:nicknames #:org.shirakumo.maiden.agents.activatable)
  (:use #:cl #:maiden #:maiden-storage #:maiden-commands #:maiden-client-entities)
  ;; activatable.lisp
  (:export
   #:activate
   #:deactivate
   #:active-p
   #:list-active
   #:activatable-handler
   #:activatable))
