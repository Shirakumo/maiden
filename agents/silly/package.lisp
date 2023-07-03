(in-package #:maiden-user)
(defpackage #:maiden-silly
  (:nicknames #:org.shirakumo.maiden.agents.silly)
  (:use #:cl #:maiden #:maiden-activatable #:maiden-commands #:maiden-api-access #:maiden-client-entities)
  ;; silly.lisp
  (:export
   #:silly
   #:silly-function
   #:remove-silly-function
   #:define-silly
   #:define-simple-silly
   #:eight
   #:jerkcity
   #:roll
   #:hello
   #:present
   #:you-are
   #:make
   #:fortune
   #:tell))
