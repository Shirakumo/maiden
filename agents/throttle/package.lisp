(in-package #:maiden-user)
(defpackage #:maiden-throttle
  (:nicknames #:org.shirakumo.maiden.agents.throttle)
  (:use #:cl #:maiden #:maiden-storage #:maiden-commands #:maiden-client-entities)
  ;; throttle.lisp
  (:export
   #:throttle
   #:attempts
   #:time-frame
   #:cooldown-function
   #:cooldown-step
   #:cooldown-max
   #:records
   #:record
   #:attempts
   #:timestamp
   #:timeout
   #:clear-tax
   #:tax
   #:view-config
   #:set-config
   #:clear-tax))
