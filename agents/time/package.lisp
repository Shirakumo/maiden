(in-package #:maiden-user)
(defpackage #:maiden-time
  (:nicknames #:org.shirakumo.maiden.agents.time)
  (:use #:cl #:maiden #:maiden-api-access #:maiden-commands #:maiden-client-entities)
  ;; time.lisp
  (:shadow #:time)
  (:export
   #:timezone-data
   #:timezone
   #:local-time
   #:user-time
   #:time
   #:timezone-location
   #:time-dwim
   #:time-location
   #:time-user
   #:time-between
   #:time-between-users))
