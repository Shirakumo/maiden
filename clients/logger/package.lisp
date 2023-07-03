(in-package #:maiden-user)
(defpackage #:maiden-logger
  (:nicknames #:org.shirakumo.maiden.clients.logger)
  (:use #:cl #:maiden #:maiden-client-entitiesk)
  ;; 
  (:export
   #:log-event
   #:logger))
