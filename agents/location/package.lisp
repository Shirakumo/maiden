(in-package #:maiden-user)
(defpackage #:maiden-location
  (:nicknames #:org.shirakumo.maiden.agents.location)
  (:use #:cl #:maiden #:maiden-api-access #:maiden-commands #:maiden-client-entities)
  ;; location.lisp
  (:export
   #:geo-information
   #:coordinates
   #:address
   #:location))
