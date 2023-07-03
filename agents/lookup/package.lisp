(in-package #:cl-user)
(defpackage #:maiden-lookup
  (:nicknames #:org.shirakumo.maiden.agents.lookup)
  (:use #:cl #:maiden #:maiden-commands #:maiden-client-entities #:maiden-api-access)
  ;; interface.lisp
  (:export
   #:lookup))
