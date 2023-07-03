(in-package #:maiden-user)
(defpackage #:maiden-urlinfo
  (:nicknames #:org.shirakumo.maiden.agents.urlinfo)
  (:use #:cl #:maiden #:maiden-commands #:maiden-client-entities #:maiden-activatable)
  ;; urlinfo.lisp
  (:export
   #:fetch
   #:urlinfo
   #:test))
