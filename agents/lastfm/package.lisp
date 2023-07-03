(in-package #:cl-user)
(defpackage #:maiden-lastfm
  (:nicknames #:org.shirakumo.maiden.agents.lastfm)
  (:use #:cl #:maiden #:maiden-client-entities #:maiden-commands #:maiden-api-access)
  ;; interface.lisp
  (:export
   #:lastfm))
