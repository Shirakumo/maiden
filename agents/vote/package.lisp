(in-package #:maiden-user)
(defpackage #:maiden-vote
  (:nicknames #:org.shirakumo.maiden.agents.vote)
  (:use #:cl #:maiden #:maiden-commands #:maiden-client-entities)
  ;; vote.lisp
  (:export
   #:vote
   #:start-vote
   #:end-vote))
