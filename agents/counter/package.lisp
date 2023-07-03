(in-package #:maiden-user)
(defpackage #:maiden-counter
  (:nicknames #:org.shirakumo.maiden.agents.counter)
  (:use #:cl #:maiden #:maiden-storage #:maiden-commands #:maiden-client-entities #:maiden-activatable)
  ;; counter.lisp
  (:shadow #:remove #:list)
  (:export
   #:counter
   #:remove-counter
   #:set-counter
   #:list-counters
   #:add
   #:change
   #:remove
   #:list))
