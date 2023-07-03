(in-package #:maiden-user)
(defpackage #:maiden-blocker
  (:nicknames #:org.shirakumo.maiden.agents.blocker)
  (:use #:cl #:maiden #:maiden-storage #:maiden-commands #:maiden-client-entities)
  ;; blocker.lisp
  (:export
   #:clause
   #:remove-clause
   #:define-clause
   #:match-rule
   #:ensure-rule
   #:rule
   #:add-rule
   #:remove-rule
   #:blocked-p
   #:blocker
   #:block-channel
   #:block-user
   #:block-regex
   #:block-prefix
   #:update-rule
   #:remove-rule
   #:view-rule
   #:view-rules))
