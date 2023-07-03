(in-package #:maiden-user)
(defpackage #:maiden-notify
  (:nicknames #:org.shirakumo.maiden.agents.notify)
  (:use #:cl #:maiden #:maiden-storage #:maiden-commands #:maiden-client-entities)
  ;; interface.lisp
  (:export
   #:notify
   #:forget-notes
   #:send-join-note
   #:send-note)
  ;; notes.lisp
  (:export
   #:note
   #:id
   #:from
   #:to
   #:message
   #:date
   #:trigger
   #:make-note
   #:register-note
   #:remove-note
   #:clear-notes
   #:user-notes))
