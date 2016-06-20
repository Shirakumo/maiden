#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-notify
  (:nicknames #:org.shirakumo.maiden.agents.notify)
  (:use #:cl #:maiden #:maiden-storage #:maiden-commands #:maiden-client-entities)
  ;; interface.lisp
  (:export
   #:notify)
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
