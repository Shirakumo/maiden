#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen.clients.irc)

(define-event irc-event ()
  ((connection :initarg :connection :reader connection))
  (:default-initargs
   :connection (error "CONNECTION required")))

(define-event reply (irc-event)
  ())
