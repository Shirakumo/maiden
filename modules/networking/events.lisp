#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.modules.networking)

(define-event connection-event (client-event)
  ())

(define-event connection-initiated (connection-event)
  ())

(define-event connection-closed (connection-event)
  ())

(define-event outgoing-event (client-event)
  ())

(define-event incoming-event (client-event)
  ())
