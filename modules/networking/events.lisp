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
