(in-package #:org.shirakumo.maiden.clients.logger)

(define-event log-event (client-event deeds:message-event)
  ())

(define-consumer logger (client)
  ())

(define-handler (logger events log-event) (logger ev message)
  :match-consumer 'client
  (v:info :logger "~a" message))
