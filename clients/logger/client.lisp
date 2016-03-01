#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen.clients.logger)

(define-event log-event (client-event deeds:message-event)
  ())

(define-consumer logger (client)
  ())

(define-handler (logger events log-event) (logger ev message)
  :match-consumer 'client
  (v:info :logger "~a" message))
