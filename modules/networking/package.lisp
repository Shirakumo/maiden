#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-networking
  (:nicknames #:org.shirakumo.maiden.modules.networking)
  (:use #:cl #:maiden)
  ;; conditions.lisp
  (:export
   #:client-connection-failed-error
   #:client-still-connected-error
   #:client-reconnection-exceeded-error
   #:client-connection-closed-uncleanly-warning
   #:closing-error
   #:client-timeout-error
   #:timeout
   #:data-condition
   #:data
   #:data-parse-error
   #:unknown-data-warning
   #:data-too-long-warning)
  ;; clients.lisp
  (:export
   #:client-connected-p
   #:close-connection
   #:initiate-connection
   #:handle-connection
   #:handle-connection-error
   #:handle-connection-idle
   #:process
   #:send
   #:receive
   #:accept
   #:make-tcp-server-client
   #:remote-client
   #:ip-client
   #:host
   #:port
   #:socket-client
   #:socket
   #:read-thread
   #:socket-lock
   #:reconnecting-client
   #:failures
   #:max-failures
   #:backoff
   #:interval
   #:timeout-client
   #:timeout
   #:last-received-time
   #:text-client
   #:encoding
   #:buffer
   #:tcp-client
   #:element-type
   #:idle-interval
   #:text-tcp-client
   #:tcp-server
   #:clients
   #:tcp-server-client
   #:server
   #:socket)
  ;; events.lisp
  (:export
   #:connection-event
   #:connection-initiated
   #:connection-closed
   #:outgoing-event
   #:incoming-event))

(use-package '#:maiden-networking '#:maiden-user)
