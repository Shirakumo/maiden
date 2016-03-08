#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen.clients.relay)

(define-condition relay-condition (client-condition) ())

(define-condition carrier-condition (relay-condition)
  ((message :initarg :message :reader message))
  (:default-initargs :message (error "MESSAGE required.")))

(define-condition target-condition (relay-condition)
  ((target :initarg :target :reader target))
  (:default-initargs :target (error "TARGET required.")))

(define-condition no-relay-target-specified (carrier-condition error)
  ()
  (:report (lambda (c s) (format s "Cannot relay ~s to nothing over ~a.
Somewhere a target was not properly specified."
                                 (message c) (client c)))))

(define-condition relay-route-not-found (carrier-condition target-condition error)
  ()
  (:report (lambda (c s) (format s "No route found to ~a over ~a for ~s.
Either the network is temporarily unstable or the target is unreachable."
                                 (target c) (client c) (message c)))))

(define-condition relay-link-not-found (carrier-condition target-condition error)
  ()
  (:report (lambda (c s) (format s "No link found to ~a over ~a for ~s.

This means that the network is either temporarily unstable or permanently corrupted.
A complete restarting of the local relay and re-establishment of the connections
should fix this problem."
                                 (target c) (client c) (message c)))))

(define-condition client-version-mismatch (relay-condition warning)
  ((remote-version :initarg :remote-version :reader remote-version))
  (:report (lambda (c s) (format s "The version of the framework at the remote relay ~a (~a) does not match ours (~a)."
                                 (remote (client c)) (remote-version c) (asdf:component-version (asdf:find-system :colleen))))))
