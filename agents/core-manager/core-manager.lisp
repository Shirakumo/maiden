#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.core-manager)

(define-consumer core-manager ()
  ())

(define-command (core-manager start-consumer) (c ev consumer)
  :command "start consumer"
  :advice (not public)
  (let ((consumer (or (consumer consumer (core ev))
                      (error "No such consumer ~s." consumer))))
    (start consumer)
    (reply ev "~a started." consumer)))

(define-command (core-manager stop-consumer) (c ev consumer)
  :command "stop consumer"
  :advice (not public)
  (let ((consumer (or (consumer consumer (core ev))
                      (error "No such consumer ~s." consumer))))
    (stop consumer)
    (reply ev "~a stopped." consumer)))

(define-command (core-manager remove-consumer) (c ev consumer)
  :command "remove consumer"
  :advice (not public)
  (let ((consumer (or (consumer consumer (core ev))
                      (error "No such consumer ~s." consumer))))
    (remove-consumer consumer (core ev))
    (reply ev "~a removed." consumer)))

(define-command (core-manager add-consumer) (c ev package &key symbol name)
  :command "add consumer"
  :advice (not public)
  (let ((class-name (if symbol
                        (find-symbol symbol package)
                        (find-consumer-in-package package))))
    (when (or (not class-name) (not (find-class class-name NIL))) 
      (error "No such class found."))
    (let ((consumer (make-instance class-name)))
      (when name (setf (name consumer) name))
      (start (add-consumer consumer (core ev)))
      (reply ev "~a added to core." consumer))))

(define-command (core-manager stop-core) (c ev)
  :command "stop core"
  :advice (not public)
  (reply ev "Stopping core...")
  ;; Maybe it'll get through in time.
  (sleep 0.1)
  (stop (core ev)))
