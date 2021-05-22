#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.core-manager)

(define-consumer core-manager ()
  ())

(defmethod start :after ((core-manager core-manager))
  (maiden-storage:with-storage (core-manager)
    (dolist (core (cores core-manager))
      (loop for (class . initargs) in (ubiquitous:value :consumers)
            do (unless (consumer (getf initargs :name) core)
                 (start (add-consumer (apply #'make-instance class initargs) core)))))))

(defun add-consumer-to-storage (c name class-name initargs)
  (maiden-storage:with-storage (c)
    (let ((clients (ubiquitous:value :consumers)))
      (setf clients (remove name clients :key (lambda (c) (getf (rest c) :name)) :test #'equal))
      (push (list* class-name (list* :name name initargs)) clients)
      (setf (ubiquitous:value :consumers) clients))))

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
      (reply ev "~a added to core." consumer)
      (add-consumer-to-storage c (name consumer) class-name ()))))

(define-command (core-manager add-client) (c ev package &key symbol name username nickname host channels password services-password)
  :command "add client"
  :advice (not public)
  (let ((class-name (if symbol
                        (find-symbol symbol package)
                        (find-consumer-in-package package)))
        (initargs ()))
    (when (or (not class-name) (not (find-class class-name NIL)))
      (error "No such class found."))
    (macrolet ((build-initargs (&rest initargs)
                 `(progn
                    ,@(loop for name in initargs
                            collect ``(when ,name
                                        (push ,name initargs)
                                        (push ,(intern (string name) "KEYWORD") initargs))))))
      (build-initargs name username nickname host channels password services-password))
    (let ((consumer (apply #'make-instance class-name initargs)))
      (start (add-consumer consumer (core ev)))
      (reply ev "~a added to core." consumer)
      (add-consumer-to-storage c (name consumer) class-name initargs))))

(define-command (core-manager list-consumers) (c ev)
  :command "list consumers"
  :advice public
  (reply ev "Active consumers: ~{~a~^ ~}"
         (mapcar (lambda (c) (or (name c) (id c))) (consumers (core ev)))))

(define-command (core-manager stop-core) (c ev)
  :command "stop core"
  :advice (not public)
  (reply ev "Stopping core...")
  ;; Maybe it'll get through in time.
  (sleep 0.1)
  (stop (core ev)))

(define-command (core-manager reload) (c ev)
  :command "reload"
  :advice (not public)
  (reply ev "Clearing out configuration caches...")
  (maiden-storage:reload))
