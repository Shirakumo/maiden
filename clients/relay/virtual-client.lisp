#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.clients.relay)

(define-consumer virtual-client (client)
  ((links :initarg :links :accessor links))
  (:default-initargs
   :links ()))

(defgeneric make-virtual-client (target &key name links))

(defmethod make-virtual-client ((target uuid:uuid) &key name links)
  (make-instance 'virtual-client :id target :name name :links links))

(defmethod make-virtual-client ((target string) &key name links)
  (make-virtual-client (uuid:make-uuid-from-string target) :name name :links links))

(defmethod make-virtual-client ((target named-entity) &key (name (name target)) links)
  (make-virtual-client (id target) :name name :links links))

(defmacro with-response-event ((response event client &key (timeout 60)) &body body)
  (let ((response-g (gensym "RESPONSE"))
        (event-g (gensym "EVENT"))
        (core-g (gensym "CORE"))
        (client-g (gensym "CLIENT")))
    `(let* ((,client-g ,client)
            (,event-g ,event)
            (,core-g (first (cores ,client-g))))
       (with-awaiting (,core-g response-event
                               :filter `(uuid:uuid= deeds:identifier ,(deeds:identifier ,event-g))
                               :timeout ,timeout) (,response-g ,response)
           (relay ,event-g ,client (consumer 'relay ,core-g))
         ,@body))))

(defmethod slot-missing (class (client virtual-client) slot operation &optional value)
  (let* ((core (first (cores client)))
         (event (ecase operation
                  (setf (make-instance 'slot-setf-event :source core :object client :slot slot :value value))
                  (slot-makunbound (make-instance 'slot-makunbound-event :source core :object client :slot slot))
                  (slot-value (make-instance 'slot-value-event :source core :object client :slot slot))
                  (slot-boundp (make-instance 'slot-boundp-event :source core :object client :slot slot)))))
    (with-response-event (response event core)
      response)))

(defmacro define-virtual-client-method (name args)
  (let ((form-g (gensym "FORM"))
        (event-g (gensym "EVENT"))
        (response-g (gensym "RESPONSE"))
        (client (or (loop for arg in args
                          until (find arg lambda-list-keywords)
                          thereis (and (listp arg) (eql (second arg) 'virtual-client)
                                       (first arg)))
                    (error "No ~s specializer in arguments list." 'virtual-client))))
    `(defmethod ,name ,args
       (let* ((,form-g (list ',name ,@(loop for arg in args
                                            unless (find arg lambda-list-keywords)
                                            collect (if (listp arg) (first arg) arg))))
              (,event-g (make-instance 'generic-call-event :source (first (cores ,client)) :form ,form-g)))
         (with-response-event ((,response-g response) ,event-g ,client)
           ,response-g)))))
