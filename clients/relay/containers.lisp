#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen.clients.relay)

(define-event connection-initiated (client-event)
  ())

(define-event connection-closed (client-event)
  ())

(define-event relay-instruction-event (instruction-event)
  ())

(define-event query-event (deeds:identified-event relay-instruction-event)
  ((source :initarg :source :reader source))
  (:default-initargs
   :source (error "SOURCE required.")
   :identifier (uuid:make-v4-uuid)))

(defmethod execute-instruction :around ((event query-event) &key relay)
  (relay (make-transport
          (make-instance 'response-event
                         :response (call-next-method)
                         :identifier (deeds:identifier event))
          (source event))
         (source event)
         relay))

(define-event response-event (deeds:identified-event)
  ((response :initarg :response :reader response))
  (:default-initargs
   :response (error "RESPONSE required.")
   :identifier (error "IDENTIFIER required.")))

(define-event slot-event (query-event relay-instruction-event)
  ((slot :initarg :slot :reader slot)
   (object :initarg :object :reader object))
  (:default-initargs
   :slot (error "SLOT required.")
   :object (error "OBJECT required.")))

(define-event slot-value-event (slot-event)
  ())

(defmethod execute-instruction ((event slot-value-event) &key)
  (slot-value (object event) (slot event)))

(define-event slot-setf-event (slot-event)
  ((value :initarg :value :reader value))
  (:default-initargs
   :value NIL))

(defmethod execute-instruction ((event slot-setf-event) &key)
  (setf (slot-value (object event) (slot event)) (value event)))

(define-event slot-makunbound-event (slot-event)
  ())

(defmethod execute-instruction ((event slot-makunbound-event) &key)
  (slot-makunbound (object event) (slot event)))

(define-event slot-boundp-event (slot-event)
  ())

(defmethod execute-instruction ((event slot-boundp-event) &key)
  (slot-boundp (object event) (slot event)))

(define-event generic-call-event (query-event relay-instruction-event)
  ((form :initarg :form :reader form))
  (:default-initargs
   :form (error "FORM required.")))

(defmethod execute-instruction ((event generic-call-event) &key)
  (funcall (compile NIL (form event))))

(defclass subscription-update (entity)
  ((target :initarg :target :accessor target)
   (subscriber :initarg :subscriber :accessor subscriber))
  (:default-initargs
   :target T
   :subscriber (error "SUBSCRIBER required.")))

(defclass subscription (subscription-update)
  ((event-type :initarg :event-type :accessor event-type)
   (filter :initarg :filter :accessor filter))
  (:default-initargs
   :event-type (error "EVENT-TYPE required.")
   :filter T))

(defclass unsubscription (subscription-update)
  ())

(defclass network-update ()
  ((new :initarg :new :accessor new)
   (bad :initarg :bad :accessor bad))
  (:default-initargs
   :new () :bad ()))

(defmethod print-object ((update network-update) stream)
  (print-unreadable-object (update stream :type T)
    (format stream "~s ~s ~s ~s" :new (new update) :bad (bad update))))

(defgeneric make-network-update (new bad))

(defmethod make-network-update ((new list) (bad list))
  (make-instance 'network-update :new new :bad bad))

(defmethod make-network-update ((new network-update) (special null))
  (make-instance 'network-update
                 :new (loop for (hops destination name) in (new new)
                            collect (list (1+ hops) destination name))
                 :bad (bad new)))

(defmethod make-network-update ((new consumer) bad)
  (make-network-update `((0 ,(id new) ,(name new))) bad))

(defmethod make-network-update (new (bad consumer))
  (make-network-update new `(,(id bad))))

(defmethod make-network-update ((new core) bad)
  (make-network-update (loop for c in (consumers new)
                             unless (typep c 'agent)
                             collect `(0 ,(id c) ,(name c))) bad))

(defmethod make-network-update (new (bad core))
  (make-network-update new (loop for c in (consumers bad)
                                 unless (typep c 'agent)
                                 collect (id c))))

(define-consumer virtual-client (client)
  ((links :initarg :links :accessor links))
  (:default-initargs
   :links ()))

(defmethod slot-missing (class (client virtual-client) slot operation &optional value)
  (v:info :test "~a" (cores client))
  (let* ((relay (consumer 'relay (first (cores client))))
         (event (ecase operation
                  (setf (make-instance 'slot-setf-event :source relay :object client :slot slot :value value))
                  (slot-makunbound (make-instance 'slot-makunbound-event :source relay :object client :slot slot))
                  (slot-value (make-instance 'slot-value-event :source relay :object client :slot slot))
                  (slot-boundp (make-instance 'slot-boundp-event :source relay :object client :slot slot)))))
    (with-awaiting (response-event re response)
        ((first (cores client))
         :filter `(uuid:uuid= deeds:identifier ,(deeds:identifier event))
         :timeout 60)
        (relay event client relay)
      response)))

(defgeneric make-virtual-client (target &key name links))

(defmethod make-virtual-client ((target uuid:uuid) &key name links)
  (make-instance 'virtual-client :id target :name name :links links))

(defmethod make-virtual-client ((target string) &key name links)
  (make-virtual-client (uuid:make-uuid-from-string target) :name name :links links))

(defmethod make-virtual-client ((target named-entity) &key (name (name target)) links)
  (make-virtual-client (id target) :name name :links links))

(defclass transport ()
  ((event :initarg :event :accessor event)
   (target :initarg :target :accessor target)))

(defmethod print-object ((transport transport) stream)
  (print-unreadable-object (transport stream :type T)
    (format stream "~s ~s ~s" :to (target transport) (event transport))))

(defgeneric make-transport (event target))

(defmethod make-transport ((event event) target)
  (make-instance 'transport :event event :target target))
