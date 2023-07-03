(in-package #:org.shirakumo.maiden)

(define-consumer agent () ())

(defmethod initialize-instance :after ((agent agent) &key)
  (unless (name agent)
    (setf (name agent) (class-name (class-of agent)))))

(defmethod print-object ((agent agent) stream)
  (print-unreadable-object (agent stream :type T)
    (format stream "~@[~a ~]~a"
            (when (string/= (name agent) (class-name (class-of agent))) (name agent))
            (id agent))))

(defmethod matches ((a agent) (b agent))
  (eql (class-of a) (class-of b)))

(defmethod matches ((a agent) (b symbol))
  (or (call-next-method)
      (and (find-class b NIL)
           (typep a b))))

(defmethod matches ((a symbol) (b agent))
  (matches b a))

(defmethod add-consumer :before ((agent agent) (core core))
  (let ((existing (find (name agent) (consumers core) :test #'matches :key #'name)))
    (when (and existing (not (eql existing agent)))
      (error 'agent-already-exists-error :core core :agent agent :existing-agent existing))))
