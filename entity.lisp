(in-package #:org.shirakumo.maiden)

(defgeneric matches (a b))

(defmethod matches (a b)
  (equal a b))

(defmethod matches ((a uuid:uuid) (b uuid:uuid))
  (uuid:uuid= a b))

(defmethod matches (a (uuid uuid:uuid))
  (matches uuid a))

(defmethod matches ((a uuid:uuid) (b vector))
  (matches (uuid:uuid-to-byte-array a) b))

(defmethod matches ((a uuid:uuid) (b string))
  (matches (princ-to-string a) b))

(defmethod matches ((a list) (b list))
  (loop for (ael . arest) in a
        for (bel . brest) in b
        always (and (xnor arest brest)
                    (matches a b))))

(defmethod matches ((a string) (b string))
  (string= a b))

(defmethod matches ((a vector) (b vector))
  (and (= (length a) (length b))
       (loop for ael across a
             for bel across b
             always (matches ael bel))))

(defclass entity ()
  ((id :initarg :id :accessor id))
  (:default-initargs
   :id (princ-to-string (uuid:make-v4-uuid))))

(defmethod print-object ((entity entity) stream)
  (print-unreadable-object (entity stream :type T)
    (format stream "~a" (id entity))))

(defmethod matches ((a entity) (b entity))
  (or (eq a b)
      (matches (id a) (id b))))

(defmethod matches ((entity entity) b)
  (matches (id entity) b))

(defmethod matches (a (entity entity))
  (matches entity a))

(defclass named-entity (entity)
  ((name :initarg :name :accessor name))
  (:default-initargs
   :name NIL))

(defmethod print-object ((named-entity named-entity) stream)
  (print-unreadable-object (named-entity stream :type T)
    (format stream "~@[~a ~]~a" (name named-entity) (id named-entity))))

(defmethod matches ((entity named-entity) b)
  (or (call-next-method)
      (and (name entity)
           (matches (name entity) b))))

(defgeneric find-entity (id place))

(defmethod find-entity (id (entity named-entity))
  (when (matches id entity)
    entity))

(defmethod find-entity (id (list list))
  (loop for item in list thereis (find-entity id item)))

(defclass data-entity (entity)
  ((data :initform (make-hash-table :test 'equal) :accessor data)))

(defmethod data-value (field (entity data-entity))
  (gethash field (data entity)))

(defmethod (setf data-value) (value field (entity data-entity))
  (setf (gethash field (data entity)) value))
