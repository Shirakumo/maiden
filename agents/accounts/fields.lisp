#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.accounts)

(defvar *field-info* (make-hash-table :test 'equal))

(defun normalize-field-name (name)
  (etypecase name
    (string name)
    (keyword (string name))
    (symbol (format T "~(~a:~a~)" (package-name (symbol-package name)) (symbol-name name)))))

(defclass field-info ()
  ((name :initarg :name :reader name)
   (documentation :initarg :documentation)
   (access :initarg :access :reader name))
  (:default-initargs
   :name (error "Field NAME required.")
   :documentation ""
   :access (list :owner :rw
                 :world :r)))

(defmethod initialize-instance :after ((info field-info) &key)
  (setf (name info) (normalize-field-name (name info)))
  (setf (field-info (name info)) info))

(defun field-info (name)
  (gethash (normalize-field-name name) *field-info*))

(defun (setf field-info) (info name)
  (setf (gethash (normalize-field-name name) *field-info*) info))

(defun remove-field-info (name)
  (remhash (etypecase name
             (string name)
             (symbol (normalize-field-name name))
             (field-info (name name))) *field-info*))

(defmacro define-fields (&body fields)
  `(progn ,@(loop for field in fields
                  collect (destructuring-bind (name (&key owner world) &optional (documentation "")) field
                            `(make-instance 'field-info :name ,(normalize-field-name name)
                                                        :documentation ,documentation
                                                        :access (list :owner ,owner
                                                                      :world ,world))))))

(defmethod documentation ((info field-info) doc-type)
  (slot-value info 'documentation))

(defmethod (setf documentation) (string (info field-info) doc-type)
  (setf (slot-value info 'documentation) (or string "")))

(defmethod access-p ((field string) account user access)
  (access-p (or (field-info field) (make-instance 'field-info :name field)) account user access))

(defmethod access-p ((info field-info) account user access)
  (access-p info (account account) user access))

(defmethod access-p ((info field-info) (account account) (user user) access)
  (let ((test (ecase access
                (:r '(:r :rw))
                (:w '(:w :rw))
                (:rw '(:rw)))))
    (or (find (getf (access info) :world) test)
        (and (authenticated-p user)
             (identity-p account user)
             (find (getf (access info) :owner) test)))))

(defmethod field (field (account (eql T)) user)
  (field field (account user) user))

(defmethod field (field (account account) user)
  (field (normalize-field-name field) account user))

(defmethod field ((field string) account user)
  (field field (account account) user))

(defmethod field ((field string) (account account) user)
  (when (and user (not (eql user :system)))
    (unless (access-p field account user :r)
      (error 'field-access-denied :field field :account account :user user)))
  (gethash field (data account)))

(defmethod (setf field) (value field (account (eql T)) user)
  (setf (field field (account user) user) value))

(defmethod (setf field) (value field (account account) user)
  (setf (field (normalize-field-name field) account user) value))

(defmethod (setf field) (value (field string) account user)
  (setf (field field (account account) user) value))

(defmethod (setf field) (value (field string) (account account) user)
  (when (and user (not (eql user :system)))
    (unless (access-p field account user :w)
      (error 'field-access-denied :field field :account account :user user)))
  (setf (gethash field (data account)) value))
