(in-package #:org.shirakumo.maiden.agents.notify)

(defclass note ()
  ((id :initarg :id :accessor id)
   (from :initarg :from :accessor from)
   (to :initarg :to :accessor to)
   (message :initarg :message :accessor message)
   (date :initarg :date :accessor date)
   (trigger :initarg :trigger :accessor trigger))
  (:default-initargs
   :id (next-note-id)
   :from "Anonymous"
   :to (error "TO required.")
   :message (error "MESSAGE required.")
   :date (get-universal-time)
   :trigger :message))

(defmethod initialize-instance :after ((note note) &key)
  (register-note note))

(defun make-note (from to message &key (date (get-universal-time)) (trigger :message))
  (make-instance 'note :from from :to to :message message :date date :trigger trigger))

(defun next-note-id ()
  (with-storage ('global)
    (setf (value :next-id) (1+ (or (value :next-id) 0)))))

(defun normalize-user-name (user)
  (string-downcase
   (etypecase user
     (user (name user))
     (symbol (string user))
     (string user))))

(defun register-note (note)
  (with-storage ('notes)
    (pushnew note (value (normalize-user-name (to note))) :key #'id)))

(defun remove-note (note)
  (let ((to (normalize-user-name (to note))))
    (with-storage ('notes)
      (setf (value to) (remove (id note) (value to) :key #'id)))))

(defun clear-notes (user)
  (with-storage ('notes)
    (setf (value (normalize-user-name user)) ())))

(defun user-notes (user)
  (copy-list
   (ignore-errors
    (with-storage ('notes)
      (value (normalize-user-name user))))))
