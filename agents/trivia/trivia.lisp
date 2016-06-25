#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.trivia)

(defvar *questions* (make-hash-table :test 'eql))
(defvar *categories* (make-hash-table :test 'equalp))

(defclass question ()
  ((text :initarg :text :accessor text)
   (answers :initarg :answers :accessor answers)
   (hint :initarg :hint :accessor hint)
   (id :initarg :id :accessor id))
  (:default-initargs :id (incf (gethash :next-id *questions* 0))))

(defmethod id ((id integer))
  id)

(defmethod check (answer (question question))
  (loop for correct in (answers question)
        thereis (search correct answer :test #'char-equal)))

(defun question (id)
  (gethash id *questions*))

(defun (setf question) (question id)
  (setf (gethash (id id) *questions*) question))

(defun remquestion (id)
  (remhash id *questions*))

(defun category (category)
  (mapcar #'question (or (gethash category *categories*)
                       (error "No such category ~s." category))))

(defun add-category (category id)
  (pushnew (id id) (gethash category *categories*)))

(defun remove-category (category &optional id)
  (if id
      (setf (gethash category *categories*)
            (remove (id id) (gethash category *categories*)))
      (remhash category *categories*)))

(defun categories (&optional id)
  (if id
      (loop for k being the hash-keys of *categories*
            for v being the hash-values of *categories*
            when (find id v) collect k)
      (loop for k being the hash-keys of *categories* collect k)))

(defun add-question (question answers &key hint categories)
  (let ((question (make-instance 'question :text question
                                           :answers (if (listp answers) answers (list answers))
                                           :hint hint)))
    (dolist (category categories)
      (add-category category question))
    (setf (question question) question)))

(defun update-question (id &key question answers (hint NIL hint-p) (categories NIL cat-p))
  (let ((q (question id)))
    (when q
      (when question (setf (question q) question))
      (when answers (setf (answers q) answers))
      (when hint-p (setf (hint q) hint))
      (when cat-p
        (dolist (category (categories)) (remove-category category q))
        (dolist (category categories) (add-category category q)))
      q)))

(defun remove-question (id)
  (maphash (lambda (k v) (setf (gethash k *categories*) (remove id v))) *categories*)
  (remquestion id))

(defun load-questions ()
  (setf *questions* (maiden-storage:restore 'question))
  (setf *categories* (maiden-storage:restore 'categories)))

(defun save-questions ()
  (maiden-storage:offload 'question *questions*)
  (maiden-storage:offload 'categories *categories*))
