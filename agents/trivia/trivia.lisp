#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.trivia)

(defvar *trivia* (make-hash-table :test 'eql))
(defvar *categories* (make-hash-table :test 'equalp))

(defclass trivia ()
  ((question :initarg :question :accessor question)
   (answers :initarg :answers :accessor answers)
   (hint :initarg :hint :accessor hint)
   (id :initarg :id :accessor id))
  (:default-initargs :id (incf (gethash :next-id *trivia*))))

(defmethod id ((id integer))
  id)

(defmethod check (answer (trivia trivia))
  (loop for correct in (answers trivia)
        thereis (search correct answer :test #'char-equal)))

(defun trivia (id)
  (gethash id *trivia*))

(defun (setf trivia) (trivia id)
  (setf (gethash (id id) *trivia*) trivia))

(defun remtrivia (id)
  (remhash id *trivia*))

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

(defun load-trivia ()
  (setf *trivia* (maiden-storage:restore 'trivia))
  (setf *categories* (maiden-storage:restore 'categories)))

(defun save-trivia ()
  (maiden-storage:offload 'trivia *trivia*)
  (maiden-storage:offload 'categories *trivia*))

(defun add-trivia (question answers &key hint categories)
  (let ((trivia (make-instance 'trivia :question question
                                       :answers (if (listp answers) answers (list answers))
                                       :hint hint)))
    (dolist (category categories)
      (add-category category trivia))
    (setf (trivia trivia) trivia)))

(defun update-trivia (id &key question answers (hint NIL hint-p) (categories NIL cat-p))
  (let ((trivia (trivia id)))
    (when trivia
      (when question (setf (question trivia) question))
      (when answers (setf (answers trivia) answers))
      (when hint-p (setf (hint trivia) hint))
      (when cat-p
        (dolist (category (categories)) (remove-category category trivia))
        (dolist (category categories) (add-category category trivia)))
      trivia)))

(defun remove-trivia (id)
  (maphash (lambda (k v) (setf (gethash k *categories*) (remove id v))) *categories*)
  (remtrivia id))
