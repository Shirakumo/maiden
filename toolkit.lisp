#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(defun kw (name)
  (intern (string name) :keyword))

(defun ensure-list (thing &rest extra-elements)
  (if (listp thing) thing (list* thing extra-elements)))

(defun unlist (thing &key (key #'first))
  (if (consp thing) (funcall key thing) thing))

(defmacro with-default-encoding ((&optional (encoding :UTF-8)) &body body)
  `(let (#+sbcl (sb-impl::*default-external-format* ,encoding)
         #+ccl (ccl:*default-external-format* ,encoding))
     ,@body))

(defun make-updated-list (thing list key test)
  (loop with found = NIL
        with thing-key = (funcall key thing)
        for item in list
        collect (cond ((funcall test (funcall key item) thing-key)
                       (setf found T)
                       thing)
                      (T item)) into new-list
        finally (return (if found new-list (cons thing new-list)))))

(defmacro update-list (thing list &key (key '#'identity) (test '#'eql))
  `(setf ,list (make-updated-list ,thing ,list ,key ,test)))
