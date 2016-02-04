#|
 This file is a part of Colleen
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen.modules.serialize)

(defvar *serialize-output* NIL)

(defun p (&rest strings)
  (dolist (string strings)
    (write-string string *serialize-output*)))

(defun pc (&rest things)
  (dolist (thing things)
    (princ thing *serialize-output*)))

(defun p1 (&rest things)
  (dolist (thing things)
    (prin1 thing *serialize-output*)))

(defun serialize (object)
  (with-output-to-string (*serialize-output*)
    (serialize-object object)))

(defgeneric serialize-object (object))

(defmethod serialize-object ((object cons))
  (p "(")
  (loop for cons on object
        do (serialize-object (car cons))
           (when (cdr cons) (p " ")))
  (p ")"))

(defmethod serialize-object ((object string))
  (p1 object))

(defmethod serialize-object ((object symbol))
  (when (keywordp object)
    (p ":"))
  (loop for char across (symbol-name object)
        do (when (find char ":()[]{}\\\" ")
             (write-char #\\ *serialize-output*))
           (write-char char *serialize-output*)))

(defmethod serialize-object ((object real))
  (p1 object))

(defmethod serialize-object ((object array))
  (p "[")
  (serialize-object (array-dimensions object))
  (loop for i from 0 below (array-total-size object)
        for el = (row-major-aref object i)
        do (p " ") (serialize-object el))
  (p "]"))

(defmethod serialize-object ((object hash-table))
  (p "{")
  (serialize-object (hash-table-test object))
  (loop for k being the hash-keys of object
        for v being the hash-values of object
        do (p " ") (serialize-object k)
           (p " ") (serialize-object v))
  (p "}"))

