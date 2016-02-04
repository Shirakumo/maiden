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

(define-compiler-macro p (&rest strings)
  `(progn ,@(loop for string in strings
                  collect `(write-string ,string *serialize-output*))))

(defun pc (&rest things)
  (dolist (thing things)
    (princ thing *serialize-output*)))

(defun p1 (&rest things)
  (dolist (thing things)
    (prin1 thing *serialize-output*)))

(defun pe (string &optional (transform #'identity))
  (loop for char across string
        for obj = (funcall transform char)
        do (when (find obj "#:()[]{}<>\\\" ")
             (write-char #\\ *serialize-output*))
           (write-char obj *serialize-output*)))

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
  (pe (symbol-name object) #'char-downcase))

(defmethod serialize-object ((object real))
  (p1 object))

(defmethod serialize-object ((object complex))
  (p "<:complex ")
  (serialize-object (realpart object))
  (serialize-object (imagpart object))
  (p ">"))

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

(defmethod serialize-object ((object deeds:event))
  (p "<")
  (serialize-object (type-of object))
  (loop for slot in (deeds:class-all-direct-slots (class-of object))
        for name = (c2mop:slot-definition-name slot)
        do (when (typep slot 'deeds:event-direct-slot-definition)
             (p " ") (serialize-object name)
             (p " ") (serialize-object (slot-value object name))))
  (p ">"))

(defmethod serialize-object ((object pathname))
  (p "<:pathname ")
  (pe (namestring object))
  (p ">"))

(defmethod serialize-object ((object package))
  (p "<:package ")
  (pe (package-name object))
  (p ">"))

(defmethod serialize-object ((object character))
  (p "#")
  (write-char object *serialize-output*))

