#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.modules.convert)

(defgeneric convert (thing type))

(defmethod convert (thing type)
  (error "Don't know hot to interpret ~s as ~s." thing type))

(defmacro define-conversion (type &body cases)
  `(progn
     ,@(loop for (class . body) in cases
             collect `(defmethod convert ((,class ,class) (type (eql ',type)))
                        ,@body))))

(define-conversion string
  (string string)
  (symbol (string symbol))
  (pathname (namestring pathname))
  (list (format NIL "~{~a ~}" list)))

(define-conversion integer
  (integer integer)
  (real (round real))
  (complex (round (realpart complex)))
  (string (parse-integer string)))

(define-conversion float
  (float float)
  (real (float real))
  (complex (round (realpart complex)))
  (string (float (parse-number:parse-real-number string))))

(define-conversion real
  (real real)
  (string (parse-number:parse-real-number string)))

(define-conversion number
  (number number)
  (string (parse-number:parse-number string)))

(define-conversion pathname
  (pathname pathname)
  (string (uiop:parse-native-namestring string)))

(define-conversion symbol
  (symbol symbol)
  (string (intern string)))

(define-conversion keyword
  (symbol (if (keywordp symbol) symbol (intern (string symbol) :keyword)))
  (string (intern string :keyword)))

(define-conversion list
  (list list)
  (vector (coerce vector 'list)))

(define-conversion vector
  (vector vector)
  (list (coerce list 'vector)))

(defmacro with-conversion (cases &body body)
  `(let ,(loop for (type . vars) in cases
               nconc (loop for var in vars collect `(,var (convert ,var ',type))))
     ,@body))
