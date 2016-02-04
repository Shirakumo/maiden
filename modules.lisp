#|
 This file is a part of Colleen
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.colleen)

(defun normalize-fqdn (name)
  (string-trim
   "-/"
   (left-trim-string
    "colleen"
    (left-trim-string
     "org/shirakumo/"
     (map 'string (lambda (c) (if (char= c #\.) #\/ (char-downcase c))) name)))))

(defun package-path (package)
  (let ((parts (split #\/ (normalize-fqdn (package-full-name package)))))
    (make-pathname :name (or (car (last parts)) "default")
                   :defaults (apply #'pathname-utils:subdirectory NIL (butlast parts)))))

(defmethod ubiquitous:designator-pathname ((package package) type)
  (merge-pathnames (or (modularize:module-storage package 'storage)
                       (package-path package))
                   (pathname-utils:downwards (ubiquitous:config-pathname type) "colleen")))

(modularize:define-option-expander storage (package storage)
  (setf (module-storage package 'storage) storage))

(defmacro with-storage ((&optional (designator *package*)) &body body)
  `(ubiquitous:with-local-storage (,designator)
     ,@body))
