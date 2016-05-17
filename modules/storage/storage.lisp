#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.modules.storage)

(defun package-short-name (package)
  (loop for min = ""  then (if (< (length name) (length min)) name min)
        for name in (cons (package-name package) (package-nicknames package))
        finally (return min)))

(defun left-trim-string (trim string)
  (if (starts-with trim string)
      (subseq string (length trim))
      string))

(defun split (char string)
  (let ((out (make-string-output-stream))
        (parts ()))
    (flet ((push-part ()
             (let ((part (get-output-stream-string out)))
               (when (string/= part "")
                 (push part parts)))))
      (loop for c across string
            do (if (char= c char)
                   (push-part)
                   (write-char c out))
            finally (push-part)))
    (nreverse parts)))

(defun normalize-fqdn (name)
  (string-trim
   "-/"
   (left-trim-string
    "maiden"
    (left-trim-string
     "org/shirakumo/"
     (map 'string (lambda (c) (if (char= c #\.) #\/ (char-downcase c))) name)))))

(defun package-path (package)
  (let ((parts (split #\/ (normalize-fqdn (package-short-name package)))))
    (make-pathname :name (or (car (last parts)) "default")
                   :defaults (apply #'pathname-utils:subdirectory NIL (butlast parts)))))

(defmethod ubiquitous:designator-pathname ((package package) type)
  (merge-pathnames (package-path package)
                   (pathname-utils:downwards (ubiquitous:config-pathname type) "maiden")))

(defmethod ubiquitous:designator-pathname ((consumer consumer) type)
  (ubiquitous:designator-pathname (type-of consumer) type))

(defun storage (thing)
  (etypecase thing
    (symbol (get thing :storage))
    (string (storage (intern thing :keyword)))
    (package (storage (package-name thing)))
    (consumer (storage (type-of thing)))))

(defun (setf storage) (storage thing)
  (etypecase thing
    (symbol (setf (get thing :storage) storage))
    (string (setf (storage (intern thing :keyword)) storage))
    (package (setf (storage (package-name thing)) storage))
    (consumer (setf (storage (type-of thing)) storage))))

(defun ensure-storage (designator type)
  (or (storage designator)
      (setf (storage designator) (ubiquitous:restore designator type))))

(defmacro with-storage ((&key (designator *package*) type (transaction T) always-load) &body body)
  (let ((type (or type 'ubiquitous:*storage-type*)))
    `(ubiquitous:with-local-storage (,designator
                                     :transaction ,transaction
                                     :type ,type
                                     ,@(unless always-load `(:storage (ensure-storage ',designator ,type))))
       ,@body)))
