#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.modules.storage)

(defun package-full-name (package)
  (loop for max = ""  then (if (< (length max) (length name)) name max)
        for name in (cons (package-name package) (package-nicknames package))
        finally (return max)))

(defun starts-with (start string)
  (and (<= (length start) (length string))
       (string= start string :end2 (length start))))

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
  (let ((parts (split #\/ (normalize-fqdn (package-full-name package)))))
    (make-pathname :name (or (car (last parts)) "default")
                   :defaults (apply #'pathname-utils:subdirectory NIL (butlast parts)))))

(defmethod ubiquitous:designator-pathname ((package package) type)
  (merge-pathnames (package-path package)
                   (pathname-utils:downwards (ubiquitous:config-pathname type) "maiden")))

(defmacro with-storage ((&optional (designator *package*)) &body body)
  `(ubiquitous:with-local-storage (,designator)
     ,@body))
