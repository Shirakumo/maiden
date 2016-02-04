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
