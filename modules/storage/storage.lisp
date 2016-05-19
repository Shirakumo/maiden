#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.modules.storage)

(defun package-short-name (package)
  (loop for min = (package-name package) then (if (< (length name) (length min)) name min)
        for name in (package-nicknames package)
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
    (make-pathname :name "default"
                   :type "lisp"
                   :defaults (apply #'pathname-utils:subdirectory NIL parts))))

(defun find-config-directory ()
  (let ((local-config (pathname-utils:subdirectory *root* "config"))
        (global-config (pathname-utils:subdirectory (ubiquitous:config-directory) "maiden")))
    (or (uiop:directory-exists-p local-config)
        (ensure-directories-exist global-config))))

(defmethod config-pathname ((pathname pathname))
  (merge-pathnames pathname (find-config-directory)))

(defmethod config-pathname ((string string))
  (config-pathname (pathname string)))

(defmethod config-pathname ((package package))
  (config-pathname (if (eql package (find-package :KEYWORD))
                       (make-pathname :name "default" :type "lisp")
                       (package-path package))))

(defmethod config-pathname ((symbol symbol))
  (config-pathname (make-pathname :name (string-downcase symbol)
                                  :type "lisp"
                                  :defaults (config-pathname (symbol-package symbol)))))

(defmethod config-pathname ((consumer consumer))
  (config-pathname (class-name (class-of consumer))))

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

(defun ensure-storage (designator)
  (or (storage designator)
      (setf (storage designator) (ubiquitous:restore (config-pathname designator) "lisp"))))

(defmacro with-storage ((designator &key (transaction T) always-load) &body body)
  `(ubiquitous:with-local-storage (,designator
                                   :transaction ,transaction
                                   :type "lisp"
                                   ,@(unless always-load `(:storage (ensure-storage ,designator))))
     ,@body))
