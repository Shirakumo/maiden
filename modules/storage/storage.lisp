#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.modules.storage)

(defvar *storages* (make-hash-table :test 'eql))

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

(defgeneric storage (thing))
(defgeneric (setf storage) (storage thing))

(defmethod storage ((thing symbol))
  (gethash thing *storages*))

(defmethod storage ((thing string))
  (storage (intern thing :keyword)))

(defmethod storage ((thing package))
  (storage (package-name thing)))

(defmethod storage ((thing consumer))
  (storage (type-of thing)))

(defmethod (setf storage) (storage (thing symbol))
  (setf (gethash thing *storages*) storage))

(defmethod (setf storage) (storage (thing string))
  (setf (storage (intern thing :keyword)) storage))

(defmethod (setf storage) (storage (thing package))
  (setf (storage (package-name thing)) storage))

(defmethod (setf storage) (storage (thing consumer))
  (setf (storage (type-of thing)) storage))

(defun ensure-storage (designator)
  (or (storage designator)
      (setf (storage designator) (restore designator))))

(defmacro with-storage ((designator &key (transaction T) always-load) &body body)
  `(ubiquitous:with-local-storage ((config-pathname ,designator)
                                   :transaction ,transaction
                                   :type :lisp
                                   ,@(unless always-load `(:storage (ensure-storage ,designator))))
     ,@body))

(defun reload (&optional designator)
  (if designator
      (setf (storage designator) NIL)
      (clrhash *storages*)))

(defun offload (designator &optional (storage (storage designator)))
  (let ((*package* #.(find-package '#:maiden-user)))
    (ubiquitous:offload (config-pathname designator) :lisp storage)))

(defun restore (designator)
  (let ((*package* #.(find-package '#:maiden-user)))
    (ubiquitous:restore (config-pathname designator) :lisp)))

(defmacro define-stored-accessor (class accessor &rest path)
  (let ((path (or path (list accessor))))
    `(defmethod (setf ,accessor) :after (value (,class ,class))
       (with-storage (,class)
         (setf (value ,@path) value)))))
