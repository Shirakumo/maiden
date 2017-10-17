#|
 This file is a part of Maiden
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.lookup)

(defvar *lookup-functions* (make-hash-table :test 'equalp))

(defun lookup-function (archive)
  (or (gethash (string archive) *lookup-functions*)
      (error "Unknown archive ~s." archive)))

(defun (setf lookup-function) (function archive)
  (setf (gethash (string archive) *lookup-functions*) function))

(defun remove-lookup-function (archive)
  (remhash (string archive) *lookup-functions*))

(defun list-archives ()
  (loop for archive being the hash-keys of *lookup-functions*
        collect archive))

(defmacro define-lookup-function (archive args &body body)
  `(setf (lookup-function ',archive)
         (lambda ,args ,@body)))

(defun look-up (archive term)
  (funcall (lookup-function archive) term))

(defmacro define-webpage-lookup (archive args &body body)
  `(define-lookup-function ,archive ,args
     (multiple-value-bind (root code headers url)
         (request-as :html (progn ,@body))
       (declare (ignore headers))
       (when (/= code 200)
         (error "~s not found in ~a." ,(first args) ',archive))
       (list (list ,(first args)
                   (puri:render-uri url NIL)
                   (lquery:$1 root "title" (text)))))))

(defun longest (things)
  (let ((longest (first things)))
    (dolist (thing (rest things) longest)
      (when (< (length longest) (length thing))
        (setf longest thing)))))

(defun table-find (term table)
  (loop for (matches . data) in table
        when (loop for match in matches
                   thereis (or (string-equal term match)))
        collect (list* (longest matches) data)))

(defmacro define-table-lookup (archive &body entries)
  (let ((term (gensym "TERM"))
        (entries (loop for entry in entries
                       collect (destructuring-bind (matches url &optional title) entry
                                 (list (enlist matches)
                                       url
                                       (or title (format NIL "~{~@(~a~)~^ ~}" (enlist matches))))))))
    `(define-lookup-function ,archive (,term)
       (or (table-find ,term ',entries)
           (error "~s not found in ~a." ,term ',archive)))))
