#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.accounts)

(defun charr (from to)
  (loop for i from from to to collect (code-char i)))

(defun chars (&rest chars)
  (mapcar #'code-char chars))

(defparameter *illegal-account-name-chars*
  (append ;; Disallow things problematic for filenames
   (list #\\ #\/ #\~ #\. #\' #\")
   ;; We disallow all spacing and all unicode control characters.
   (charr #x0000 #x0020)
   (charr #x0080 #x009F)
   (charr #x2000 #x200D)
   (charr #x2028 #x202F)
   (charr #xFFF0 #xFFFF)
   (chars #x007F #x061C #x1680 #x180E #x205F #x2060 #x3000 #xFEFF)))

(defvar *accounts* (make-hash-table :test 'equalp))

(defclass account (named-entity)
  ((identities :initform () :accessor identities)
   (data :initform (make-hash-table :test 'equal) :reader account-data)))

(defmethod ubiquitous:field ((account account) field &optional default)
  (gethash field (account-data data) default))

(defmethod (setf ubiquitous:field) (value (account account) field)
  (setf (gethash field (account-data data)) value))

(defmethod add-identity (account-ish (event sender-event))
  (add-identity account-ish (sender event)))

(defmethod add-identity (account-ish (user user))
  (add-identity account-ish (cons (name (client user)) (name user))))

(defmethod add-identity (account-ish (identity cons))
  (add-identity (ensure-account account-ish) identity))

(defmethod add-identity ((account account) (identity cons))
  (pushnew identity (identities account) :test #'equalp))

(defmethod remove-identity (account-ish (event sender-event))
  (remove-identity account-ish (sender event)))

(defmethod remove-identity (account-ish (user user))
  (remove-identity account-ish (cons (name (client user)) (name user))))

(defmethod remove-identity (account-ish (identity cons))
  (remove-identity (ensure-account account-ish) identity))

(defmethod remove-identity ((account account) (identity cons))
  (setf (identities account) (remove identity (identities account) :test #'equalp)))

(defun normalize-account-name (name)
  (remove-if (lambda (c) (find c *illegal-account-name-chars*))
             (string-downcase name)))

(defun account-pathname (name)
  (maiden-storage:config-pathname
   (make-pathname :name (string-downcase name)
                  :type "lisp"
                  :directory '(:relative "account"))))

(defun stored-account-pathnames ()
  (directory
   (maiden-storage:config-pathname
    (make-pathname :name pathname-utils:*wild-component*
                   :type "lisp"
                   :directory '(:relative "account")))))

(defmethod maiden-storage:config-pathname ((account account))
  (account-pathname (name account)))

(defun account (name &key (error T))
  (let ((name (normalize-account-name name)))
    (or (gethash name *accounts*)
        (handler-case
            (setf (gethash name *accounts*)
                  (ubiquitous:restore (account-pathname name)))
          (ubiquitous:no-storage-file () NIL))
        (when error (error "No such account ~s." name)))))

(defun ensure-account (account-ish)
  (etypecase account-ish
    (account account-ish)
    (string (ensure-account (account account-ish)))
    (symbol (ensure-account (string account-ish)))))

(defun create-account (name)
  (let ((name (normalize-account-name name)))
    (when (account name :error NIL)
      (error "Account ~s already exists." name))
    (let ((account (make-instance 'account :name name)))
      (ubiquitous:offload (account-pathname name) "lisp" account)
      (setf (gethash name *accounts*) account))))

;; Reload all from files
(dolist (pathname (stored-account-pathnames))
  (let ((account (ubiquitous:restore pathname)))
    (setf (gethash (name account) *accounts*) account)))
