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

(defvar *accounts* (make-hash-table :test 'equal))
(defvar *identity-account-map* (make-hash-table :test 'equalp))

(defclass account (named-entity)
  ((identities :initform () :accessor identities)
   (data :initform (make-hash-table :test 'equal) :reader account-data)))

(defmethod ubiquitous:field ((account account) field &optional default)
  (gethash (string-downcase field) (account-data data) default))

(defmethod (setf ubiquitous:field) (value (account account) field)
  (setf (gethash (string-downcase field) (account-data data)) value))

(defmethod ensure-identity ((event user-event))
  (ensure-identity (user event)))

(defmethod ensure-identity ((user user))
  (cons (name (client user)) (name user)))

(defmethod ensure-identity ((cons cons))
  cons)

(defmethod add-identity (account-ish identity-ish)
  (add-identity account-ish (ensure-identity identity-ish)))

(defmethod add-identity (account-ish (identity cons))
  (add-identity (account account-ish) identity))

(defmethod add-identity ((account account) (identity cons))
  (pushnew identity (identities account) :test #'equalp)
  (setf (gethash identity *identity-account-map*) account))

(defmethod remove-identity (account-ish identity-ish)
  (remove-identity account-ish (ensure-identity identity-ish)))

(defmethod remove-identity (account-ish (identity cons))
  (remove-identity (account account-ish) identity))

(defmethod remove-identity ((account account) (identity cons))
  (setf (identities account) (remove identity (identities account) :test #'equalp))
  (remhash identity *identity-account-map*))

(defmethod identity-p (account-ish identity-ish)
  (identity-p account-ish (ensure-identity identity-ish)))

(defmethod identity-p (account-ish (identity cons))
  (identity-p (account account-ish) identity))

(defmethod identity-p ((account account) (identity cons))
  (find cons (identities account) :test #'equalp))

(defun normalize-account-name (name)
  (remove-if (lambda (c) (find c *illegal-account-name-chars*))
             (string-downcase (etypecase name
                                (string name)
                                (account (name name))))))

(defun account-pathname (name)
  (maiden-storage:config-pathname
   (make-pathname :name (normalize-account-name name)
                  :type "lisp"
                  :directory '(:relative "account"))))

(defmethod maiden-storage:config-pathname ((account account))
  (account-pathname (name account)))

(defmethod account ((account account) &key)
  account)

(defmethod account ((user user) &rest args)
  (apply #'account (ensure-identity user) args))

(defmethod account ((identity cons) &key (error T))
  (or (gethash identity *identity-account-map*)
      (when error (error 'no-account-for-identity :identity identity))))

(defmethod account ((name symbol) &rest args)
  (apply #'account (string name) args))

(defmethod account ((name string) &key (error T))
  (let ((name (normalize-account-name name)))
    (or (gethash name *accounts*)
        (handler-case
            (setf (account name) (ubiquitous:restore (account-pathname name)))
          (ubiquitous:no-storage-file () NIL))
        (when error (error 'account-not-found :name name)))))

(defmethod (setf account) (account (name symbol))
  (setf (account (string name)) account))

(defmethod (setf account) (account (name string))
  (dolist (identity (identities account))
    (setf (account identity) account))
  (setf (gethash name *accounts*) account))

(defmethod (setf account) (account (user user))
  (setf (account (ensure-identity user)) account))

(defmethod (setf account) (account (identity cons))
  (add-identity account identity))

(defun create-account (name)
  (let ((name (normalize-account-name name)))
    (when (account name :error NIL)
      (error 'account-already-exists :name name))
    (let ((account (make-instance 'account :name name)))
      (ubiquitous:offload (account-pathname name) "lisp" account)
      (setf (account name) account))))

(defun delete-account (account-ish)
  (let ((account (account account-ish)))
    (uiop:delete-file-if-exists (account-pathname account))
    (remhash (name account) *accounts*)
    account))

(defun load-all-accounts ()
  (dolist (pathname (directory
                     (maiden-storage:config-pathname
                      (make-pathname :name pathname-utils:*wild-component*
                                     :type "lisp"
                                     :directory '(:relative "account")))))
    (let ((account (ubiquitous:restore pathname)))
      (setf (account (name account)) account))))

(load-all-accounts)
