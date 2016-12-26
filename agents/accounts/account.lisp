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

(defun normalize-account-name (name)
  (remove-if (lambda (c) (find c *illegal-account-name-chars*))
             (string-downcase (etypecase name
                                (string name)
                                (account (name name))))))

(defvar *accounts* (make-hash-table :test 'equal))
(defvar *identity-account-map* (make-hash-table :test 'equalp))

(defclass account (named-entity data-entity)
  ((identities :initform () :accessor identities)
   (password :initarg :password :accessor password))
  (:default-initargs
   :password (error "PASSWORD required.")))

(defmethod initialize-instance :after ((account account) &key)
  (setf (name account) (normalize-account-name (name account)))
  (when (gethash (name account) *accounts*)
    (error "An account with the name ~s already exists."
           (name account)))
  (setf (account (name account)) account))

(defmethod offload ((account account))
  (ubiquitous:offload (account-pathname account) :lisp account))

(defmethod offload (account-ish)
  (offload (account account-ish)))

(defmethod (setf data-value) :after (value field (account account))
  (offload account))

(defmethod data-value ((field symbol) (account account))
  (data-value (normalize-field-name field) account))

(defmethod identity ((event user-event))
  (identity (user event)))

(defmethod identity ((user user))
  (cons (name (client user)) (name user)))

(defmethod identity ((cons cons))
  (when (typep (car cons) 'client)
    (setf (car cons) (name (car cons))))
  (when (typep (cdr cons) 'user)
    (setf (cdr cons) (name (cdr cons))))
  cons)

(defmethod add-identity (account-ish identity-ish)
  (add-identity account-ish (identity identity-ish)))

(defmethod add-identity (account-ish (identity cons))
  (add-identity (account account-ish) identity))

(defmethod add-identity ((account account) (identity cons))
  (let ((identity (identity identity)))
    (when (gethash identity *identity-account-map*)
      (error "The identity ~s is already linked to an account!" identity))
    (pushnew identity (identities account) :test #'equalp)
    (setf (gethash identity *identity-account-map*) account)))

(defmethod remove-identity (account-ish identity-ish)
  (remove-identity account-ish (identity identity-ish)))

(defmethod remove-identity (account-ish (identity cons))
  (remove-identity (account account-ish) identity))

(defmethod remove-identity ((account account) (identity cons))
  (let ((identity (identity identity)))
    (setf (identities account) (remove identity (identities account) :test #'equalp))
    (remhash identity *identity-account-map*)))

(defmethod identity-p (account-ish identity-ish)
  (identity-p account-ish (identity identity-ish)))

(defmethod identity-p (account-ish (identity cons))
  (identity-p (account account-ish) identity))

(defmethod identity-p ((account account) (identity cons))
  (find (identity identity) (identities account) :test #'equalp))

(defun account-pathname (name)
  (maiden-storage:config-pathname
   (make-pathname :name (normalize-account-name name)
                  :type "lisp"
                  :directory '(:relative "account"))))

(defmethod maiden-storage:config-pathname ((account account))
  (account-pathname (name account)))

(defmethod account ((account account) &key error)
  (declare (ignore error))
  account)

(defmethod account ((user user) &key error)
  (or (data-value 'account user)
      (and (authenticated-p user)
           (setf (account user) (account (identity user) :error error)))))

(defmethod account ((identity cons) &key (error T))
  (let ((identity (identity identity)))
    (or (gethash identity *identity-account-map*)
        (when error (error 'no-account-for-identity :identity identity)))))

(defmethod account ((name symbol) &key error)
  (account (string name) :error error))

(defmethod account ((name string) &key (error T))
  (let ((name (normalize-account-name name)))
    (or (gethash name *accounts*)
        (handler-case
            (setf (account name) (ubiquitous:restore (account-pathname name)))
          (ubiquitous:no-storage-file () NIL))
        (when error (error 'account-not-found :name name)))))

(defmethod (setf account) (account (user user))
  (setf (data-value 'account user) account))

(defmethod (setf account) (account (name symbol))
  (setf (account (string name)) account))

(defmethod (setf account) (account (name string))
  (dolist (identity (identities account))
    (setf (account identity) account))
  (setf (gethash name *accounts*) account)
  (offload account))

(defmethod (setf account) (account (identity cons))
  (add-identity account identity))

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
    (with-simple-restart (abort "Abort restoring the account from ~a." pathname)
      (let ((account (ubiquitous:restore pathname)))
        (setf (account (name account)) account)))))

(load-all-accounts)
