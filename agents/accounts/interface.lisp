#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.accounts)

(define-consumer accounts (agent)
  ())

(defun random-string (length)
  (with-output-to-string (out)
    (loop repeat length do (write-char (elt "abcdefghijklmnopqrstuvwxyz0123456789" (random 36)) out))))

(define-command (accounts login) (c ev password &optional account)
  :command "login"
  (let ((account (account (or account (user ev)))))
    (cond ((gethash 'account (data (user ev)))
           (error "I still remember you, ~a." (name (user ev))))
          ((string/= password (password account))
           (error "Invalid password."))
          (T
           (setf (account (user ev)) account)
           (reply ev "Welcome back, ~a." (name (user ev)))))))

(define-command (accounts create) (c ev &key password name)
  :command "create account"
  (let ((name (if (or (not name) (string= name "")) (name (user ev)) name))
        (password (if (or (not password) (string= password "")) (random-string 6) password)))
    (when (account (user ev) :error NIL)
      (error "This identity is already tied to ~a." (account ev)))
    (let ((account (make-instance 'account :name name :password password)))
      (setf (account (user ev)) account)
      (reply ev "Your account ~a has been created with the password ~s." name password)
      (cond ((identified (user ev))
             (add-identity account ev))
            (T
             (reply ev "Since this identity is not authenticated, you will not be logged in automatically in the future."))))))

(define-command (accounts destroy) (c ev)
  :command "destroy account"
  (let ((account (account (user ev))))
    (when (and (not (gethash 'account (data (user ev))))
               (not (authenticated user)))
      (error "Please log in to this account first."))
    (delete-account account)
    (reply ev "Your account ~a has been deleted." (name account))))

(define-command (accounts associate) (c ev account password)
  :command "associate with account"
  (let ((account (account account)))
    (when (account (user ev) :error NIL)
      (error "This identity is already associated with ~a." (account ev)))
    (unless (authenticated (user ev))
      (error "You must be authenticated to associate this identity with the account."))
    (unless (string= (password account) password)
      (error "Invalid password."))
    (add-identity account ev)
    (setf (account (user ev)) account)
    (reply ev "Identity associated. You will now be automatically logged in.")))

(define-command (accounts deassociate) (c ev)
  :command "deassociate from account"
  (let ((account (account (user ev))))
    (remove-identity account ev)
    (reply ev "Identity deassociated.")))

(define-command (accounts field) (c ev field &optional account)
  :command "show account field"
  (let* ((account (account (or account (user ev))))
         (value (field field account (user ev))))
    (if value
        (reply ev "The field is not set to anything." field)
        (reply ev "The field is set to ~s." value))))

(define-command (accounts set-field) (c ev field value &optional account)
  :command "set account field"
  (let ((account (account (or account (user ev)))))
    (setf (field field account (user ev)) value)
    (reply ev "The field has been set to ~s." value)))
