#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.accounts)

(define-condition account-condition (condition) ())

(define-condition field-access-denied (account-condition error)
  ((user :initarg :user)
   (field :initarg :field)
   (account :initarg :account))
  (:report (lambda (c s) (format s "~a is not permitted to access the field ~a on ~a."
                                 (name (slot-value c 'user))
                                 (slot-value c 'field)
                                 (name (slot-value c 'account))))))

(define-condition no-account-for-identity (account-condition error)
  ((identity :initarg :identity))
  (:report (lambda (c s) (format s "The identity ~s does not have any account associated with it."
                                 (slot-value c 'identity)))))

(define-condition account-not-found (account-condition error)
  ((name :initarg :name))
  (:report (lambda (c s) (format s "No account with name ~s found."
                                 (slot-value c 'name)))))

(define-condition account-already-exists (account-condition error)
  ((name :initarg :name))
  (:report (lambda (c s) (format s "An account with the name ~s already exists."
                                 (slot-value c 'name)))))
