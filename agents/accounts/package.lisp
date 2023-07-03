(in-package #:maiden-user)
(defpackage #:maiden-accounts
  (:nicknames #:org.shirakumo.maiden.agents.accounts)
  (:use #:cl #:maiden #:maiden-commands #:maiden-client-entities)
  (:shadow #:identity)
  ;; account.lisp
  (:export
   #:account
   #:identities
   #:password
   #:identity
   #:add-identity
   #:remove-identity
   #:identity-p
   #:account
   #:delete-account)
  ;; conditions.lisp
  (:export
   #:account-condition
   #:field-access-defnied
   #:no-account-for-identity
   #:account-not-found
   #:account-already-exists)
  ;; fields.lisp
  (:export
   #:field-info
   #:name
   #:access
   #:field-info
   #:remove-field-info
   #:define-fields
   #:access-p
   #:field)
  ;; interface.lisp
  (:export
   #:accounts
   #:login
   #:logout
   #:create
   #:destroy
   #:update-password
   #:associate
   #:deassociate
   #:field
   #:set-field
   #:test-authentication))
