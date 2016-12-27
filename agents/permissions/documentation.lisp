#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.permissions)

(docs:define-docs
  (type permissions
    "This agent provides selective protection of resources by implementing a permissions system. Each user can be granted or denied access to protected resources.")
  
  (command check-access
    "Check whether you have access to a permission branch.")

  (command grant
    "Grant access to a permission branch to a user.")

  (command deny
    "Deny access to a permission branch to a user.")

  (command add-administrator
    "Add a user as an administrator.")

  (command remove-administrator
    "Remove an administrator.")

  (command add-default-permission
    "Register a permission branch that should be granted to all users unless explicitly denied.")

  (command remove-default-permission
    "Remove a permission branch that was set as a default."))
