#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.permissions)

(docs:define-docs
  (variable *tokens*
    "List of administration granting tokens that are still up for grabs.

See GENERATE-TOKEN")

  (function random-string
    "Generates a random alphanumeric string of the given length.")

  (function generate-token
    "Creates a fresh administration authentication token that can be used to get admin access.

See *TOKENS*")

  (type permission-denied
    "Condition signalled when an attempt is made to access a protected resource the user does not have access to.")

  (function normalize-permission
    "Attempt to normalise the permission designator.

Returns a list that should properly identify the resource.")

  (function separate-grant-deny
    "Attempts to separate the list of permissions into groups of grants and denies.

Denies are permissions that begin with an exclamation mark.

Returns two values, a list of granting permissions, and a
list of denying permissions.")

  (function perm-match-p
    "Returns true if the permission is allowed by one or more of the matches and not denied by any of them.")

  (function user-perm
    "Create a permission that identifies the user, hopefully uniquely.")

  (function administrator-p
    "Returns true if the user is an administrator.

This is the case if the user is in the configuration's
:administrators list.

See MAIDEN-STORAGE:WITH-STORAGE")

  (function add-administrator
    "Add the user as an administrator.

See MAIDEN-STORAGE:WITH-STORAGE")

  (function remove-administrator
    "Remove the user as an administrator.

See MAIDEN-STORAGE:WITH-STORAGE")

  (function add-default-permission
    "Add the permission as a default one that is granted to everyone.

See MAIDEN-STORAGE:WITH-STORAGE")

  (function remove-default-permission
    "Remove the permission as a default one so that it is no longer automatically granted to everyone.

See MAIDEN-STORAGE:WITH-STORAGE")

  (function allowed-p
    "Returns true if the user is allowed access to the given permission.

This is the case if the user is authenticated and
an administrator, or if the permission is covered
by the default permissions or the user's permissions
and is not denied by the user's permissions.

See ADMINISTRATOR-P
See PERM-MATCH-P
See MAIDEN-STORAGE:WITH-STORAGE")

  (function grant
    "Grant the permission to the user.

This will simultaneously add the permission to the
user's list of permissions, and remove any denying
permissions that would match it. Note that this may
thus grant more than you expect, if there is a 
denying permission that denies more than just this
permission.

See NORMALIZE-PERMISSION
See DENY")

  (function deny
    "Deny the permission to the user.

This will simultaneously add the permission to the
user's list of permissions, and remove any granting
permissions that would match it exactly.

See NORMALIZE-PERMISSION
See GRANT")

  (function check-allowed
    "Check whether the user is allowed access to the permission.

If not, an error of type PERMISSION-DENIED is signalled.

See ALLOWED-P
See PERMISSION-DENIED")

  (function with-permission
    "Only execute the body if the user is allowed access to the permission.

See ALLOWED-P")
  
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
    "Remove a permission branch that was set as a default.")

  (command administrate-self
    "Upgrade yourself to an administrator with an active authorization token. You can generate such a token at the repl with GENERATE-TOKEN."))
