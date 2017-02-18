#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.accounts)

;; account.lisp
(docs:define-docs
  (function charr
    "Return a list of all characters in the specified code range.")

  (function chars
    "Return a list of all characters from the given code chars.")

  (variable *illegal-account-name-chars*
    "A list of characters that are not allowed in account names for security purposes.")

  (function normalize-account-name
    "Remove all illegal characters from the account name.

See *ILLEGAL-ACCOUNT-NAME-CHARS*")

  (variable *accounts*
    "Map of names to account objects.")

  (variable *identity-account-map*
    "Map of identity strings to account objects.")

  (type account
    "This class represents an account.

Accounts are things by which users can be linked across
different networks and different identities. Users can
also store information and preferences in an account,
which can then be reused by other parts of the system to
provide better user interaction.

See IDENTITIES
See PASSWORD
See MAIDEN:NAMED-ENTITY
See MAIDEN:DATA-ENTITY")

  (function identities
    "Accessor to the list of identities that this account is tied to.

An identity attempts to identify a user object on a
client. This way, an authenticated user on a client
can use an account directly without the need for a
manual login.

See ACCOUNT")

  (function password
    "Accessor to the password of the account.

See ACCOUNT")

  (function offload
    "Store the account object to file to ensure it is persisted.")

  (function identity
    "Construct an identity string for the given object.")

  (function add-identity
    "Add a new identity to the account.

See ACCOUNT
See IDENTITY")

  (function remove-identity
    "Remove an identity from an account.

See ACCOUNT
See IDENTITY")

  (function identity-p
    "Returns whether the given identity is one for the given account.

See ACCOUNT
See IDENTITY")

  (function account-pathname
    "Return the proper pathname to the storage file for an account of the given name.

See MAIDEN-STORAGE:CONFIG-PATHNAME
See NORMALIZE-ACCOUNT-NAME")

  (function account
    "Return the account object suitable for the given object.

If :ERROR is non-NIL and no suitable account for the
given object can be found, a condition of type NO-
ACCOUNT-FOR-IDENTITY is signalled.")

  (function delete-account
    "Completely delete the given account.

This will also delete the storage file on disk, so
the account will be gone for good once its object is
garbage-collected.")

  (function load-all-accounts
    "Load all the account objects from disk and make sure they're ready for use."))

;; conditions.lisp
(docs:define-docs
  (type account-condition
    "Superclass for all conditions relating to the account system.")

  (type field-access-denied
    "Condition signalled when a field is attempted to be accessed, which is not accessible to the user.

See ACCOUNT-CONDITION")

  (type no-account-for-identity
    "Condition signalled when no matching account can be found for the identity.

See ACCOUNT-CONDITION")

  (type account-not-found
    "Condition signalled when no account can be found for a given name or object.

See ACCOUNT-CONDITION")

  (type account-already-exists
    "Condition signalled when an account is attempted to be created, but already exists.

See ACCOUNT-CONDITION"))

;; fields.lisp
(docs:define-docs
  (variable *field-info*
    "This table maps field names to field-info instances.

See FIELD-INFO")

  (type field-info
    "This class represents metadata information about an account field.

Using this info, the access to a field can be
restricted and the field's purpose can be documented.

See NAME
See CL:DOCUMENTATION
See ACCESS")

  (function access
    "Reader for the access information on the field-info.

Should be a property list mapping readers to access
permissions. A reader can be one of :OWNER :WORLD.
Access permissions can be :R :RW or NIL, describing
read-only, read-write, or no access respectively.

See FIELD-INFO")

  (function field-info
    "Accessor for the field-info object for the given field name.

See FIELD-INFO")

  (function remove-field-info
    "Remove the field-info object for the given field.

See FIELD-INFO")

  (function define-fields
    "Define account field information.

Each field must be a lambda-list of the following
structure:
  (name (&key owner world) &optional documentation)

See FIELD-INFO
See ACCESS")

  (function access-p
    "Returns true if the given user has access to the given field and account.

See ACCOUNT
See FIELD-INFO")

  (function field
    "Access the given field on the account, using the user as the accessing identity.

See ACCESS-P
See FIELD-INFO
See ACCOUNT"))

;; interface.lisp
(docs:define-docs
  (type accounts
    "This agent handles user accounts and related information. User accounts can be used to store information about a user, allowing the bot to tailor responses to them. Accounts also allow authentication of users on servers where no native authentication is possible.")

  (function random-string
    "Creates a string of random alphanumeric characters of the requested length.")
  
  (command login
    "Allows you to login in to an existing account in case your identity does not log you in automatically. If your identity is not associated, you might need to supply the account name.")

  (command logout
    "Explicitly log out from an account.")

  (command create
    "Create a new account. Accounts allow you to store information about yourself that may help the bot to better tailor responses to you. An account is also necessary for permission management. If no password is specified, one is randomly generated for you. Be sure not to do this on a public channel.")

  (command destroy
    "Destroy your own account. You need to be logged in in order to do this.")

  (command update-password
    "Change your account password. Be sure not to do this on a public channel.")

  (command associate
    "Associate the current identity with your account for automatic login. The identity must be authenticated by an outside source.")

  (command deassociate
    "Deassociate the current identity from your account, removing its automatic login.")

  (command field
    "Retrieve the value of a user account field.")

  (command set-field
    "Update the value of a user account field.")

  (command test-authentication
    "Test whether the bot can verify the authenticity of a user, or yourself."))
