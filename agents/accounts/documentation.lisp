#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.accounts)

(docs:define-docs
  (type accounts
    "This agent handles user accounts and related information. User accounts can be used to store information about a user, allowing the bot to tailor responses to them. Accounts also allow authentication of users on servers where no native authentication is possible.")
  
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
    "Update the value of a user account field."))
