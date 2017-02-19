#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.medals)

(docs:define-docs
  (function maybe-account
    "Attempts to find an account of the given name.

See MAIDEN-ACCOUNTS:ACCOUNT")

  (function user-name
    "Attempts to find the user-name of the thing.

Accepts USER, STRING, SYMBOL.")

  (function medals
    "Accessor to the medals carried by the user of the given name.

If the user has an account, the account's
name is used for the storage instead.

See MAYBE-ACCOUNT")

  (function add-medals
    "Adds the given medals to the user of the given name.

See MEDALS")

  (function remove-medals
    "Remove the given medals from the user of the given name.

See MEDALS")
  
  (type medals
    "This implements a simple 'medal' system, where users can be awarded random medals.")

  (command show
    "Displays the awarded medals for a user or yourself.")

  (command award
    "Award medals to a user. The medals can be literally anything.")

  (command take
    "Take away medals from a user."))
