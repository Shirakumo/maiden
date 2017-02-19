#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.counter)

(docs:define-docs
  (function counter
    "Direct accessor to the counter storage.

A counter is a plist of the following keys:
- :NAME      The name of the counter
- :MATCH     The regex that matches an applicable message.
- :RESPONSE  The response string to use when a message matches. Should be a format string that contains a single placeholder that is filled with the current counter value.
- :COUNT     The actual counter value of how many times the counter was invoked.

See REMOVE-COUNTER
See LIST-COUNTERS
See SET-COUNTER")

  (function remove-counter
    "Remove a counter from the storage.

See COUNTER
See LIST-COUNTERS")

  (function list-counters
    "Return a list of all the counters in the storage.

See COUNTER
See REMOVE-COUNTER")

  (function set-counter
    "More conveniently update or create a counter in the storage.

See COUNTER")
  
  (type counter
    "This agent provides a simple regex based matcher that increases a counter every time it sees a matching message.")
  
  (command add
    "Add a new counter. MATCH should be a regular expression. RESPONSE, if specified, is the message displayed when the counter matches.")

  (command change
    "Update an existing counter definition.")

  (command remove
    "Remove an existing counter definition.")

  (command list
    "List the names of all existing counter definitions."))
