#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.counter)

(docs:define-docs
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
