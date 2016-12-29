#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.core-manager)

(docs:define-docs
  (type core-manager
    "This agent handles the management of consumers on the bot's core. Specifically it allows adding, removing, starting, and stopping consumers.")
  
  (command start-consumer
    "Start an existing consumer of the given name on the current core.")

  (command stop-consumer
    "Stop an existing consumer of the given name on the current core.")

  (command remove-consumer
    "Completely remove an existing consumer of the given name from the current core.")

  (command add-consumer
    "Create and add a new consumer to the current core.")

  (command list-consumers
    "Show a list of all consumers on the current core by their names or IDs.")

  (command stop-core
    "Stop the current core completely. Note that this will effectively shut down the bot, but not the lisp instance."))
