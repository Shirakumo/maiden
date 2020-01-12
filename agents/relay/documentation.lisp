#|
 This file is a part of Maiden
 (c) 2020 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.relay)

(docs:define-docs
  (type relay
    "Allows relaying messages from a remote channel.")
  (function mappings
    "Returns the hash table from source channels to target relay mappings.")
  (type mapping
    "Information about a relay mapping to another channel.")
  (function prefix-id
    "Whether to prefix the channel ID in the relay message.")
  (function prefix-user
    "Whether to prefix the username in the relay message.")
  (command activate
    "Start relaying messages from another channel visited by the bot.")
  (command deactivate
    "Stop relaying messages from a remote channel."))
