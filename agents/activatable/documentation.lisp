#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.activatable)

(docs:define-docs
  (type activatable
    "This agent manages the activation of other agents that might want to be blocked on certain channels to avoid unwanted responses.")
  
  (command activate
    "Activate modules on the current channel. By default modules are deactivated.")

  (command deactivate
    "Deactivate modules on the current channel."))
