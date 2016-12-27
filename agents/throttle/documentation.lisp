#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.throttle)

(docs:define-docs
  (type throttle
    "Provides throttling to prevent spamming the bot with commands.")

  (command view-config
    "Shows the current configuration that the throttling works with.")

  (command set-config
    "Update the configuration values for the throttling behaviour.")

  (command clear-tax
    "Clear the throttling tax from a user."))
