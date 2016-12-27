#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.silly)

(docs:define-docs
  (type silly
    "This module implements silly commands and responses.")

  (command eight
    "8")

  (command jerkcity
    "Respond with a randomly selected jerkcity comic strip.")

  (command roll
    "Roll some dice.   Note that this is not provided with the intention of providing gambling means.")

  (command fortune
    "Display the fortune of today for you, or a user. It changes daily!"))
