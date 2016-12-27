#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.markov)

(docs:define-docs
  (type markov
    "This agent implements a markov-chain based chat bot for amusement. It can learn from the actual chat messages of a channel.")

  (command ramble
    "Generates a random sentence.")

  (command ramble-about
    "Generates a random sentence that should be related to the given topic.")

  (command ramble-chance
    "Display the chance [0,1] for the bot to reply to a message.")

  (command set-ramble-chance
    "Update the chance [0,1] for the bot to reply to a message.")

  (command stats
    "Display some statistics about the dictionary of markov chains."))
