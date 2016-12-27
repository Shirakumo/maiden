#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.chatlog)

(docs:define-docs
  (type chatlog
    "This agent handles the recording of messages and activity in a channel to a database, potentially for public viewing on the web.")
  
  (command activate
    "Activate chat logging for the current channel.")

  (command activate-on
    "Activate logging on a specific client's channel.")

  (command deactivate
    "Deactivate chat logging on the current channel.")

  (command deactivate-on
    "Deactivate chat logging on a specific client's channel.")

  (command initialize
    "Set up the chatlog's postgres connection details."))
