#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.notify)

(docs:define-docs
  (type notify
    "This agent implements an offline messaging system. It is useful for reminding people if they are not currently around.")

  (command forget-notes
    "Throws out all notes for yourself, or a specific user. Useful if you've already seen the notes, don't need them anymore, or addressed them to the wrong person.")

  (command send-join-note
    "Send a notification message that will be displayed as soon as the user joins a channel the bot is on again.")

  (command send-note
    "Send a notification message that will be displayed as soon as the user speaks again."))
