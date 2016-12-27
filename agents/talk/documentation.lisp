#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.talk)

(docs:define-docs
  (type talk
    "This agent provides text-to-speech reading.")

  (command talk-en
    "Speak the given text in English. Note that this will be played back on the bot owner's machine.")

  (command talk-lang
    "Speak the given text in the requested language, if possible. Note that this will be played back on the bot owner's machine."))
