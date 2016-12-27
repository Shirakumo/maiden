#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.emoticon)

(docs:define-docs
  (type emoticon
    "This agent provides 'emoticons' as seen on forums. Emoticons are tokens like :this: that the bot replies to with a saved response. This can also serve as a convenient shortcut mechanism.")

  (command add
    "Add a new emoticon mapping. If you want your name or emoticon text to contain spaces or such, supply it as a string encased in double-quotes.")

  (command change
    "Change the reply for an emoticon mapping.")

  (command remove
    "Remove an existing emoticon mapping.")

  (comand list
    "List the names of all available emoticon mappings."))
