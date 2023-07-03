(in-package #:org.shirakumo.maiden.agents.emoticon)

(docs:define-docs
  (function emoticon
    "Accessor to the emoticon of the given name.

The value should be the response string for the emote.

See REMOVE-EMOTICON
See LIST-EMOTICONS")

  (function remove-emoticon
    "Remove the emoticon of the given name from the system.

See EMOTICON
See LIST-EMOTICONS")

  (function list-emoticons
    "Return a fresh list of all emoticons that are saved on the system.

See EMOTICON
See REMOVE-EMOTICON")

  (function maximum
    "Accessor to the maximum number of emoticons that are expanded in a single message.

Defaults to 5.

See EMOTICON")
  
  (type emoticon
    "This agent provides 'emoticons' as seen on forums. Emoticons are tokens like :this: that the bot replies to with a saved response. This can also serve as a convenient shortcut mechanism.")

  (command add
    "Add a new emoticon mapping. If you want your name or emoticon text to contain spaces or such, supply it as a string encased in double-quotes.")

  (command change
    "Change the reply for an emoticon mapping.")

  (command remove
    "Remove an existing emoticon mapping.")

  (command list
    "List the names of all available emoticon mappings."))
