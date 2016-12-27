#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.blocker)

(docs:define-docs
  (type blocker
    "This agent handles the blocking of users, channels, or commands based on flexible rules.")
  
  (command add-rule
    "Add a new blocking rule. The rule syntax allows AND/OR/NOT logical combinations and CHANNEL/CLIENT/USER/REGEX/PREFIX matchers. Use s-expression style syntax. Eg: (and (channel foo) (not (user bar)))")

  (command block-channel
    "Block a specific channel from all commands. The name of the rule will be the same as the channel.")

  (command block-user
    "Block a specific user from all commands. The name of the rule will be the same as the user name.")

  (command block-regex
    "Block all commands that match a certain regex. The name of the rule will be the same as the regex.")

  (command block-prefix
    "Block all commands that match a certain prefix. The name of the rule will be the same as the prefix string.")

  (command update-rule
    "Update an existing rule definition.")

  (command remove-rule
    "Remove a rule definition that was previously defined.")

  (command view-rule
    "View the actual rule definition.")

  (command view-rules
    "Show a listing of names of all the known block rules."))
