(in-package #:org.shirakumo.maiden.agents.blocker)

(docs:define-docs
  (variable *clauses*
    "This holds an alist mapping clause names to clause resolver functions.

Each such function must take one or more arguments,
where the first argument is the event, and the others
are arguments of the clause.

See CLAUSE
See REMOVE-CLAUSE
See DEFINE-CLAUSE")

  (function clause
    "Accessor to set or retrieve clause resolution functions.

See *CLAUSES*")

  (Function remove-clause
    "Remove a clause so that it is no longer recognised.

See *CLAUSES*")

  (function define-clause
    "Define a new clause resolution function.

The function should analyse the event and its
arguments and return whether the event passes
the clause or not.")

  (function match-rule
    "Attempt to match the event against the rule.

The rule must be a properly parsed rule in list
form. If it contains an invalid clause, an error
is signalled.")

  (function parse-clause
    "Attempt to parse a clause from the stream.

See PARSE-TOKEN")

  (function parse-word
    "Attempt to parse a word from the stream.")

  (function parse-string
    "Attempt to parse a string from the stream.")

  (function parse-token
    "Attempt to parse a token from the stream.

See PARSE-CLAUSE
See PARSE-STRING
See PARSE-WORD")

  (function parse-rule
    "Attempt to parse the string into a proper rule form.

See PARSE-CLAUSE")

  (function ensure-rule
    "Attempt to coerce the object into a proper rule form.

This will also make sure that the rule form is
a proper one, as it will simulate a matching
against a dummy event.

See ENSURE-RULE
See MATCH-RULE")
  
  (type blocker
    "This agent handles the blocking of users, channels, or commands based on flexible rules.")

  (function rules
    "Accessor to the rules of the blocker.

See BLOCKER")

  (function rule
    "Accessor to the rule instances that the blocker keeps.

See RULES
See BLOCKER")

  (function remove-rule
    "Remove a rule instance from the blocker.

See RULES
See BLOCKER")

  (function add-rule
    "Add a new rule to the blocker.

If a rule of the same name already exists, an
error is signalled.

See RULE")

  (function blocked-p
    "Returns whether the given event is blocked by the blocker.

See RULES
See MATCH-RULE")
  
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
