#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.commands)

;; dispatch.lisp
(docs:define-docs
  (type commands
    "Agent to allow users to issue commands over messages. It is responsible for detecting, parsing, and issuing the commands as events.")

  (function compare-alternatives
    "Return which of the alternatives is preferable.

Each alternative must be a cons of distance and command-invoker."))

;; extraction.lisp
(docs:define-docs
  (variable *extractors*
    "Alist map of extractor names to extractor functions.

An extractor function must take a single argument, which is the
event to potentially extract a command from. It should return a
sequence to be parsed as a command, should the event indicate a
command of some fashion.

There is a default extractor called PREFIX, which simply checks
the message-event for a :: prefix.

See COMMAND-EXTRACTOR
See REMOVE-COMMAND-EXTRACTOR
See DEFINE-COMMAND-EXTRACTOR")

  (function command-extractor
    "Accessor to set/get the command-extractor of a given name.

See *EXTRACTORS*")

  (function remove-command-extractor
    "Remove the command-extractor of the given name.

See *EXTRACTORS*")

  (function define-command-extractor
    "Define a new command extractor.

See *EXTRACTORS*")

  (function extract-command
    "Attempt to extract a command string from the event.

If this is not possible, NIL is returned.")

  (function command-p
    "Returns true if the event can be interpreted as a command.

See EXTRACT-COMMAND"))

;; invocation.lisp
(docs:define-docs
  (variable *invokers*
    "List of command invokers.

This list must always be sorted by the prefixes of the
command-invokers and by STRING<.

See COMMAND-INVOKER
See REMOVE-COMMAND-INVOKER")

  (variable *framework-client*
    "An instance of a client used for testing and stubbing purposes.

See MAIDEN:CLIENT")

  (variable *framework-user*
    "An instance of a user used for testing and stubbing purposes.

See MAIDEN-CLIENT-ENTITIES:USER")

  (variable *dispatch-event*
    "Holds the original event that dispatched the command event.")

  (variable *alternative-distance-threshold*
    "The maximum distance that an alternative command can be away from the original query to be considered as a suggestion.

See FIND-MATCHING-COMMAND")

  (type framework-message
    "A message event used to simulate commands or issue them without a specific client for them to come from.

See *FRAMEWORK-CLIENT*
See *FRAMEWORK-USER*
See MAIDEN-CLIENT-ENTITIES:MESSAGE-EVENT
See MAIDEN:PASSIVE-EVENT")

  (function issue-message
    "Issue a framework message onto the core.

See FRAMEWORK-MESSAGE")

  (type command-event
    "Superclass for all command events.

The dispatch-event defaults to either *DISPATCH-EVENT*, or
if it is not present, an instance of FRAMEWORK-MESSAGE.

Command events are by default advised to be public. If a
command should not be publicly accessible, you need to set
the advice to (not public).

See DISPATCH-EVENT
See MAIDEN:INSTRUCTION-EVENT
See MAIDEN:ACTIVE-EVENT")

  (function dispatch-event
    "Reader for the original event that lead to the command event being dispatched.

See COMMAND-EVENT")

  (type command-invoker
    "Container class to hold a command invoker.

Command invokers are responsible for parsing a command line
and turning it into a command event instance, which is then
issued onto a core to cause the actual command to be executed.

See NAME
See PREFIX
See LAMBDA-LIST
See INVOKER")

  (function prefix
    "Accessor to the prefix that must be present in the command string for the invoker to be considered for it.

See COMMAND-INVOKER")

  (function lambda-list
    "Accessor to the command invoker's lambda-list.

Note that this is not equal to the invoker's lambda-list.

See COMMAND-INVOKER")

  (function invoker
    "Accessor to the command invoker's invocation function.

The invoker is responsible for parsing the message,
destructuring it into the arguments necessary to produce the
command event, and finally issuing the complete event onto
the same core as the one the event originated from.

The function must take two arguments, the event that caused
the command, and the command string that needs to be parsed.

See COMMAND-INVOKER")

  (function command-invoker
    "Accessor to the command-invoker of the given name.

See COMMAND-INVOKER
See *INVOKERS*")

  (function remove-command-invoker
    "Remove the command-invoker of the given name.

See COMMAND-INVOKER
See *INVOKERS*")

  (function list-command-invokers
    "Return a fresh list of all command-invokers.

See COMMAND-INVOKER
See *INVOKERS*")

  (function find-command-invoker
    "Attempt to find a command-invoker of the given name or prefix.

See *INVOKERS*")

  (function define-command-invoker
    "Define a new command-invoker.

The body defines the command-invoker's invoker function, with
the lambda-list making up the lambda-list for WITH-COMMAND-
DESTRUCTURING-BIND. The message is thus already properly
parsed into the necessary arguments within the body.

See COMMAND-INVOKER
See INVOKER
See WITH-COMMAND-DESTRUCTURING-BIND")

  (function define-simple-command-invoker
    "Define a new, simple command-invoker.

This defines a command-invoker that simple issues a given
event onto the loop, using the arguments to fill in the
initargs of the event. The MESSAGE-EVENT-INITARG option
specifies which initarg, if any, the dispatching event should
be passed to.

See DEFINE-COMMAND-INVOKER")

  (function define-command
    "Define a new command.

This will define a new event for the command, a new handler
for the event that will actually cause the desired code to
be executed, and a command-invoker that is responsible for
parsing the command from a message and issuing the event.

The event will always have COMMAND-EVENT as a superclass,
but you can specify additional superclasses through the
usual body option.

The following additional body options are present and will
not be passed to DEFINE-FUNCTION-HANDLER:
- :COMMAND                
  The prefix string that will cause the command to be parsed
  from a string. This prefix can contain any character
  sequence you want, the system will be able to deal with it.
  Defaults to the command name.
- :COMMAND-EVENT-VARIABLE
  The variable that the command-event is bound to. This is
  not the same as the EVENT argument. The EVENT argument is
  bound to the dispatching event of the command, which is
  usually what you want to inspect and reply to. The command-
  event may be useful in some circumstances, but usually it
  will just be a carrier for the arguments, which will
  already have been bound to distinct variables.

If an error occurs during the execution of the command's
body, a REPLY is done to the dispatching event with the
message set to the error printed as a string.

Within the body, *DISPATCH-EVENT* is bound to the dispatching
event instance.

See REPLY
See *DISPATCH-EVENT*
See MAIDEN:DEFINE-FUNCTION-HANDLER
See DEFINE-SIMPLE-COMMAND-INVOKER
See REMOVE-COMMAND")

  (function remove-command
    "Remove a command from the system.

See REMOVE-FUNCTION-HANDLER
See REMOVE-COMMAND-INVOKER")

  (function flatten-typespec
    "Flattens the typespec to a single list of types.")

  (function consumer-commands
    "Attempts to collect a list of all command invokers that the consumer defined.

This is a heuristic and may not find everything if the
developer of the commands is being sneaky. It works by
going through all handlers defined on the consumer, looking
at each handler's typespec, and checking whether it contains
a type that is a subclass of COMMAND-EVENT. If so, a command-
invoker by the name of the handler is attempted to be found.")

  (function find-matching-command
    "Attempt to find a command that matches the message.

Returns two values-- the matching command-invoker if it
could be found, and a list of alternatives, where each
alternative is a cons cell of its distance to the message
and the command-invoker.

See COMPARE-ALTERNATIVES
See LEVENSHTEIN-DISTANCE
See *ALTERNATIVE-DISTANCE-THRESHOLD*")

  (function levenshtein-distance
    "Calculate the Levenshtein distance between two strings.
See https://en.wikipedia.org/wiki/Levenshtein_distance"))

;; parsing.lisp
(docs:define-docs
  (type command-condition
    "Superclass for all conditions related to the command system.

See MAIDEN:MAIDEN-CONDITION")

  (type lexing-error
    "Superclass for all conditions related to the lexing and parsing of commands.

See COMMAND-CONDITION")

  (type expected-key-error
    "Error signalled when a key was expected, but could not be found.

See LEXING-ERROR")

  (type destructuring-error
    "Superclass for all conditions related to the destructuring of argument lists.

See COMMAND-CONDITION")

  (type not-enough-arguments-error
    "Error signalled when the argument list was too short and could not satisfy the lambda-list.

See DESTRUCTURING-ERROR")

  (type too-many-arguments-error
    "Error signalled when the argument list was too long and the lambda-list was over-satisfied.

See DESTRUCTURING-ERROR")

  (function peek
    "Peek ahead a character on the IN stream.

See CL:PEEK-CHAR")

  (function consume
    "Read a character on the IN stream.

See CL:READ-CHAR")

  (function clear-whitespace
    "Remove all spaces from the IN stream.")

  (function read-delimited-token
    "Read a token which is delimited by DELIM from IN.

This is to say that it consumes until an unescaped version of
DELIM is reached. Every character that is read can optionally
be transformed by the KEY function before it is put into the
resulting string. If UNREAD is non-NIL, the delimiter is unread
before the function exits.")

  (function read-token
    "Read a single token.

A token is anything that is delimited by an unescaped space.

See READ-DELIMITED-TOKEN")

  (function read-string
    "Read a string.

This only returns something if the current character at the
stream position is a double-quote \" character. It then
consumes until and including the closing double-quote. Strings
may escape characters with a backslash.

See READ-DELIMITED-TOKEN")

  (function read-value
    "Read the value part of a key-value pair.

This consumes leading whitespace and will consume either a
string or a token.

See CLEAR-WHITESPACE
See READ-STRING
See READ-TOKEN")

  (function normalize-opt-arg
    "Normalizes the optional argument spec into a list of three values.

It also checks the spec for validity, so an error is
signalled if the first or third items are not symbols.")

  (function stream-end-p
    "Returns true if the stream has reached its end.")

  (function read-rest-arg
    "Read a list of tokens to form the &rest argument.

See READ-VALUE")

  (function read-string-arg
    "Read the rest of the stream into a string to form the &string argument.")

  (function getf*
    "Like GETF, but allows for a test to be specified.

See CL:GETF")

  (function generate-lambda-list-body
    "Generate a list of forms and variable bindings necessary to establish the lambda-list bindings from the input stream.

This reads and processes the lambda-list in parallel. As such
it is harder to impossible to recover from errors and you
probably will just have to give up on a parser failure.

Returns two values-- the forms to put into the let body, and
the variables to create bindings for.

The supported lambda-lists are almost like ordinary lambda-
lists, with the exception that there is a new &string keyword
that gathers the rest of the input into a single string
verbatim, without any processing. However, it is not possible
to combine &string with &rest or &key.

See READ-VALUE
See READ-REST-ARG
See READ-STRING-ARG
See WITH-COMMAND-DESTRUCTURING-BIND")

  (function with-command-destructuring-bind
    "Destructure the INPUT string according to the lambda-list.

The lambda-list keywords &optional &string &rest &key &allow-other-keys
are supported.

If the arguments cannot be parsed or if they do not match
the lambda-list, an error is signalled.

See GENERATE-LAMBDA-LIST-BODY"))
