#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.clients.irc)

;; client.lisp
(docs:define-docs
  (variable *send-length-limit*
    "The maximum number of bytes that can be sent in a message to the server.")

  (type irc-client
    "This client offers a connection to an IRC server.

The client will attempt to keep a properly consistent
view of the server. It will track all users and channels
and their relationships to the best of its ability.
It will also attempt to automatically reconnect and
correct potentially incorrect nicknames and all of these
kinds of problems that plague IRC networks.

See NICKNAME
See USERNAME
See PASSWORD
See REALNAME
See INTENDED-NICKNAME
See SERVICES
See SERVICES-PASSWORD
See MAIDEN-NETWORKING:TEXT-TCP-CLIENT
See MAIDEN-NETWORKING:RECONNECTING-CLIENT
See MAIDEN-NETWORKING:TIMEOUT-CLIENT
See MAIDEN-NETWORKING:SIMPLE-USER-CHANNEL-CLIENT")

  (function nickname
    "The current nickname of the client.

Unlike a user, where the NAME is the nickname of the
user, in order to keep the name of a client instance
consistent and unchanging, the client's nickname is
kept separate.

This is another accessor to the same field as MAIDEN-
CLIENT-ENTITIES:USERNAME

See IRC-CLIENT
See MAIDEN-CLIENT-ENTITIES:USERNAME")

  (function username
    "The username that is passed to the server upon connection.

Note that this is /not/ the same as MAIDEN-CLIENT-ENTITIES:
USERNAME, which instead corresponds to NICKNAME.

See IRC-CLIENT")

  (function password
    "The password that is used during connection to the server.

See SERVICES-PASSWORD
See IRC-CLIENT")

  (function realname
    "The realname that is passed to the server upon connection.

See IRC-CLIENT")

  (function intended-nickname
    "Holds the intended nickname, which may be different from the actual nickname.

This is used to try and automatically change NICK to
the intended nickname when possible.

See IRC-CLIENT")

  (function services
    "Holds the type of services the server uses.

The client library must have specific support for each
services network. Currently the following types of
services are explicitly supported:

- :FREENODE
- :ANOPE
- :RIZON

This is in part used to be able to authenticate the
nicknames of users and check if they aren't impostors.

See IRC-CLIENT")

  (function services-password
    "Holds the password used to identify with NickServ.

See IRC-CLIENT")

  (function authenticate-with
    "Attempt to authenticate the user over a specific method.

Method can be one of the following:

- :ACC :FREENODE
  Asks the NickServ for an ACC reply, which is then
  parsed to see if the nickname is authenticated.

- :STATUS :ANOPE :TYNET :RIZON
  Asks the NickServ for a STATUS reply, which is then
  parsed to see if the nickname is authenticated.

- :R-MODE
  Tests for the r flag in the user's mode string.

- :GENERIC
  Attempts to first use :ACC, then :STATUS, finally :R-MODE

- NIL
  Always returns T.

See IRC-CLIENT
See SERVICES"))

;; commands.lisp
(docs:define-docs
  (variable *message-length-limit*
    "Maximum number of characters to be allowed in a single message event.

The default is set to 360.
This leaves 152 characters to encode the preamble and
CRLF of a message. That should be enough while still
sending a fairly long message over the net. Calculating
the limit precisely would involve querying the hostname
as seen by others as well as the precise manner in
which things are encoded on the wire. Since that is
somewhat too complicated and 360 is still plenty long
we settle for this compromise. The protocols don't
actually say anything about the length restrictions of
the username, so we cannot make a worst-case estimate
limit. However, we do know that the hostname is limited
to 63 characters, we need 2 for the line ending, ~10
for the command, and ~36 for the nickname. The nick
length here being overly long especially considering
the initial RFCs limit it to 9, which is way too short
to be relied upon. Either way, 152 should be a good
limit.")

  (type send-event
    "Superclass for all IRC events that are outgoing.

See UPDATE-MESSAGE
See IRC-EVENT
See MAIDEN-NETWORKING:OUTGOING-EVENT
See MAIDEN:ACTIVE-EVENT
See UPDATE-MESSAGE")

  (function update-message
    "The list of direct update messages to send to the server to execute the event.

See SEND-EVENT")

  (function define-irc-command
    "Define an IRC server command.

This will do the following:
- It defines an event of the same name whose MESSAGE
  method contains the BODY and whose slots are parsed
  from the arglist.
- It defines a function of the same name that will
  issue said event onto a core.

The even is always a subclass of INSTRUCTION-EVENT and
SEND-EVENT. The name is re-interned in the ORG.
SHIRAKUMO.MAIDEN.CLIENTS.IRC.EVENTS package to avoid
clashing.

You can specify additional superclasses with the
:SUPERCLASSES body option.

See MESSAGE
See MAIDEN:INSTRUCTION-EVENT
See SEND-EVENT")

  (function define-simple-irc-command
    "Defines a simple IRC command.

The body should be a format string followed by the
format options.

See DEFINE-IRC-COMMAND
See CL:FORMAT")

  (type send-message-event
    "Superclass for all send events that carry a message.

See SEND-EVENT
See MAIDEN-CLIENT-ENTITIES:MESSAGE-EVENT")

  (function splittable-char-p
    "Returns true if the character is considered to be one suitable for splitting a message.

More specifically, it returns T if the character
is not a graphic char, or is one of the following:
 .,;:?!　。、：；？！

See CL:GRAPHIC-CHAR-P")

  (function reasonable-message-end
    "Attempt to determine a reasonable end position for the given message.

It will not return a message that is shorter by
more than MAX-BACKTRACK characters.

See SPLITTABLE-CHAR-P")

  (function split-message-smartly
    "If necessary, split the message into multiple strings that each are of maximum LENGTH.

See REASONABLE-MESSAGE-END")

  (function split-message-considering-newlines
    "Smartly split the message, first by splitting newlines.

See SPLIT-MESSAGE-SMARTLY")

  (function define-message-irc-command
    "Define an IRC command that carries a primary message text.

This takes care of splitting the message at
appropriate locations should the message be too
long.

See DEFINE-IRC-COMMAND"))

;; TODO: Document all commands

;; conditions.lisp
(docs:define-docs
  (type message-too-long-warning
    "Condition signalled when a message has been truncated because it is too long.

See MAIDEN-CLIENT-ENTITIES:MESSAGE-CONDITION
See MAIDEN:CLIENT-CONDITION"))

;; events.lisp
(docs:define-docs
  (variable *reply-events*
    "Map from update names and codes to their event class names.")

  (type irc-event
    "Superclass for all IRC events.

See MAIDEN:CLIENT-EVENT")

  (type irc-channel-event
    "Superclass for all events related to a channel.

See IRC-EVENT
See MAIDEN-CLIENT-ENTITIES:CHANNEL-EVENT")

  (type reply-event
    "Superclass for all events that are a reply from the server.

See IRC-EVENT
See MAIDEN-CLIENT-ENTITIES:USER-EVENT
See MAIDEN-NETWORKING:INCOMING-EVENT
See MAIDEN:PASSIVE-EVENT")

  (type unknown-event
    "Base container event for all replies that are unknown and not handled by a specific class.

See REPLY-EVENT")

  (function parse-reply
    "Parse a raw reply line from the server into a list of reply events.

If the line is malformatted, a condition of type
DATA-PARSE-ERROR is signalled. If a reply has a type
or code that is not specifically handled, a condition
of type UNKNOWN-DATA-WARNING is signalled.

The line is parsed by a regex, and further
destructured as necessary by the actual event.

See *REPLY-EVENTS*
See COERCE-IRC-OBJECT
See MAKE-REPLY-EVENTS
See MAIDEN-NETWORKING:DATA-PARSE-ERROR
See MAIDEN-NETWORKING:UNKNOWN-DATA-WARNING")

  (function make-reply-events
    "Create the appropriate event instances.

See DEFINE-IRC-REPLY")

  (function permute
    "Create a list of all possible permutations of the elements in the list.")

  (function define-irc-reply
    "Define a new IRC reply command and how it is parsed.

NAME  -- The name of the event class. Will be interned
         into ORG.SHIRAKUMO.MAIDEN.CLIENTS.IRC.EVENTS.
CODE  -- The string or code that identifies the event
         on the IRC reply line.
REGEX -- A regular expression to destructure the reply
         line body into usable slots.
SLOTS -- The slots of the event. Must correspond to
         REGEX capture groups.

This defines an event class according to the slots
and the defined superclasses. It always injects the
REPLY-EVENT superclass. It also defines a MAKE-REPLY-
EVENTS method which will create the event instances as
necessary. It will even take care of parsing multi-
target fields and splitting them up into multiple events.

See MAKE-REPLY-EVENTS
See CL-PPCRE:REGISTER-GROUPS-BIND"))

;; TODO: Document all events... if it's not me.

;; users.lisp
(docs:define-docs
  (type irc-server
    "Class to represent an IRC server.

See MAIDEN-CLIENT-ENTITIES:CLIENT-ENTITY")

  (type irc-user
    "Class to represent an IRC user.

See USER
See HOST
See MAIDEN-CLIENT-ENTITIES:SIMPLE-USER")

  (type irc-channel
    "Class to represent an IRC channel.

See TOPIC
See MAIDEN-CLIENT-ENTITIES:SIMPLE-CHANNEL")

  (function topic
    "Accessor to the channel's topic line.

See IRC-CHANNEL")

  (function prune-users
    "Clear out all users from the client's user-map that are no longer in any channels.

See IRC-CLIENT")

  (function coerce-irc-object
    "Attempt to parse the name to figure out what kind of object it represents.

This proceeds as follows, which is a heuristic, but
should work well enough in most cases.

1. The characters ~=+*@ are stripped from the name
2. If the name contains a . it is parsed as a server
3. If the name contains a # it is parsed as a channel
4. Otherwise it is parsed as a user.

This does not always work as the IRC spec is very
lenient on what a user, channel, or server may call
themselves, so in sufficiently bad situations it can
be impossible to tell and this will fail, with
potentially horrendous consequences.

See IRC-SERVER
See IRC-CHANNEL
See IRC-USER"))
