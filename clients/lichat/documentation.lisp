#|
 This file is a part of Maiden
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.clients.lichat)

;; client.lisp
(docs:define-docs
  (type lichat-user
    "Class to represent a user on the Lichat server.

See MAIDEN-CLIENT-ENTITIES:SIMPLE-USER")

  (function find-conversation-channel
    "Attempt to find a channel for the user that is suitable for private conversations.

Such a channel must be:
1. Anonymous
2. Only contain this user, and the client itself.")

  (function make-anonymous-channel
    "Create a new anonymous channel and optionally invite the given users to it.

See LICHAT-CMD:CREATE
See LICHAT-CMD:PULL")

  (type lichat-channel
    "Class to represent a channel on the Lichat server.

See MAIDEN-CLIENT-ENTITIES:SIMPLE-CHANNEL")

  (type lichat-client
    "This client offers a connection to a Lichat server.

The client will attempt to keep a properly consistent
view of the server. It will track all users and channels
and their relationships to the best of its ability.
It will also attempt to automatically reconnect.

See MAIDEN-NETWORKING:TEXT-TCP-CLIENT
See MAIDEN-NETWORKING:RECONNECTING-CLIENT
See MAIDEN-NETWORKING:TIMEOUT-CLIENT
See MAIDEN-CLIENT-ENTITIES:SIMPLE-USER-CHANNEL-CLIENT
See SERVERNAME
See USERNAME
See PASSWORD")

  (function servername
    "Accessor to the name of the server that this client is connected to.

See LICHAT-CLIENT")

  (function username
    "Accessor to the name the client has on the server.

See LICHAT-CLIENT")

  (function password
    "Accessor to the password used to connect to the server with.

See LICHAT-CLIENT"))

;; events.lisp
(docs:define-docs
  (function parse-event
    "Parse an event from the given stream.

If the update has an unknown type, a condition of type
LICHAT-PROTOCOL:UNKNOWN-WIRE-OBJECT is signalled. If the
update is malformed, a condition of type LICHAT-PROTOCOL:
MALFORMED-WIRE-OBJECT is signalled.

See LICHAT-PROTOCOL:READ-SEXPR
See LICHAT-PROTOCOL:MALFORMED-WIRE-OBJECT
See LICHAT-PROTOCOL:UNKNOWN-WIRE-OBJECT
See LICHAT-PROTOCOL:CHECK-UPDATE-OPTIONS")

  (function good-initarg-p
    "Returns true if the initarg is considered \"good\", or in other words is known by the system as an initarg that can be exported.

See PRINT-EVENT")

  (function print-event
    "Write the event to the stream in the proper Lichat wire format.

This is a specialised method since the standard version
provided by the Lichat protocol package cannot be used.
The standard version would use the wrong initargs, as some
slots on the events in this client have multiple initargs
for slot sharing and compatibility reasons.

If the object cannot be printed to the the stream properly,
a condition of type LICHAT-PROTOCOL:UNPRINTABLE-OBJECT is
signalled.

See GOOD-INITARG-P")

  (function define-update
    "Define an update type.

This defines a couple of things:
- An event class of the given name, with the args parsed
  as slots.
- An event class in the LICHAT-RPL package with the above
  and LICHAT-RPL:UPDATE as superclasses.
- An event class in the LICHAT-CMD package with the one
  before and LICHAT-CMD:UPDATE as superclasses.
- A function of the same name in the LICHAT-CMD package
  that issues the event of the same name onto a core.")

  (type update
    "Superclass for all updates, both commands and replies.

You should not subclass this directly. Instead use the
versions from the LICHAT-CMD and LICHAT-RPL packages.

See ID
See CLOCK
See LICHAT-CLIENT-ENTITIES:USER-EVENT
See LICHAT-PROTOCOL:WIRE-OBJECT")

  (function id
    "Reader for the ID of the update.

See LICHAT-PROTOCOL:ID
See UPDATE")

  (function clock
    "Reader for the CLOCK of the update.

See LICHAT-PROTOCOL:CLOCK
See UPDATE"))

;; FIXME: document all the replies and commands
