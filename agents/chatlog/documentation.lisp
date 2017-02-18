#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.chatlog)

;; chatlog.lisp
(docs:define-docs
  (type chatlog
    "This agent handles the recording of messages and activity in a channel to a database, potentially for public viewing on the web.")

  (function back-queue
    "Accessor to the chatlog's back-queue of messages that it should send out.

See CHATLOG
See MAYBE-RECORD-MESSAGE")
  
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

;; database.lisp
(docs:define-docs
  (function connection
    "Accessor to the connection information.

Returns a list of six values as expected by the
postmodern interface. When set, it expects a
plist where the values to set are given by their
respective keyword-value pairs.")

  (function with-db
    "Call the body with an active database connection.

If a connection is already active, nothing is
done aside from evaluating the body.

See POSTMODERN:*DATABASE*
See POSTMODERN:WITH-CONNECTION
See CONNECTION")

  (function prepared-statement
    "Execute a prepared statement.

See CL-POSTGRES:PREPARE-QUERY
See CL-POSTGRES:EXEC-PREPARED")

  (function initialize-database
    "Set up the necessary tables on the database.

The tables created will be called \"channels\"
and \"chatlog\", along with an index called
\"chatlog_channel-id_index\". The tables are
not created if they already exist.

See WITH-DB")

  (function channel-designator
    "Attempt to coerce the given object into a proper designator for a channel.

Should return a cons of server name and channel name.")

  (function user-designator
    "Attempt to coerce the given object into a proper user name.

Should return a string of the user name.")

  (function channel-exists-p
    "Returns true if the given channel is being logged.

See CHANNEL-DESIGNATOR
See WITH-DB")

  (function add-channel
    "Adds the channel for logging.

If the channel is already being logged, an
error is signalled.

See CHANNEL-DESIGNATOR
See WITH-DB")

  (function del-channel
    "Removes the channel from logging.

The actual data that was logged on the channel
is not removed, however.

If the channel is not being logged, an error
is signalled.

See CHANNEL-DESIGNATOR
See WITH-DB")

  (function type->char
    "Translate the type of event into a single character to use.

Possible types are:
- :MESSAGE           => m
- :ACTION :SELF      => a
- :NICK :NAME        => n
- :QUIT :DISCONNECT  => q
- :LEAVE :PART       => p
- :ENTER :JOIN       => j
- :KICK              => k
- :MODE              => o
- :TOPIC             => t")

  (function record-message
    "Record a message of a given type in the database.

TYPE must be an acceptable type as by TYPE->CHAR

See TYPE->CHAR
See CHANNEL-DESIGNATOR
See USER-DESIGNATOR")

  (function process-back-queue
    "Process the queue on the consumer and record all messages, if possible.

See MAYBE-RECORD-MESSAGE")

  (function maybe-record-message
    "Records the message, if the connection to the database can be established.

Push the message to record onto the back-queue
of the client, and  then try to process the
client's back-queue if possible."))
