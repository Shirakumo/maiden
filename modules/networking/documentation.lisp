(in-package #:org.shirakumo.maiden.modules.networking)

;; clients.lisp
(docs:define-docs
  (function client-connected-p
    "Returns true if the client is currently connected to its remote.

See REMOTE-CLIENT")

  (function close-connection
    "Closes the connection to the remote if it's still open.

May signal a warning of type CLIENT-CONNECTION-CLOSED-UNCLEANLY-
WARNING if the connection could not be closed cleanly.

See REMOTE-CLIENT
See CLIENT-CONNECTION-CLOSED-UNCLEANLY-WARNING")

  (function initiate-connection
    "Opens the connection to the remote.

If the connection is already open, an error of type CLIENT-STILL-
CONNECTED-ERROR is signalled. If the connection has failed, a
condition of type CLIENT-CONNECTION-FAILED-ERROR is signalled.

After connection initiation, the client will most likely handle
the connection in the background and may signal a variety of
conditions while that happens.

See REMOTE-CLIENT
See CLIENT-STILL-CONNECTED-ERROR
See CLIENT-CONNECTION-FAILED-ERROR")

  (function handle-connection
    "This is called to handle the connection after it has been initiated.

This method is likely called in a background thread, and as such
must not exit unless the connection has been closed. The primary
method on this function must read content from the connection to
the remote server by RECEIVE and send it off for processing by
the client through PROCESS.

If an error occurs while the connection is live, the error must be
passed on to HANDLE-CONNECTION-ERROR.

If no update is received from the server in a long enough time,
the method must call HANDLE-CONNECTION-IDLE. The minimal or
maximal amount of time that may pass between two calls to HANDLE-
CONNECTION-IDLE is client-specific, however.

HANDLE-CONNECTION provides two restarts around the primary method:
CONTINUE, which will retry the primary method, and ABORT, which
will exit the connection handling.

Upon exiting HANDLE-CONNECTION, the connection is closed by CLOSE-
CONNECTION.

See REMOTE-CLIENT
See RECEIVE
See PROCESS
See HANDLE-CONNECTION-ERROR
See CLOSE-CONNECTION")

  (function handle-connection-error
    "This is called to handle errors during connection handling.

The primary method should decide what to do based on the condition
it received. Depending on this it may either close the connection
and exit, try to stabilise and mitigate, reconnect, or do whatever
seems suitable for the client.

See REMOTE-CLIENT
See HANDLE-CONNECTION")

  (function handle-connection-idle
    "This is called when the connection has not seen or provided new content within a long enough period of time.

The primary method should decide what to do in order to check
whether the connection is still alive, and what to do to stabilise
it, if anything.

See REMOTE-CLIENT
See HANDLE-CONNECTION")

  (function process
    "This is called to handle an update from the remote.

The update may be an object of any type, depending on what the
client deals with. The client will likely want to convert the
update object into an event of some kind and issue that event
onto its cores.

If the data can be parsed, but the client doesn't understand it,
a condition of type UNKNOWN-DATA-WARNING is signalled.

See HANDLE-CONNECTION
See UNKNOWN-DATA-WARNING")

  (function send
    "This is called to send an update to the remote.

The update may be an object of any type, depending on what the
client deals with. The client will likely want to handle certain
types of events and convert them into objects to send off with,
using this function.

A client may only support updates up to a particular length. If
the length is exceeded, a condition of type DATA-TOO-LONG-WARNING
is signalled.

This function may be called asynchronously.

See REMOTE-CLIENT
See DATA-TOO-LONG-WARNING")

  (function receive
    "This function blocks until it has read a complete update from the remote.

Whatever an \"update\" may be is client dependant. However, an
update must be an object that can be passed to PROCESS to be
handled by the client.

This function usually does not need to be called outside of
HANDLE-CONNECTION.

If the data cannot be parsed, a condition of type DATA-PARSE-ERROR
is signalled. If the data can be parsed, but the client doesn't
understand it, a condition of type UNKNOWN-DATA-WARNING is signalled.

This function may be called asynchronously.

See REMOTE-CLIENT
See HANDLE-CONNECTION
See UNKNOWN-DATA-WARNING")

  (function accept
    "This function is called to establish the connection to a new client.

It should create the appropriate client instance and add it to the
server's list of clients. It should then initiate the actual
connection establishment and connection handling protocols.

See TCP-SERVER
See MAKE-TCP-SERVER-CLIENT")

  (function make-tcp-server-client
    "This function is called to construct a new client instance for the TCP server.

See TCP-SERVER-CLIENT")

  (type remote-client
    "Superclass for all clients that are somehow connected to one or more remotes.

A remote-client may be a server, or a client in the networking
sense of the words.

The client's connection can be established by INITIATE-CONNECTION,
which is automatically called for you when the client is STARTed.
The client's connection can be shut down by CLOSE-CONNECTION,
which is automatically called for you when the client is STOPped.

Usually, once the connection has been initiated, a new thread is
spawned that calls HANDLE-CONNECTION to process the updates
coming from the remote. HANDLE-CONNECTION will then call RECEIVE
to receive an update, and then call PROCESS to perform the
necessary actions to handle the update in a useful manner. HANDLE-
CONNECTION must also take care of detecting errors in the
connection's stability. If the client must send something to the
remote, it must do so via the SEND function.

See INITIATE-CONNECTION
See CLOSE-CONNECTION
See HANDLE-CONNECTION
See RECEIVE
See PROCESS
See SEND")

  (type ip-client
    "Superclass for all clients that are connected over the IP protocol.

See HOST
See PORT
See REMOTE-CLIENT")

  (function host
    "Accessor to the host that the client is connected to.

See IP-CLIENT")

  (function port
    "Accessor to the port that the client is connected to.

See IP-CLIENT")

  (type socket-client
    "Superclass for all clients that are connected over a socket.

Updates over a socket-client are processed in the background by a
separate READ-THREAD. The READ-THREAD will simply call HANDLE-
CONNECTION. Sending to/from should happen over the SOCKET.

The thread is attempted to be shut down cleanly when the
connection is closed.

See SOCKET
See READ-THREAD
See IP-CLIENT")

  (function socket
    "Accessor to the socket object that handles the internals of the connection.

See SOCKET-CLIENT
See USOCKET:SOCKET")

  (function read-thread
    "Accessor to the background reading thread used to process the connection.

See SOCKET-CLIENT
See BT:THREAD")

  (type reconnecting-client
    "Mixin for a client that attempts to reconnect automatically after a failure.

The reconnection will be attempted a couple of times, should it
fail the first time around. The current number of failures is
stored in FAILURES. The maximum number of possible failures in
MAX-FAILURES. The amount of time that is waited between each
successive reconnection attempt is handled by BACKOFF and INTERVAL
where BACKOFF is one of :CONSTANT :LINEAR :EXPONENTIAL, and the
INTERVAL is the step for each respective function combined by
the number of failed attempts so far. The client never waits more
than the number of seconds in MAX-RECONNECT-DELAY between
reconnection attempts however. If the number of allowed failed
attempts is exceeded, a condition of type CLIENT-RECONNECTION-
EXCEEDED-ERROR is signalled. If MAX-FAILURES is NIL, this never
happens.

This implements a primary method for HANDLE-CONNECTION-ERROR.

See FAILURES
See MAX-FAILURES
See BACKOFF
See INTERVAL
See MAX-RECONNECT-DELAY
See CLIENT-RECONNECTION-EXCEEDED-ERROR
See HANDLE-CONNECTION-ERROR
See SOCKET-CLIENT")

  (function failures
    "The current number of failures occurred during reconnection attempts.

See RECONNECTING-CLIENT")

  (function max-failures
    "The maximum number of failed connection attempts that may occur before it gives up.

If this is NIL, reconnection attempts will not stop automatically.

See RECONNECTING-CLIENT")

  (function backoff
    "The type of backoff used to determine the amount of time between each reconnection attempt.

Must be one of the following.
:CONSTANT     The sleep time is INTERVAL
:LINEAR       The sleep time is INTERVAL*FAILURES
:EXPONENTIAL  The sleep time is INTERVAL^FAILURES

See INTERVAL
See RECONNECTING-CLIENT")

  (function interval
    "The interval used for the calculation of the backoff time between reconnection attempts.

See BACKOFF
See RECONNECTING-CLIENT")

  (function max-reconnect-delay
    "The number of seconds that are waited at most between reconnection attempts.

See RECONNECTING-CLIENT")

  (type timeout-client
    "Mixin for a client that generates a timeout error if there isn't enough feedback from the remote.

A timeout is detected by keeping track when the last update was
received from the remote. If the time distance is too great, then
the connection is closed and re-initiated.

See TIMEOUT
See LAST-RECEIVED-TIME
See REMOTE-CLIENT")

  (function timeout
    "Accessor to the number of seconds after which the connection is deemed to have been severed.

See TIMEOUT-CLIENT")

  (function last-received-time
    "Accessor to the universal-time representing the last time an update was received from the remote.

See TIMEOUT-CLIENT")

  (type text-client
    "Mixin for a client whose connection deals with cleartext updates.

The updates are read into a buffer. If you need more control over
how the updates are read, you should implement your own version of
RECEIVE, or simply not use this mixin.

See ENCODING
See BUFFER
See SOCKET-CLIENT")

  (function encoding
    "The character encoding used by the connection.

See TEXT-CLIENT")

  (function buffer
    "The buffer used to read from the connection.

Can be either a vector, or :line if READ-LINE should be used
instead.

See TEXT-CLIENT")

  (type tcp-client
    "Superclass for all clients communicating over a TCP socket.

The default stream element-type is (UNSIGNED-BYTE 8).

See USOCKET:SOCKET-CONNECT
See IDLE-INTERVAL
see ELEMENT-TYPE
See SOCKET-CLIENT")

  (function element-type
    "Reader for the element-type the TCP connection's stream should use.

See TCP-CLIENT")

  (function idle-interval
    "The number of seconds to wait for an update from the remote before calling HANDLE-CONNECTION-IDLE.

See TCP-CLIENT")

  (type text-tcp-client
    "Superclass for all clients that have a text-based connection over TCP.

See TCP-CLIENT
See TEXT-CLIENT")

  (type tcp-server
    "Superclass for all clients serving as a TCP server.

The HANDLE-CONNECTION method simply waits for new connections and
then calls ACCEPT with the new socket instance. The list of
connected clients is kept in CLIENTS.

See CLIENTS
See ACCEPT
See MAKE-TCP-SERVER-CLIENT
See SOCKET-CLIENT")

  (function clients
    "Accessor to the TCP server's list of client instances.

See TCP-SERVER
See TCP-SERVER-CLIENT")

  (type tcp-server-client
    "Superclass for all TCP server client instances.

Each client represents a connection to a remote client this serves
to. In case you subclass this, you must also subclass TCP-SERVER
and specialise on MAKE-TCP-SERVER-CLIENT to construct an instance
of your subclass.

See TCP-SERVER"))

;; conditions.lisp
(docs:define-docs
  (type client-connection-failed-error
    "Condition signalled when the connection initiation failed.

See INITIATE-CONNECTION
See CLIENT-CONDITION
See SOCKET-CLIENT")

  (type client-still-connected-error
    "Condition signalled when another connection initiation is attempted while it is still connected.

See INITIATE-CONNECTION
See CLIENT-CONDITION
See SOCKET-CLIENT")

  (type client-reconnection-exceeded-error
    "Condition signalled when the maximum number of reconnection attempts has been exceeded.

See HANDLE-CONNECTION-ERROR
See RECONNECTING-CLIENT
See CLIENT-CONDITION")

  (type client-connection-closed-uncleanly-warning
    "Condition signalled when the connection could not be closed cleanly.

See CLOSE-CONNECTION
See CLIENT-CONDITION
See SOCKET-CLIENT")

  (function closing-error
    "Reader for the error that occurred during the connection closing attempt.")

  (type client-timeout-error
    "Condition signalled when the connection has timed out.

See HANDLE-CONNECTION
See TIMEOUT-CLIENT")

  (type data-condition
    "Superclass for all conditions relating to errors during interpretation of an update from the remote.

See REMOTE")

  (function data
    "Reader for the data that could not be interpreted.

See DATA-CONDITION")

  (type data-parse-error
    "Condition signalled when the data received from the remote could not be parsed.

See DATA-CONDITION
See CLIENT-CONDITION")

  (type unknown-data-warning
    "Condition signalled when the data received could be parsed, but is something unknown that the client doesn't know what to do with.

See DATA-CONDITION
See CLIENT-CONDITION")

  (type data-too-long-warning
    "Condition signalled when the data to be sent is too long and was truncated or dropped as a result.

See DATA-CONDITION
See CLIENT-CONDITION"))

;; events.lisp
(docs:define-docs
  (type connection-event
    "Superclass for all events relating to connection updates.

See CLIENT-EVENT")

  (type connection-initiated
    "Event issued when a connection has been initiated successfully.

See CONNECTION-EVENT")

  (type connection-closed
    "Event issued when a connection has been closed fully.

See CONNECTION-EVENT")

  (type outgoing-event
    "Superclass for all events that represent requests for data to be sent to the remote.")

  (type incoming-event
    "Superclass for all events that represent data that has been received from the remote."))
