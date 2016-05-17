## About Maiden-Networking
This module adds support for common networking problems to make implementing a client that has to rely on a remote connection easier.

## Classes
We start with `remote-client`, which adds `initiate-connection`, `close-connection`, and `client-connected-p` that should be implemented to perform the obvious corresponding actions on the client.

Moving on to `ip-client`, which adds a `host` and `port` slot.

Next up there is `socket-client`, which adds `socket` and `read-thread` slots and `handle-connection`, `handle-connection-error`, `process`, `receive`, and `send`. It implements `client-connected-p`, and `close-connection`. It automatically launches a thread on `initiate-connection`, which calls `handle-connection`. This function should loop and call `receive` to get new information for as long as appropriate and pass it on to `process`. `continue` and `abort` restarts are automatically established around it. It also makes sure that `receive` and `send` are locked to avoid threading collisions while sending or receiving. `handle-connection-error` is invoked if an error within `handle-connection` is detected. Finally, if `handle-connection` exits, `close-connection` is called which takes care of properly cleaning up the thread and calling `usocket:socket-close`.

After that big stinker, we get `reconnecting-client`, which adds the `failures`, `max-failures`, `backoff`, and `interval` slots. It also implements `handle-connection-error` which should smartly handle problems in the connection by automatically reestablishing it. It does this `max-failures` times before giving up with a `client-reconnection-exceeded-error`. Each time `interval` amount of time is waited, which is increased by `backoff`. `backoff` can be one of `:constant`, `:linear`, or `:exponential`.

Next we have a smaller class called `timeout-client`, adding a `timeout` and `last-received-time` slot. Upon `receive` it updates the `last-received-time` with the current time, and when `handle-connection-idle` is invoked, it will test if the timeout is exceeded. If so, a `client-timeout-error` is signalled.

Moving on there is the `text-client`, adding an `encoding`, and `buffer` field. Since it is a subclass of `socket-client` it can implement the `receive` and `send` functions to handle strings. `buffer` can be either `:line` in which case `read-line` is used, or a string of a fixed length, in which case `read-sequence` is used.

Now we get to the `tcp-client` with which come the `element-type` and `idle-interval` slots. It implements `client-connected-p`, `initiate-connection`, `handle-connection`, and `handle-connection-idle`. The `handle-connection` method will repeatedly wait for input with the `idle-interval` timeout, after which `handle-connection-idle` is called until there actually is some input available. If input is available, `process` is called with the result of `receive`. 

The `text-tcp-client` is merely a combination of `text-client` and `tcp-client`.

Next up we have the `tcp-server`, which adds a `clients` slot that keeps a list of known client connections of this server. It also adds the `accept` and `make-tcp-server-client` methods, which have standard implementations. By default `accept` just calls `initiate-connection` with the result of `make-tcp-server-client`, which simply constructs an instance of `tcp-server-client`.

The `tcp-server-client` is a `tcp-client` with a `server` slot, to which it pushes itself upon initialisation. Upon connection closing, it removes itself from the server's clients list again. If you actually do implement a server, you will definitely want to subclass this to implement actually receiving behaviour and override `make-tcp-server-client` to construct an instance of your subclass instead.
