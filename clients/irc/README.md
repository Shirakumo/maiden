## About
This is a client that enables using Maiden as a client for IRC networks. It does its best to provide all possible IRC commands and replies, but may still be missing a few exotic ones. With a bit of work, it could also be used as the basis for an IRC server.

## How To
In order to use this client, simply create an instance of `irc-client`, add it to a core, and start it. You can use `add-to-core` to do so quickly. Upon construction, you will probably be interested in the following initargs:

* `:nickname` The nickname to use on the server. The client's name defaults to the hostname. If you need to retrieve it from the core easily, or want to maintain multiple connections to the same host, you should also provide `:name`.
* `:username` The IRC username. Defaults to `(machine-instance)`.
* `:realname` The IRC realname. Defaults to `(machine-instance)`.
* `:password` The password for use upon connection. Defaults to NIL. Most networks don't use this feature anyway.
* `:host` The hostname of the server to connect to.
* `:port` The port of the server to connect to. Defaults to 6667.
* `:services-password` The password to use to identify with the NickServ on the servers.

All the IRC replies and commands are present as events, and functions for the latter, in the `org.shirakumo.maiden.clients.irc.events` package, which is nicknamed to `irc`.
