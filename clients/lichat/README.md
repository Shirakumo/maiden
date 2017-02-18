## About
This is a client that enables using Maiden as a client for Lichat networks.

## How To
In order to use this client, simply create an instance of `lichat-client`, add it to a core, and start it. You can use `add-to-core` to do so quickly. Upon construction, you will probably be interested in the following initargs:

* `:username` The name to use on the server. The client's name defaults to the hostname. If you need to retrieve it from the core easily, or want to maintain multiple connections to the same host, you should also provide `:name`.
* `:password` The password for use upon connection. Defaults to NIL. If your user has an account, you will need to set this to be able to connect.
* `:host` The hostname of the server to connect to.
* `:port` The port of the server to connect to. Defaults to 1111.

All the Lichat replies and commands are present as events, and functions for the latter, in the `org.shirakumo.maiden.clients.lichat.rpl` and `org.shirakumo.maiden.clients.lichat.cmd` packages, which are nicknamed to `lichat-rpl` and `lichat-cmd` respectively.
