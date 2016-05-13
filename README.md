## About Maiden
Maiden is a modular, extensible, dynamic, <insert-buzzword-here> framework for building all sorts of stuff. I'm saying all sorts of stuff because primarily Maiden is only concerned with providing a usable event delivery system. From there on out it can be used for pretty much anything that fits this pattern. Historically however, Maiden was intended as an IRC bot framework, and can still act as such now, but may also very well be used to be integrated with other chat systems, or completely different tasks altogether.

## Concepts
Before you continue here, make sure that you have read the documentation for [Deeds](https://shinmera.github.io/deeds) as it is central to the workings of Maiden.

### Base Components
The primary difference to Deeds' model is that Maiden introduces the concept of a consumer that acts as an intermediary to handlers and event loops. Additionally, an event loop is encapsulated in a core. Consumers hold a list of handlers, which are added to the core's loop when the consumer is added to the core. In effect this means that you can bundle handlers together to encapsulate some form of functionality and add or remove this piece in one go.

Consumers are implemented through a metaclass and a base superclass. The metaclass ensures that you can 'define' handlers on a consumer class and that inheritance of handlers throughout the consumer hierarchy is preserved properly. It also takes care to make redefinition of handlers flush through gracefully even if active consumer instances already exist.

On top of the consumer, two classes are defined: agent and client. Agent should be used as a superclass for any kind of thing that can only exist once per core. This is usually something that represents a module that provides internal mechanisms like updating events or performing some kind of response to an event. A concrete example would be something that looks up the weather on the web and generates a response with the weather data. Clients on the other hand can be instantiated many times over for each core, the typical example for which is some sort of connection to a remote server like IRC.

### Protocols
Maiden provides various standard parts to make implementing easier on you. You may of course choose to ignore all of this and do everything yourself, but if actually do want to use this framework for something, you'll need to understand the protocol in question.

#### Entity
Most things in Maiden are a subclass of `entity`. This provides only a single method, `matches`, which compares two things with each other and performs some kind of smart comparison. While this function does not necessarily always do what you want --equality is hard after all-- it is used to generically perform tests in various contexts.

Underneath the `entity` we have `named-entity` which provides a single slot, `name`, and an additional method, `find-entity`, which should discover a matching entity within some form of container.

#### Core
A `core` is a container for two event loops and a list of consumers. You can search for a consumer with `consumer` and add or remove them with `add/remove-consumer`. Cores, just like event-loops, can be `start`ed and `stop`ped and you can `issue` events onto them, which they will `handle`. The two event loops are distinguished for the following reason:

The primary event loop is optimised for fast delivery, which makes adding and removing handlers from it expensive. For things like one-time-handlers that exist for possibly only a very short period of time and may be added very quickly, this would become a bad bottleneck. For this reason, the core has a secondary event loop that does not optimise delivery, but instead has very low-cost adding and removal.

`de/register-handler` act on the primary event loop, whereas `with-awaiting` and `with-response` act on the secondary event loop. `handler` also only works on the primary event loop, so you cannot search the secondary one unless you do so manually.

#### Consumers
We start with the `consumer-class` which is a `standard-class` for consumers. It contains a list of abstract handler definitions that is computed according to proper class inheritance. In order to be able to update the instances of the class when a handler is redefined, it also keeps a weak pointer list to all its instances. This is intentionally in contradiction to the standard MOP/CLOS method of deferring an update until the respective slot is needed, because deferring the update would cause the event-loop to become unstable and potentially miss handlers on delivery.

When the `consumer-class`' `handlers` accessor is updated, `reinitialize-instance` is called on all its instances. Additionally, if the `direct-handlers` accessor is updated, the change is immediately propagated throughout the class hierarchy, causing classes (and thus instances) further down the line to be readjusted.

However, the `consumer-class` by itself is not enough to complete the protocol. We also need `consumer` itself, which takes care of establishing the link-back from the `consumer-class` to the instance upon initialisation, and takes care of updating its local handler instances upon redefinition, starting, and stopping. `define-consumer` is a wrapper around defclass that takes care of establishing the proper super- and metaclasses.

In order for the handler definition to work smoothly, when `define-handler` is invoked, not an actual handler object is constructed, but instead an `abstract-handler` instance. This instance keeps all initialisation options around and is attached to the class it is being defined on. When a `consumer` is actually instantiated, it then creates actual handler instances by invoking `instantiate-handler` on the abstract instances on its class. These instances are then de/registered whenever the consumer is added/removed from a core.

There is an additional complication to be aware of. If we have an event that is supposed to be directed to a specific instance of a consumer, how can we avoid the handler to be triggered for every instance? Maiden solves this problem by implicitly adding a filter test clause to the handler when it is instantiated from its abstract definition. If the definition contains the `:match-consumer` argument, the specified field of the event is tested against the consumer instance, thus avoiding the handler being triggered if it is not of the required instance.

Additionally, the delivery function is extended through a closure that captures the consumer instance, so that it is possible to refer to it within the handler body.

Finally, Maiden provides some convenience macros to make common operations easier. `define-command` is a shorthand for defining an event, a handler to execute whatever the event should do, and a matching function to broadcast the event while making it appear like a regular function call. `define-query` is like `define-command` except that its handler automatically responds with a response event that contains the multiple-values-list of the body, and the corresponding function waits for the response, thus completing the illusion of a regular function call, including return values.

#### Agents
The `agent` class only does three things: automatically set the name to its class if it is not explicitly given, change `matches` on agents to test against the class, and signal a `agent-already-exists-error` on `add-consumer` of an agent if an agent with the same name already exists on the core.

#### Clients
Since connection handling can get pretty involved, the client protocol involves quite a few classes and methods at the top of which is `client` that does nothing useful by itself, aside from distinguishing the consumer as a client.

Next we have `user-client`, which adds the `authenticate` method that should return `T` if the given user is known to be authenticated on whatever the client is connected to.

Following that there is `remote-client`, which adds `initiate-connection`, `close-connection`, and `client-connected-p` that should be implemented to perform the obvious corresponding actions on the client.

Moving on to `ip-client`, which adds a `host` and `port` slot.

Next up there is `socket-client`, which adds `socket` and `read-thread` slots and `handle-connection`, `handle-connection-error`, `process`, `receive`, and `send`. It implements `client-connected-p`, and `close-connection`. It automatically launches a thread on `initiate-connection`, which calls `handle-connection`. This function should loop and call `receive` to get new information for as long as appropriate and pass it on to `process`. `continue` and `abort` restarts are automatically established around it. It also makes sure that `receive` and `send` are locked to avoid threading collisions while sending or receiving. `handle-connection-error` is invoked if an error within `handle-connection` is detected. Finally, if `handle-connection` exits, `close-connection` is called which takes care of properly cleaning up the thread and calling `usocket:socket-close`.

After that big stinker, we get `reconnecting-client`, which adds the `failures`, `max-failures`, `backoff`, and `interval` slots. It also implements `handle-connection-error` which should smartly handle problems in the connection by automatically reestablishing it. It does this `max-failures` times before giving up with a `client-reconnection-exceeded-error`. Each time `interval` amount of time is waited, which is increased by `backoff`. `backoff` can be one of `:constant`, `:linear`, or `:exponential`.

Next we have a smaller class called `timeout-client`, adding a `timeout` and `last-received-time` slot. Upon `receive` it updates the `last-received-time` with the current time, and when `handle-connection-idle` is invoked, it will test if the timeout is exceeded. If so, a `client-timeout-error` is signalled.

Moving on there is the `text-client`, adding an `encoding`, and `buffer` field. Since it is a subclass of `socket-client` it can implement the `receive` and `send` functions to handle strings. `buffer` can be either `:line` in which case `read-line` is used, or a string of a fixed length, in which case `read-sequence` is used.

Now we get to the `tcp-client` with which come the `element-type` and `idle-interval` slots. It implements `client-connected-p`, `initiate-connection`, `handle-connection`, and `handle-connection-idle`. The `handle-connection` method will repeatedly wait for input with the `idle-interval` timeout, after which `handle-connection-idle` is called until there actually is some input available. If input is available, `process` is called with the result of `receive`. 

The `text-tcp-client` is merely a combination of `text-client` and `tcp-client`.

Next up we have the `tcp-server`, which adds a `clients` slot that keeps a list of known client connections of this server. It also adds the `accept` and `make-tcp-server-client` methods, which have standard implementations. By default `accept` just calls `initiate-connection` with the result of `make-tcp-server-client`, which simply constructs an instance of `tcp-server-client`.

The `tcp-server-client` is a `tcp-client` with a `server` slot, to which it pushes itself upon initialisation. Upon connection closing, it removes itself from the server's clients list again. If you actually do implement a server, you will definitely want to subclass this to implement actually receiving behaviour and override `make-tcp-server-client` to construct an instance of your subclass instead.

#### Additional Protocols
Further protocols may be added by agents or clients. Refer to their documentation for information on how they work and how to interact with them.
