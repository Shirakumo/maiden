## About Colleen-Relay
The Colleen relay allows transparent event relaying across a network of cores. Optimally no other module should have to be aware that the relay is in effect. However, there must always be some source of authority that first establishes the connections and issues the proper subscriptions so that events get transmitted at all.

## How To
In order to set your core up for accepting relay connections, simply instantiate a `relay` consumer and add it to the core. If you simply want to be able to send and receive events without allowing anyone to actually connect to you, set either `:host` or `:port` to `NIL`. Using `connect` you can then connect to a remote relay. With `subscribe` you can subscribe to events that happen on a remote relay and have them be transmitted over to your currently local core. Note that you can construct a network that bridges over several cores, but your network must never contain cycles. If a cycle is introduced, the system will immediately enter a message passing infinite loop until the cycle is broken.

Also note that remote clients get recreated locally through `virtual-client` instances. The purpose of this is such that the `client` field in events can be properly tagged and such that you can `respond` to events without having to be aware that the event came from a different core. Additionally you can access slots on the client transparently, but accessor functions will need an additional method. You can even construct relaying methods such that a call of a method with a virtual client will be executed on the core local to the virtualised client. To do this, see `define-virtual-client-method`.

## Internals
### Connection Establishment
When a relay initiates a connection with a new remote, the `client`s at both ends follow the following procedure:

1. Issue a `connection-initiated` event
2. Send the following message: `(:id relay-id colleen-version)`
3. Send a network update containing the local relay in its `new` slot.

Plain lists are treated specially upon receiving. They indicate relay internal communication and are handled as follows:

* `:id` Check the version against the local one, signal a `client-version-mismatch` warning if there's a mismatch. Then set the received id as the `remote` of the client.
* `:timeout` Signal a `client-timeout-error` error.
* `:close` Call `close-connection` on the client.
* `:ping` Send a `(:pong)` message back.
* `:pong` Do nothing.

Otherwise the following handling steps are taken depending on the type of message:

* `network-update` Call `update` on the relay with the remote and a reconstructed update to propagate the cĥange. Reconstructing the update through `make-network-update` has the effect that the hop count of the update is increased by one.
* `subscription-update` Call `update` on the relay with the remote and the update.
* `client-event` Call `relay` on the relay with the remote and relay.
* `transport` Call `relay` on the relay with the transport `target` and relay.
* Otherwise signal an `unknown-message-warning`.

Connection reestablishment in case of disruptions or timeouts are handled by the respective Colleen base client mixins.

### Network Persistence
The network state gets persisted through `network-update`s. An update can contain a list of `bad` links that should be removed or `new` links that should get recognised. Each entry in the `new` list is of the form `(hops client-id client-name)`, and each entry in the `bad` list is of the form `destination-id`. For each update that gets processed we must know its next stop origin, which we know by looking at which client received it. For bad links we then look at each client it lists. If it's a virtual-client, remove each link in its link table that contains this origin. If there are no more links left, remove the client entirely. For new links if the client does not exist, it is created and added to the cores of the relay. Then each link's corresponding virtual client gets a new entry with the hop count and origin saved.

This way each particular virtual client knows which directly connected relay to relay the message to next to reach itself, but it does not know how the full network path. Since each link is annotated with a hopcount it should also be able to choose the path that requires the least hops. This is not necessarily the fastest, but we assume that the latency addition of a hop outweighs the badness of a potentially slow link.

### Message Relaying
A relay instance has a global handler that acts on all events. Unless the origin of the event is `'relay` It checks if the event is a client event and the client is a virtual client. If that is the case, it `relay`s the event. Otherwise, if the event matches a subscription, it wraps it in a `transport` and `relay`s it.

When a relay client receives a client-event or a transport, it is too sent to `relay` as described in the Connection Establishment section above.

`relay` then does the following depending on what the target is:

* `core` The event is `issue`d onto the core, but making sure to change the `origin` to `'relay` and fixing the `event-loop` slot.
* `consumer` The event is `issue`d onto whatever core can be found in the relay's list that contains the client.
* `virtual-client` The closest link in the `virtual-client` that still exists is looked up and the message is sent to the corresponding client.
* `(eql T)` The message is sent to all clients on the relay.

### Message Serialisation
See `colleen-serialize`.
