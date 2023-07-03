(in-package #:org.shirakumo.maiden.clients.relay)

;; client.lisp
(docs:define-docs
  (type relay-client
    "This class represents a remote client that was connected to this system via the relay.

See REMOTE
See MAIDEN-NETWORKING:TCP-SERVER-CLIENT
See MAIDEN-NETWORKING:TIMEOUT-CLIENT")

  (function remote
    "Accessor to the identifier of the remote core that is connected over this relay-client.

See RELAY-CLIENT")

  (type relay-client-initiator
    "This class is a relay-client that actively seeks out a connection.

See RELAY-CLIENT"))

;; conditions.lisp
(docs:define-docs
  (type relay-condition
    "Superclass for all conditions related to the relay system.

See MAIDEN:CLIENT-CONDITION")

  (type carrier-condition
    "Superclass for all conditions related to the carrying of the updates across the relay network.

See RELAY-CONDITION
See MESSAGE")

  (type target-condition
    "Superclass for all conditions that relate to a certain target relay.

See RELAY-CONDITION
See TARGET")

  (function target
    "The ID of the target core that the object is related to.

See TARGET-CONDITION
See SUBSCRIPTION-UPDATE
See TRANSPORT")

  (type no-relay-target-specified
    "Condition signalled when the target of an update to be relayed got lost or was not specified properly.

See CARRIER-CONDITION")

  (type relay-route-not-found
    "Condition signalled when no possible route to the specified target can be found.

See CARRIER-CONDITION
See TARGET-CONDITION")

  (type relay-link-not-found
    "Condition signalled when a link between two relays was attempted to be used, but appears to be missing.

See CARRIER-CONDITION
See TARGET-CONDITION")

  (type client-version-mismatch
    "Condition signalled when the versions of the two relay endpoints are not compatible and a connection cannot be established.

See RELAY-CONDITION
See REMOTE-VERSION")

  (function remote-version
    "Reader for the version of the remote relay system."))

;; containers.lisp
(docs:define-docs
  (type subscription-update
    "Base class for updates relating to the subscription of events.

See TARGET
See SUBSCRIBER
See SUBSCRIPTION
See UNSUBSCRIPTION")

  (function subscriber
    "Accessor to the ID describing the remote that has sent the subscription update.

See SUBSCRIPTION-UPDATE")

  (type subscription
    "An update that describes a desire to have certain kinds of events relayed.

See SUBSCRIPTION-UPDATE
See EVENT-TYPE
See FILTER")

  (function event-type
    "Accessor to the type of events that the subscription update should make a subscription for.

See SUBSCRIPTION")

  (function filter
    "Accessor to the filter description that limits the events that the subscription update should make a subscription for.

See SUBSCRIPTION")

  (type unsubscription
    "An update that describes a desire to no longer have certain kinds of events relayed.

See SUBSCRIPTION-UPDATE")

  (type network-update
    "An update to notify of a change in the network.

The update contains a list of new connections that should
be added, and a list of bad connections that should be
removed.

See NEW
See BAD
See MAKE-NETWORK-UPDATE")

  (function new
    "Accessor to the list of new links to be registered.

This is a list of lists, where each inner list has the
following elements:
- HOP-COUNT    The number of hops necessary to reach the
               destination.
- DESTINATION  The ID of the core of the destination.
- NAME         The name of this particular link.

See NETWORK-UPDATE")

  (function bad
    "Accessor to the list of bad links to be removed.

This is a list of link names.

See NETWORK-UPDATE")

  (function make-network-update
    "This function takes care of creating an appropriate network update to send out to the relays directly connected to this core.

It takes a NEW and BAD argument, which can be the
respective lists directly, COREs, CONSUMERS, or already
existing NETWORK-UPDATES.

See NETWORK-UPDATE")

  (type transport
    "An update that carries an event to a destination.

See EVENT
See TARGET
See MAKE-TRANSPORT")

  (function event
    "Accessor to the event that the transport carries.

See MAIDEN:EVENT
See TRANSPORT")

  (function make-transport
    "Create a transport object to carry the event to the target.

See TRANSPORT"))

;; events.lisp
(docs:define-docs
  (type relay-instruction-event
    "Superclass for all relay events that represent an instruction to be performed on the remote end.

The event can be \"executed\" via the EXECUTE-
INSTRUCTION function.

See EXECUTE-INSTRUCTION
See MAIDEN:INSTRUCTION-EVENT")

  (type data-response-event
    "Superclass for all instruction events that will send back a data response.

See DEEDS:IDENTIFIED-EVENT
See RELAY-INSTRUCTION-EVENT
See SOURCE")

  (function source
    "Reader for the source of the event. This should be an identifier for who requested the instruction.

See DATA-RESPONSE-EVENT")

  (function execute-instruction
    "This function is responsible for actually executing the event that represents an instruction.

See RELAY-INSTRUCTION-EVENT")

  (type slot-event
    "Superclass for all events relating to slot operations on CLOS objects.

The idea of these events is that they would carry operations
that should be performed on an object that exists on a remote
system and is not \"actually there\" on the local system.

See SLOT
See OBJECT
See DATA-RESPONSE-EVENT")

  (function slot
    "Reader for the slot that the event relates to.

See SLOT-EVENT")

  (function object
    "Reader for the object that the event relates to.

See SLOT-EVENT")

  (type slot-value-event
    "Event to represent a slot value read request.

When executed, the object's slot value is read and sent back
over the network.

See EXECUTE-INSTRUCTION
See SLOT-EVENT")

  (type slot-setf-event
    "Event to represent a slot value set request.

When executed, the object's slot value is set to the
requested value.

See EXECUTE-INSTRUCTION
See SLOT-EVENT
See VALUE")

  (function value
    "Reader for the value that the slot should be set to.

See SLOT-SETF-EVENT")

  (type slot-makunbound-event
    "Event to represent a slot unbinding request.

When executed, the object's slot is made unbound.

See EXECUTE-INSTRUCTION
See SLOT-EVENT")

  (type slot-boundp-event
    "Event to represent a slot bound test request.

When executed, it tests whether the slot is bound and sends
the result back over the network.

See EXECUTE-INSTRUCTION
See SLOT-EVENT")

  (type generic-call-event
    "Event to represent a generic call request.

When executed, the requested form is evaluated.

See EXECUTE-INSTRUCTION
See FORM")

  (function form
    "Reader for the form that should be evaluated for the call event

See GENERIC-CALL-EVENT"))

;; relay.lisp
(docs:define-docs
  (type relay
    "This agent represents a relay node.

It is a TCP server that accepts remote connections from
other relay nodes. It manages the updates that need to be
sent and distributed over the PTP relay network. It also
takes care of capturing events that are relevant for other
nodes and sends them out to the interested parties.

There can only be one relay node per core, as otherwise
the messaging and networking infrastructure could much
too easily become confused.

Currently there is no support for loops in the PTP network.
If a cycle appears, it will spawn and endless loop of
updates that will clog the network.

In order for clients to be visible to other nodes in the
network the relay must notify all other nodes of additions
and removals of clients on its core. It must then also
create \"virtual\" representations of the clients on the
other cores on its local core. Without this, it would not
be possible for the system to completely create a
transparent layer that allows agnostic communication with
clients on remote cores.

See SUBSCRIPTIONS
See MY-SUBSCRIPTIONS
See VIRTUAL-CLIENT
See MAIDEN-NETWORKING:TCP-SERVER
See MAIDEN:AGENT")

  (function subscriptions
    "Accessor to the list of SUBSCRIBE objects to keep track of.

See RELAY
See SUBSCRIBE")

  (function my-subscriptions
    "Accessor to the list of SUBSCRIBE objects this node itself generated.

See RELAY
See SUBSCRIBE")

  (function routable-p
    "Returns whether the given target seems to be reachable, or the given event seems to be able to reach its destination from the current relay.

See RELAY")

  (function update
    "Process an update from a source on a relay.

Updates should only be processed on the relay that it
is intended for. This function is also responsible for
updating the internal representation of the network
as well as for delivering the update event to the core.

See RELAY
See NETWORK-UPDATE
See SUBSCRIPTION-UPDATE")

  (function relay
    "Relay a message object to the target over the relay.

This will take care of, if possible, potentially
encapsulating the object and sending it off to its
destination over the PTP network.

See UPDATE
See RELAY")

  (type conect
    "This event is issued to start a connection attempt to a remote relay node.

See CONNECT")

  (function connect
    "This function initiates a connection to a remote relay node.

See CONNECT")

  (type subscribe
    "This event starts the subscription process for a certain event-type.

After a subscription has been sent out, events matching
it on remote cores will be relayed to this core and
will thus be visible.

See SUBSCRIBE")

  (function subscribe
    "This function starts the subscription process for a certain event-type.

See SUBSCRIBE")

  (type unsubscribe
    "This event starts the unsubscription process for a certain subscription.

See SUBSCRIPTION
See MY-SUBSCRIPTIONS
See UNSUBSCRIBE")

  (function unsubscribe
    "This event starts the unsubscription process for a certain subscription.

See SUBSCRIPTION
See MY-SUBSCRIPTIONS
See UNSUBSCRIBE"))

;; virtual-client.lisp
(docs:define-docs
  (type virtual-client
    "This client is a virtual representation of a client instance that exists on a remote core.

Operations on the core will be relayed over the network.
You will not be able to use accessors on the virtual-
client, even if it does \"stand in\" for a certain type
of client that should allow you to use those. However,
accessing slot values should work just fine, albeit they
may be slow to return, since they need to be routed over
the network.

See LINKS
See MAIDEN:CLIENT")

  (function links
    "Accessor to the list of links the actual client can be reached through.

The list should be sorted by proximity of the hops.

See VIRTUAL-CLIENT")

  (function make-virtual-client
    "Create a virtual-client instance for the given target.

See VIRTUAL-CLIENT"))
