#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden)

;; agent.lisp
(docs:define-docs
  (type agent
    "A type of consumer of which only one instance should exist on a core.

An agent's name defaults to the agent's class name.

An agent MATCHES if the class or the class name matches.

If an agent is attempted to be added to a core when an
agent that matches it by name already exists on the core,
a warning of type AGENT-ALREADY-EXISTS-ERROR is signalled.

See CONSUMER"))

;; client.lisp
(docs:define-docs
  (type client
    "A type of consumer of which multiple instances can exist on a core.

See CONSUMER"))

;; conditions.lisp
(docs:define-docs
  (type maiden-condition
    "Superclass for all condition types in the Maiden system.")

  (type core-condition
    "Superclass for all conditions related to operations on a core.

See CORE
See MAIDEN-CONDITION")

  (function core
    "Accessor to the core that this object holds.

See CORE-CONDITION")

  (type consumer-name-duplicated-warning
    "A condition signalled when a consumer is added to a core and has the same name as an already existing consumer.

See EXISTING-CONSUMER
See NEW-CONSUMER
See CORE-CONDITION")

  (function existing-consumer
    "Reader for the consumer that previously already existed on the core.

See CONSUMER-NAME-DUPLICATED-WARNING")

  (function new-consumer
    "Reader for the new consumer that is being added to the core.

See CONSUMER-NAME-DUPLICATED-WARNING")

  (type agent-condition
    "Superclass for all conditions related to agents.

See AGENT
See MAIDEN-CONDITION")

  (function agent
    "Accessor to the agent that this object holds.

See AGENT-CONDITION")

  (type agent-already-exists-error
    "A condition signalled when an agent of the same name already exists on the core.

See EXISTING-AGENT
See AGENT-CONDITION
See CORE-CONDITION")

  (function existing-agent
    "Reader for the agent that already exists on the core.

See AGENT-ALREADY-EXISTS-ERROR")

  (type client-condition
    "Superclass for all conditions related to clients.

See CLIENT
See MAIDEN-CONDITION")

  (function client
    "Accessor to the client that this object holds.

See CLIENT-CONDITION"))

;; consumer.lisp
(docs:define-docs
  (type consumer-class
    "Metaclass for all consumer objects.

It handles the proper instantiation of handler objects when the
consumer is added to a core or the handler definitions are
updated.

See DIRECT-HANDLERS
See EFFECTIVE-HANDLERS
See INSTANCES
See CONSUMER")

  (function direct-handlers
    "Accessor to the list of direct handler definitions on the consumer class.

This only holds handler definitions that have been defined for
this specific class directly.
Also note that the handler objects contained in this list are
only abstract-handler instances and cannot be directly used as
handlers.

When this place is set, the consumer-class' inheritance is
finalized.

See CONSUMER-CLASS
See ABSTRACT-HANDLER
See MOP:FINALIZE-INHERITANCE")

  (function effective-handlers
    "Accessor to the list of effective handler definitions on the consumer class.

This holds all handler definitions, including inherited ones.
Note that the handler objects contained in this list are only
abstract-handler instances and cannot be directly used as
handlers.

When this place is set, the list of INSTANCES is updated and
each existing instance is reinitialised through REINITIALIZE-
HANDLERS.

See CONSUMER-CLASS
See ABSTRACT-HANDLER
See INSTANCES
See REINITIALIZE-HANDLERS")

  (function instances
    "Accessor to the list of weak-pointers to consumer instances.

The elements in the list are instances of TG:WEAK-POINTER and
may point to instances of the consumer class. This list is
necessary to keep track of and properly synchronise the handlers
upon redefinition.

This list is updated whenever a new CONSUMER instance is created
or when EFFECTIVE-HANDLERS of its class is set.

See TG:WEAK-POINTER
See CONSUMER")

  (function cascade-handler-changes
    "This function computes the effective handlers list for the class and cascades the update downwards in the hierarchy.

Only subclasses that are a subclass of CONSUMER-CLASS and have
been finalized are cascaded.

The EFFECTIVE-HANDLERS list is updated as part of this.

See CONSUMER-CLASS
See EFFECTIVE-HANDLERS")

  (function update-handler
    "This function is responsible for handling the redefinition of a handler on a consumer-class.

This function simply updates the list of direct-handlers
on the class by replacing the corresponding abstract-handler
or adding it to it.

The class-ish can be a CONSUMER-CLASS, a CONSUMER, or a
SYMBOL naming a consumer-class.

See DIRECT-HANDLERS
See ABSTRACT-HANDLER
See CONSUMER-CLASS")

  (function remove-handler
    "Removes the handler from the consumer-class.

This function simply updates the list of direct-handlers
on the class by removing the corresponding abstract-handler.

The class-ish can be a CONSUMER-CLASS, a CONSUMER, or a
SYMBOL naming a consumer-class.

The abstract-handler can be an ABSTRACT-HANDLER, or a
SYMBOL denoting the NAME of an abstract-handler.

See DIRECT-HANDLERS
See ABSTRACT-HANDLER
See CONSUMER-CLASS")

  (type consumer
    "Superclass for all consumers on a core.

Consumers are responsible for issuing and responding to
events that happen on a core. They do this by having a number
of handler definitions tied to them, which are instantiated
into proper handlers when they are added to a core. These
handlers are then added to the core's event-loop in order to
react to events. See the Deeds library for information on how
the event-loop and handlers work in detail.

Consumers are divided into two classes, ones of which only a
single instance should exist on the core, and ones of which
many may exist on the core. The former are called AGENTS, and
the latter are called CLIENTS. The former usually provide
functionality that is reactionary in some sense. The latter
usually provide some form of connection to another entity and
primarily provide events rather than consuming them. You should
not inherit directly from CONSUMER therefore, and rather pick
either CLIENT or AGENT, depending on which of the two is more
suitable for the kind of consumer you want to write for the
system.

Consumer classes must inherit from the CONSUMER-CLASS class,
which is responsible for ensuring that handler definitions get
properly instantiated and managed over consumer instances.

In order to easily define consumer classes with the appropriate
superclass and metaclass, you can use DEFINE-CONSUMER.

In order to add handlers to the consumer, use DEFINE-HANDLER.

Each consumer has a LOCK that can be used to synchronise
access to the consumer from different parts in the system.
Since Deeds, and Maiden by extension, is highly parallel most
of the time, locking of resources and access to the consumer
from different handlers is vital.

The list of handler instances is held in the HANDLERS slot.
The list of cores the consumer is on is held in the CORES slot.

You can start and stop all the handlers on a consumer by the
usual Deeds START and STOP functions.

After the initialisation of a consumer, the consumer instance
is pushed onto the INSTANCES list of its class by way of a
weak-pointer. It will also turn all of its effective-handlers
into actual handler instances by way of INSTANTIATE-HANDLER and
push them onto its HANDLERS list.

See NAMED-ENTITY
See CONSUMER-CLASS
See AGENT
See CLIENT
See DEFINE-CONSUMER
See DEFINE-HANDLER
See HANDLERS
See CORES
See LOCK
See START
See STOP
See INSTANCES
See INSTANTIATE-HANDLER")

  (function cores
    "Accessor to the list of cores the consumer is currently registered with.

See CONSUMER")

  (function lock
    "Accessor to the lock that is used to synchronise access to this object.

See CONSUMER")

  (function reinitialize-handlers
    "This handles the updating of handler definitions on a consumer.

The procedure is as follows:
1. The consumer is removed from all its cores
2. The handlers list is emptied
3. New handlers are created from the list of abstract handlers
4. The consumer is added to all of its cores again

See CONSUMER
See INSTANTIATE-HANDLER
See HANDLERS")

  (type abstract-handler
    "This is an object to represent a handler definition. It contains all data necessary to construct an appropriate handler instance for a consumer.

See TARGET-CLASS
See OPTIONS
See INSTANTIATE-HANDLER
See DEFINE-HANDLER")

  (function target-class
    "Accessor to the target class that the actual handler should be of when the abstract-handler is instantiated.

Defaults to DEEDS:QUEUED-HANDLER

See ABSTRACT-HANDLER
See DEEDS:QUEUED-HANDLER")

  (function options
    "Accessor to the list of initargs that the handler should receive upon instantiation.

See ABSTRACT-HANDLER")

  (function instantiate-handler
    "This function creates an actual handler instance from the abstract handler definition.

The instantiation proceeds as follows:
1. The options :FILTER, :DELIVERY-FUNCTION, and :MATCH-CONSUMER
   are extracted from the options list.
2. If :MATCH-CONSUMER is given and is eql to T, then the :FILTER
   option is extended by surrounding it as follows:
     (and (eq ,consumer consumer) ..)
   where ,consumer denotes the consumer instance passed to
   instantiate-handler.
3. If :MATCH-CONSUMER is given and is not eql to T, then the :FILTER
   option is extended by surrounding it as follows:
     (and (eq ,consumer ,match-consumer) ..)
   where ,consumer is as above and ,match-consumer is the value of
   the :MATCH-CONSUMER option.
4. MAKE-INSTANCE is called with the TARGET-CLASS of the abstract
   handler, a :delivery-function initarg that is a function that
   calls the :DELIVERY-FUNCTION extracted from the option with the
   consumer and the event, a :filter initarg that is the value of
   the :FILTER option, and the rest of the OPTIONS of the abstract
   handler.

See ABSTRACT-HANDLER")

  (function define-handler
    "Defines a new handler on the consumer class.

CONSUMER    must be the class-name of the consumer to define on.
NAME        must be a symbol denoting the name of the handler
            definition. Note that this name will not be carried over
            to actual handler instances, as they would otherwise
            clash on multiple consumer instances on the same core.
EVENT-TYPE  must be a base class for all events that the handler
            will receive.
ARGS        must be a list of arguments, of which the first two will
            be bound to the consumer instance and the event
            respectively. The rest of the arguments denote fuzzy slot
            bindings of the event.
BODY        a number of extra handler definition options as a plist
            followed directly by a number of forms to evaluate upon
            receiving an event.

The body options are evaluated and passed as class initargs to the
resulting handler instance once one is constructed. Note that as such
the values will be shared across all instances of the handler defined
here. Also note that there are two options which are exempt from
this and play special roles:

  :DELIVERY-FUNCTION This option is already provided by default.
                     Supplying it manually will mean that the body
                     forms of the DEFINE-HANDLER will be ignored.
  :MATCH-CONSUMER    Should be a slot name of the event that needs
                     to match the consumer for the event to be handled.
                     You'll want to use this option for handlers of
                     clients, in order to ensure that the handler from
                     the client instance that matches the client the
                     event is intended for is called.

In effect this constructs an appropriate ABSTRACT-HANDLER instance
and calls UPDATE-HANDLER with it on the consumer class.

See DEEDS:WITH-FUZZY-SLOT-BINDINGS
See DEEDS:WITH-ORIGIN
See ABSTRACT-HANDLER
See UPDATE-HANDLER
See REMOVE-HANDLER")

  (function slot-args->slots
    "Converts a list of slot arguments into slot definitions.

The slot-args must be structured like a special kind of lambda-list:

SLOT-ARGS    ::= REQ* (&optional OPT*)? (&REST REQ)? (&key OPT*)?
REQ          ::= name | (name SLOT-INITARG*)
OPT          ::= name | (name default-value? SLOT-INITARG*)
SLOT-INITARG ::= keyword value

In short:
There are no provided-p predicate variables for optional arguments,
but each argument can be followed by a list of possible slot initargs
if it is given as a list.

Each returned slot will also automatically receive an initarg of the
same name as the slot, but from the keyword package.")

  (function slot-args->args
    "Converts a list of slot arguments into a lambda list.

See SLOT-ARGS->SLOTS")

  (function args->initargs
    "Converts a lambda-list into a plist to pass as initargs.

More specifically, each variable in the lambda list will be turned
into a key-value pair where the key is the keyword corresponding to
the symbol name, followed by the symbol itself.")

  (function define-function-handler
    "Shorthand macro to define an event an a corresponding handler in one go.

Special body options are extracted to provide further control over
the definition of the event:
  :SUPERCLASSES  The superclass list to use.
  :EXTRA-SLOTS   A list of extra slot definitions.
  :CLASS-OPTIONS A list of extra class options.
  :DOCUMENTATION The docstring to use for the class.
  :ADVICE        The advice value to use for the event.
Note that these options will NOT be passed on to the DEFINE-HANDLER
form.

The ARGS are used both for the arguments to DEFINE-HANDLER and as
slot definitions by way of SLOT-ARGS->SLOTS.

See SLOT-ARGS->SLOTS
See DEFINE-EVENT
See DEFINE-HANDLER
See REMOVE-FUNCTION-HANDLER")

  (function remove-function-handler
    "Shorthand function to remove a function-handler definition.

This removes both the event class and the handler it defined.

See CL:FIND-CLASS
See REMOVE-HANDLER")

  (function define-instruction
    "Shorthand macro to define an instruction-like event.

This is essentially the same as DEFINE-FUNCTION-HANDLER with
the following additions:
- INSTRUCTION-EVENT is always injected as a superclass.
- A function of the same name as the instruction is generated that
  creates the appropriate event and sends it off to a core.

This thus allows you to simulate a standard function interface
for code that runs over the event-loop. Note that the generated
function will not wait for a response to the event and immediately
returns. The returned value is the generated event instance.

See DEFINE-FUNCTION-HANDLER
See BROADCAST
See REMOVE-INSTRUCTION")

  (function remove-instruction
    "Shorthand function to remove an instruction definition.

This removes both the event class, the handler, and the issue-
function it defined.

See CL:FMAKUNBOUND
See REMOVE-FUNCTION-HANDLER")

  (function define-query
    "Shorthand macro to define a query-like event.

This is similar to DEFINE-INSTRUCTION, with the exception that
possibly two events (one for issue and one for response) are
generated, and that the issue function will await a response
and return with the intended return value, thus simulating a
complete function API over the event system.

If no explicit EVENT-RESPONSE-TYPE is specified, a generic
response event is used instead. See RESPOND.

See RESPOND
See REMOVE-QUERY")

  (function remove-query
    "Shorthand function to remove a query definition.

This removes both the event classes, the handler, and the issue-
function it defined.

See REMOVE-FUNCTION-HANDLER
See CL:FIND-CLASS
See CL:FMAKUNBOUND")

  (function define-consumer
    "Shorthand to define a consumer class.

This is like CL:DEFCLASS, with the appropriate superclass and
metaclass injected for you. It also makes sure that the class
definition is available during compile-time as well.

See CONSUMER
See CONSUMER-CLASS"))

;; core.lisp
(docs:define-docs
  (function consumer
    "Retrieve a consumer from the core.

If no consumer that matches the ID is found, NIL is returned.

See MATCHES")

  (function add-consumer
    "Add the consumer to the core.

If the consumer already exists on the core, nothing is done.
If a consumer of the same name already exists on the core, a
warning of type CONSUMER-NAME-DUPLICATED-WARNING is signalled.

If a consumer has been added, an event of type CONSUMER-ADDED
is issued onto the core.

See CONSUMER-NAME-DUPLICATED-WARNING
See CONSUMER-ADDED")

  (function remove-consumer
    "Remove the consumer from the core.

If the consumer doesn't exist on the core, nothing is done.
Otherwise the consumer is removed from the core's list.

If a consumer has been removed, an event of type CONSUMER-
REMOVED is issued onto the core.

See CONSUMER-REMOVED")

  (type core
    "The core of an event system in Maiden.

The core is responsible for managing events, consumers, and
their handlers. It uses two (!) event-loops in the back to
handle event delivery. The first loop, called the primary
loop is where most handlers live. It is (by default) of type
PRIMARY-LOOP and should be fairly speed in delivery, at
the cost that adding and removing handlers will be slow. The
second loop, called the blocking loop is where temporary
handlers that only exist for hopefully a short time live. It
is (by default) of type BLOCK-LOOP and is not optimised
for fast delivery, but much faster at removing and adding
handlers. Thus, whenever you wait for an event for a one-time
request, the handler should be added to the block loop.

Calling DE/REGISTER-HANDLER on a core will automatically add
it to the primary loop. If you want to change the blocking
loop you will have to access it directly.

When an event is ISSUEd onto the core, it is ISSUEd onto the
primary loop and then ISSUEd onto the block loop. The
behaviour for when an event is directly HANDLEd by the core
is analogous.

See PRIMARY-LOOP
See BLOCK-LOOP
See CONSUMERS
See CONSUMER
See ADD-CONSUMER
See REMOVE-CONSUMER
See START
See STOP
See ISSUE
See HANDLE
See WITH-AWAITING
See WITH-RESPONSE
See MAKE-CORE
See ADD-TO-CORE")

  (function primary-loop
    "Accessor to the primary loop of the Maiden core.

This should take care of the bulk of handlers and events.

See CORE")

  (function block-loop
    "Accessor to the blocking back loop of the Maiden core.

This should govern one-time handlers and response events.

See CORE")

  (function consumers
    "Accessor to the list of consumers associated with the core.

See CONSUMER
See CORE")

  (type primary-loop
    "Base class for the primary loop on a Maiden core.

See CORE
See DEEDS:COMPILED-EVENT-LOOP")

  (type block-loop
    "Base class for the block loop on a Maiden core.

See CORE
See DEEDS:EVENT-LOOP")

  (function with-awaiting
    "Waits for a response event to arrive on the core before evaluating the body.

This is useful to write event-driven, reactionary code.
The temporary handler to catch the code is added to the
core's back loop.

Note that CORE can be one of
- CORE              The temporary handler is attached to
                    the core's block-loop.
- CONSUMER          The first core on the client's list of
                    cores is used as above.
- DEEDS:EVENT-LOOP  The temporary handler is directly
                    attached to it.

See DEEDS:WITH-AWAITING")

  (function make-core
    "Construct a new core instance and add consumers to it.

The resulting core will be started.

See CORE
See ADD-TO-CORE")

  (function add-to-core
    "Easily add consumers to a core.

The consumers will be started after having been added to
the core. Each consumer in the list of consumers can be:

- A symbol denoting the class of consumer to construct
- A string or keyword denoting a package that homes a
  symbol denoting a consumer class.
- A list starting with one of the above followed by the
  initargs for the class instantiation.

The instances are all constructed before any of them are
added to the core or started, so as to catch errors early."))

;; entity.lisp
(docs:define-docs
  (function matches
    "Generic comparator operator.

This compares in a potentially ambiguous \"dwim\" sense.
Various components in the system add methods to make the
matching work as much as expected as possible.")

  (type entity
    "Superclass for things that are comparable according to some kind of identity.

See ID")

  (function id
    "Accessor to the IDentity of an entity.

By default this is initialised to a fresh UUIDv4 string.

See ENTITY")

  (type named-entity
    "An entity with a human-readable name attached to it.

See ENTITY
See NAME")

  (function name
    "Aecessor to the name of the entity.

See NAMED-ENTITY")

  (function find-entity
    "Attempt to find an entity in a container.

The ID must be matched against each entity in the container
by MATCHES.

See ENTITY
See MATCHES")

  (type data-entity
    "Superclass for entities that have a data storage table.

See ENTITY
See DATA
See DATA-VALUE")

  (function data
    "Accessor to the data storage container for the data entity.

See DATA-ENTITY
See DATA-VALUE")

  (function data-value
    "Accessor for a single data field in the data entity.

See DATA
See DATA-ENTITY"))

;; event.lisp
(docs:define-docs
  (type event-class
    "Metaclass for events.

In addition to the effects inherited from DEEDS:EVENT-CLASS,
this class ads so-called \"advice\". Advice denotes advisory
information about the event that may or may not be processed
depending on whether there are any components that can make
use of the advice.

The advice list is inherited through the class structure.
Advice can also be inhibited for a class and its descendants
by surrounding the advice item with a (NOT ..). Note that
advice items are only tested for equality by EQL.

See DEEDS:EVENT-CLASS
See ADVICE
See DIRECT-ADVICE")

  (function advice
    "Accessor to the advice information on the event or event-class.

See EVENT-CLASS
See EVENT")

  (function direct-advice
    "Accessor to the advice information that was defined directly on the class.

See EVENT-CLASS")

  (type event
    "The superclass for all events in Maiden.

An event is an object that represents a change that occurs
in the system. It may also carry relevant information about
the change. Events can also be used to signify requests for
things to happen in the system.

Events need to be ISSUEd onto a core, where they will then
be dispatched to HANDLERs that can process it.

See EVENT-CLASS
See CORE
See DEEDS:EVENT
See DEFINE-EVENT")

  (function define-event
    "Shorthand macro to define an event class.

This takes care of potentially injecting the EVENT superclass
and setting the necessary EVENT-CLASS metaclass. Otherwise it
is identical to CL:DEFCLASS.

See CL:DEFCLASS
See EVENT
See EVENT-CLASS"))

;; standard-events.lisp
(docs:define-docs
  (type passive-event
    "Superclass for all passive events in the system.

A passive event notifies of a change that happened somewhere
in the system. It is passive in the sense that it provides
information, rather than explicitly requesting information or
explicitly requesting an action.

See EVENT
See ACTIVE-EVENT")

  (type active-event
    "Superclass for all active events in the system.

An active event notifies of a request for an action to be
taken somewhere in the system. It is active in the sense that
it should cause some part of the system to perform an action,
rather than merely notifying of a change happening.

See EVENT
See PASSIVE-EVENT")

  (type client-event
    "Superclass for all events that relate to a client.

This event holds the client it relates to in a slot.

See CLIENT
See EVENT")

  (type instruction-event
    "Superclass for all instruction events in the system.

Instructions are event representations of a change request in
the system. They often represent a \"virtual\" function call.

See DEFINE-INSTRUCTION
See ACTIVE-EVENT")

  (function respond
    "Respond to the event in an appropriate way.

The response event will be issued on to the same core that
the event being responded to was issued to. If the event
does not have a specific response event already (through a
specialised method on RESPOND), then you may specify the
class to use with the :CLASS initarg.")

  (type query-event
    "Superclass for all query events in the system.

Queries are events that represent both a change request and
a request for a result to be obtained about the change. They
often represent a \"full\" function call.

It is identified so that it and its response event counter-
piece can be found.

See DEFINE-QUERY
See DEEDS:IDENTIFIED-EVENT
See INSTRUCTION-EVENT")

  (type response-event
    "Generic response event class.

This event is used to deliver the response payload of a
query-event. Its IDentity must be the same as that of the
query-event that prompted it.

See DEEDS:IDENTIFIED-EVENT
See DEEDS:PAYLOAD-EVENT
See PASSIVE-EVENT")

  (type core-event
    "Superclass for all events relating to a Maiden core.

See EVENT")

  (type consumer-added
    "Event that is issued after a consumer has been added to the core.

See CORE-EVENT
See CONSUMER")

  (type consumer-removed
    "Event that is issued after a consumer has been removed from the core.

See CORE-EVENT
See CONSUMER"))

;; toolkit.lisp
(docs:define-docs
  (variable *root*
    "This variable holds a directory pathname that points to the \"root\" of the Maiden installation.

The root should mainly be used for storage of runtime
fragments such as configuration, cache, and so forth.")

  (variable *debugger*
    "This variable sets whether an internal error should call out to the debugger or not.

On deployed systems, this should probably be NIL. The
default value is whether the SWANK package is present
or not.")

  (function maybe-invoke-debugger
    "Might invoke the debugger with the condition.

If *DEBUGGER* is non-NIL, the debugger is invoked with
a CONTINUE restart surrounding it to allow giving up on
handling the condition. Otherwise, if the RESTART
argument is passed, that restart is invoked with the
rest of the arguments as values for the restart.

See *DEBUGGER*")

  (function update-root-for-image
    "This function attempts to update the *ROOT* to the current location of the image.

See UIOP:ARGV0")

  (function xor
    "If either A or B are true, but not both.")

  (function xnor
    "If both A and B are either true or false at the same time.")

  (function kw
    "Return the keyword corresponding to the given symbol designator.")

  (function enlist
    "If THING is a list, it is returned. Otherwise a new list out of the given elements is constructed.")

  (function unlist
    "If THING is not a list, it is returned. Otherwise the element by KEY from the list is returned.")

  (function starts-with
    "Returns true if SEQUENCE begins with START.")

  (function with-default-encoding
    "Evaluate BODY in an environment where the default external format is set to the given encoding.

Only works on:
- SBCL
- CCL")

  (function make-updated-list
    "Construct an updated version of LIST where THING is either updated from its old value, or added to it.

The elements in the list are passed through KEY and then
compared against THING using the TEST function.")

  (function update-list
    "Macro to update the list with a new item.

The item is added to the list if it is not yet contained
and updated in-place otherwise.

See MAKE-UPDATED-LIST")

  (function with-retry-restart
    "Evaluates body around which a restart is established that allows retrying the evaluation of the body.

Similar to CL:WITH-SIMPLE-RESTART.")

  (function do-issue
    "Shorthand macro to construct and issue an event onto a core.

The event-type should not be quoted.

See DEEDS:DO-ISSUE")

  (function broadcast
    "Shorthand function to construct and issue an event onto a set of cores.

Unlike DO-ISSUE, this is a function, so the event-type
has to be quoted.")

  (function named-lambda
    "Attempt to construct a lambda with a name.

Note that standard name clashing rules apply and naming the lambda
after a CL function will likely fail if the implementation supports
package locks.")

  (variable *unix-epoch-difference*
    "The difference in seconds between universal-time and unix-time.")

  (function universal-to-unix
    "Convert universal-time to unix-time.")

  (function unix-to-universal
    "Convert unix-time to universal-time.")

  (function get-unix-time
    "Returns the time in seconds since the unix epoch of 1970.")

  (function format-relative-time
    "Formats the time in seconds as a human-readable string.

Time is split up into seconds, minutes, hours, days, weeks, months,
years, decades, centuries, and æons.")

  (function format-absolute-time
    "Formats the universal-time as a timestring in the format of YYYY.MM.DD hh:mm:ss")

  (function format-time
    "Formats the universal-time in a (hopefully) appropriate manner.

If the time differs from now by more than the RELATIVE-TIME-THRESHOLD
then the time is printed absolutely, otherwise relatively.

See FORMAT-RELATIVE-TIME
See FORMAT-ABSOLUTE-TIME")

  (function find-consumer-in-package
    "Scans through the symbols in the given package and attempts to find one that denotes a class that is a subclass of CONSUMER.

The first such symbol found is returned."))
