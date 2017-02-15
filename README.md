## About Maiden
Maiden is a collection of systems to help you build applications and libraries that interact with chat servers. It can help you build a chat bot, or a general chat client. It also offers a variety of parts that should make it much easier to write a client for a new chat protocol.

## How To
TBD

## Core Documentation
Before understanding Maiden, it is worth it to understand [Deeds](https://shinmera.github.io/deeds), if only at a surface level. Maiden builds on it rather heavily.

### Core
A `core` is the central part of a Maiden configuration. It is responsible for managing and orchestrating the other components of the system. You can have multiple cores running simultaneously within the same lisp image, and can even share components between them.

More specifically, a Core is made up of an event-loop and a set of consumers. The event-loop is responsible for delivering events to handlers. Consumers are responsible for attaching handlers to the event-loop. The operations you will most likely want to perform on a core are thus: issuing events to it by `issue`, adding consumers to it by `add-consumer`, or removing a consumer from it by `remove-consumer`.

In order to make it easier on your to create a useful core with consumers added to it, you can make use of the `make-core` and `add-to-core` functions.

### Event
An `event` is an object that represents a change in the system. Events can be used to either represent a change that has occurred, or to represents a request for a change to happen. These are called `passive-event`s and `active-event`s respectively.

Generally you will use events in the following ways:

1. Consuming them by writing a handler that takes events of a particular type and does something in response to them.
2. Define new event classes that describe certain behaviour.
3. Emitting them by writing components that inform the system about changes.

### Consumer
A `consumer` is a class that represents a component in the system. Each consumer can have a multitude of handlers tied to it, which will react to events in the system. Consumers come in two basic supertypes, `agent`s and `client`s. Agents are consumers that should only exist on a core once, as they implement functionality that would not make sense to be multiplexed in some way. Clients on the other hand represent some kind of bridge to an outside system, and naturally should be allowed to have multiple instances on the same core.

Thus developing a set of commands or an interface of some kind would probably lead to an agent, whereas interfacing with a service like XMPP would lead to a client.

Defining a consumer should happen with `define-consumer`, which is similar to the standard `defclass`, but ensures that the superclasses and metaclasses are properly set up.

### Handler
`handler`s are objects that hold a function that performs certain actions when a particular event is issued onto the core. Each handler is tied to a particular consumer and is removed or added to the core's event-loop when the consumer is removed or added to the core.

Handler definition happens through one of `define-handler`, `define-function-handler`, `define-instruction`, or `define-query`. Which each successively build on the last to provide a broader shorthand for common requirements. Note that the way in which a handler actually receives its events can differ. Have a look at the Deeds' documentation to see what handler classes are available.

## Subsystems
Included in the Maiden project are a couple of subsystems that extend the core functionality.

* [API Access](modules/api-access/)
* [Client Entities](modules/client-entities/)
* [Networking](modules/networking/)
* [Serialize](modules/serialize/)
* [Storage](modules/storage/)

## Existing Clients
The Maiden project also includes a few standard clients that can be used right away.

* [IRC](clients/irc/)
* [Lichat](clients/lichat/)
* [Relay](clients/relay/)

## Existing Agents
Finally, the project has a bunch of agent modules that provide functionality that is useful for creating chat bots and such. They, too, can be used straight away.

* [accounts](agents/accounts/)
* [activatable](agents/activatable/)
* [blocker](agents/blocker/)
* [chatlog](agents/chatlog/)
* [commands](agents/commands/)
* [core-manager](agents/core-manager/)
* [counter](agents/counter/)
* [crimes](agents/crimes/)
* [emoticon](agents/emoticon/)
* [help](agents/help/)
* [location](agents/location/)
* [markov](agents/markov/)
* [medals](agents/medals/)
* [notify](agents/notify/)
* [permissions](agents/permissions/)
* [quicklisp](agents/quicklisp/)
* [silly](agents/silly/)
* [talk](agents/talk/)
* [throttle](agents/throttle/)
* [time](agents/time/)
* [trivia](agents/trivia/)
* [urlinfo](agents/urlinfo/)
* [weather](agents/weather/)
