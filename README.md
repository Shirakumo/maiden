## About Maiden
Maiden is a collection of systems to help you build applications and libraries that interact with chat servers. It can help you build a chat bot, or a general chat client. It also offers a variety of parts that should make it much easier to write a client for a new chat protocol.

## How To Use Maiden as a Bot
If you only care about using Maiden to set up a bot of some kind, the steps to do so are rather straightforward. First we'll want to load in Maiden and all of the modules and components that you'd like to use in your bot.

```
(ql:quickload '(maiden maiden-irc maiden-command maiden-silly))
```

And then we'll create a core with instances of the consumers added to it as we'd like them to be.

```
(defvar *core* (maiden:make-core
                 '(:maiden-irc :nickname "MaidenTest" :host "irc.freenode.net" :channels ("##testing"))
                 :maiden-command
                 :maiden-silly))
```

The make-core command takes either package names (as strings or symbols) of consumers to add, or the direct class name of a consumer. In the former case it'll try to find the appropriate consumer class name on its own.

And that's it. `make-core` will create a core, instantiate all the consumers, add them to it, and start everything up. A loot of the modules provided for Maiden will make use of some kind of configuration or persistent storage. For the management thereof, see the [storage](modules/storage/) subsystem.

## How To Use Maiden as a Framework to Develop With
In order to use Maiden as a framework, you'll first want to define your own system and package as usual for a project. For now we'll just use the `maiden-user` package to play around in. Next we'll want to define a consumer. This can be done with `define-consumer`.

```
(in-package #:maiden-user)
(define-consumer ping-notifier (agent)
  ())
```

Usually you'll want to define an agent. Agents can only exist once on a core. We'll go through an example for a client later. Now, from here on out we can define our own methods and functions that specialise or act on the consumer class as you'd be used to from general CLOS programming. Next, we'll define our own event that we'll use to send "ping requests" to the system.

```
(define-event ping (passive-event)
  ())
```

The event is defined as a `passive-event` as it is not directly requesting an action to be taken, but rather informs the system of a ping that's happening. Now, in order to actually make the consumer interact with the event system however, we'll also want to define handlers. This can be done with `define-handler`.

```
(define-handler (ping-notifier ping-receiver ping) (c ev)
  (v:info :ping "Received a ping: ~a" ev))
```

This defines a handler called `ping-receiver` on our `ping-notifier` consumer. It also specifies that it will listen for events of type `ping`. The arglist afterwards says that the consumer instance is bound to `c` and the event instance to `ev`. The body then simply logs an informational message using [Verbose](https://shinmera.github.io/verbose).

Let's test this out real quick.

```
(defvar *core* (make-core 'ping-notifier))

(do-issue *core* ping)
```

That should print the status message to the REPL as expected. And that's most of everything there is to using this system. Note that in order to do actually useful things, you'll probably want to make use of some of the preexisting subsystems that the Maiden project delivers aside from the core. Those will help you with users, channels, accounts, commands, networking, storage, and so forth. Also keep in mind that you can make use of the features that [Deeds](https://shinmera.github.io/deeds) offers on its own as well, such as filtering expressions for handlers.

Now let's take a look at a primitive kind of client. The client will simply be able to write to a file through events.

```
(define-consumer file-client (client)
  ((file :initarg :file :accessor file))
  (:default-initargs :file (error "FILE required.")))
  
(define-event write-event (client-event active-event)
  ((sequence :initarg :sequence))
  (:default-initargs :sequence (error "SEQUENCE required.")))
```

We've made the `write-event` a `client-event` since it needs to be specific to a client we want to write to, and we've made it an `active-event` since it requests something to happen. Now let's define our handler that will take care of actually writing the sequence to file.

```
(define-handler (file-client writer write-event) (c ev sequence)
  :match-consumer 'client
  (with-open-file (stream (file c) :direction :output :if-exists :append :if-does-not-exist :create)
    (write-sequence sequence stream)))
```

The `:match-consumer` option modifies the handler's filter in such a way that the filter will only pass events whose `client` slot contains the same `file-client` instance as the current handler instance belongs to. This is important, as each instance of `file-client` will receive its own instances of its handlers on a core. Without this option, the `write-event` would be handled by every instance of the `file-client` regardless of which instance the event was intended for. Also note that we added a `sequence` argument to the handler's arglist. This argument will be filled with the appropriate slot from the event. If no such slot could be found, an error is signalled.

Time to test it out. We'll just reuse the core from above.

```
(add-to-core *core* '(file-client :file "~/foo" :name :foo)
                    '(file-client :file "~/bar" :name :bar))

(do-issue *core* write-event :sequence "foo" :client (consumer :foo *core*))
(do-issue *core* write-event :sequence "bar" :client (consumer :bar *core*))

(alexandria:read-file-into-string "~/foo") ; => "foo"
(alexandria:read-file-into-string "~/bar") ; => "bar"
```

As you can see, the events were directed to the appropriate handler instances according to the client we wanted, and the files thus contain what we expect them to.

Finally, it is worth mentioning that it is also possible to dynamically add and remove handlers at runtime, and even do so for handlers that are not associated with a particular consumer. This is often useful when you need to wait for a response event from somewhere. To handle the logic of doing this asynchronously and retain the impression of an imperative flow, Maiden offers --just as Deeds does-- a `with-awaiting` macro. It can be used as follows:

```
(with-awaiting (core event-type) (ev some-field)
    (do-issue core initiating-event)
  :timeout 20
  some-field)
```

`with-awaiting` is very similar to `define-handler`, with the exception that it doesn't take a name, and instead of a consumer name at the beginning it needs a core or consumer instance. It also takes one extra option that is otherwise unused, the `:timeout`.

And that's pretty much all of the basics. As mentioned above, take a look at the subsystems this project includes, as they will help you with all sorts of common tasks and problems revolving around chat systems and so on.

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
