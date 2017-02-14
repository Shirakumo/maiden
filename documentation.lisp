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

  (function consumer-name-duplicated-warning
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

  (function agent-already-exists-error
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

See CONSUMER-CLASS
See ABSTRACT-HANDLER")

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

  )

;; core.lisp
(docs:define-docs)

;; entity.lisp
(docs:define-docs)

;; event.lisp
(docs:define-docs)

;; standard-events.lisp
(docs:define-docs)

;; toolkit.lisp
(docs:define-docs)
