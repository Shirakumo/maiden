## Maiden-Commands
This agent is responsible for catching `message-event`s and turning them into `command-event`s if apropriate. It also gives you the `define-command` macro to define user-invokable commands with.

## How To
In order to define a command, use `define-command`, which will generate a matching event, function, and handler for you, similar to `define-instruction`. It will also create the appropriate translation for you through `define-command-invoker`.

First, let's create a consumer and add a command to it.

    (maiden:define-consumer tester () ())

    (maiden-commands:define-command (tester greet) (instance event name &optional greeting)
      (maiden:reply event "~a, ~a" (or greeting "Hello") name))

Now we have to create a test environment for us to work with.

    (defvar *core* (maiden:make-simple-core 'maiden-commands:commands 'tester))
    
We can now call the command as a function.

    (greet *core* "someone")
    
Or through a message that is recognised as a command.
    
    (maiden-commands:issue-message *core* "::greet you")

In order for the latter to work, the `commands` agent must be present on the core.

You can manipulate what is recognised as a command message through the extractors. Calling `remove-command-extractor` on `prefix` will stop the double-colons from being recognised as a command prefix. You can add arbitrary functions that do what you want through `define-command-extractor`. The function should return the substring of the event's message that contains the actual command, or `NIL` if it is not a command message.
