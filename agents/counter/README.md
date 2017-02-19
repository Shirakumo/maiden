## About
This agent provides simple, silly counter functionality. It detects messages by a regex and then increases a counter and replies with a message.

## How To
There's several commands to manage the counters.

```
::add counter foo \\bfoo|bar|baz|ban\\b "~a examples have been posted."
```

When a message is encountered that contains `foo`, `bar`, `baz`, or `ban`, the counter is increased and a response is given as specified above. Simple stuff. See the symbol index for the other commands.

The command handler is an activatable handler, so it must be activated explicitly first. See [maiden-activatable](../activatable/).
