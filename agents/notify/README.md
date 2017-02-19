## About
This agent provides notification messages, allowing users to write eachother reminders in case one should be temporarily unresponsive or offline. Notes will be persisted, and delivered to the target user as soon as it appears that they are responsive again.

## How To
You can create a note to be delivered when the user next speaks, or when they next join a visible channel.

    ::notify SomeDude Hey man, where have you been?
    ::notify on join SomeDude Whoa, hello! Long time no see.

The message will be delivered as soon as appropriate. If you already saw the message and don't want to be notified of them again, you can throw them away.

    ::forget notes

And that's it. Naturally this interface is also available directly from the REPL.
