## About
This agent provides handlers that can be de/activated on a per-client/channel basis. This means that handlers, which provide passive reactions not based on commands can be managed with this.

## How To
In order to define a handler that can be de/activated, you must set `activatable-handler` as its superclass and pass the body option `:module`. An example, for a fictitious `beer` agent it might look as follows:

```
(define-handler (beer give-beer message-event) (c ev message)
  :class activatable-handler
  :module #.*package*
  (when (search "I'm thirsty" message)
    (reply ev "Have a beer! c(%)")))
```

We're passing the file's package at read-time here, which is a bit more comfortable than passing the name of the package explicitly. Henceforth the handler can be de/activated using the appropriate functions or commands and the package name.
