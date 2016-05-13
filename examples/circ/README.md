## Primitive Maiden Based IRC Client
This is a very small and primitive IRC client application to be used in the SLIME REPL. It serves mostly as a starting point for those who want to write a real client using Maiden3 as the back-end.

## How To:
Currently neither Maiden3 nor the latest version of Deeds are on Quicklisp, so you'll have to clone them into your `local-projects` or wherever. 
For Maiden, clone the `v3` branch of `https://github.com/Shinmera/maiden` and for Deeds the `master` branch of `https://github.com/Shinmera/deeds`.

Once you got that, make sure to `(ql:register-local-projects)`, then simply `(ql:quickload :maiden-circ)`, and finally `(in-package #:circ)`.

Next you have to start up everything, which can be done by the `init` function. Using it without any arguments will setup a simple local IRC client. However, if you pass it `:relay T` it will also set it up to be able to relay events and such to another, remote client that will share the IRC connections rather than establish them by itself. If you pass it `:remote T` it will connect to a relay and use that for everything. Instead of `T` for both you can pass a list with the hostname and port to use.

Once the init is done, you can `connect` to an IRC server like so: `(connect *core* 'freenode "irc.freenode.net" "CircTest")`. The `*core*` argument is necessary due to the way Maiden automatically creates commands, which has to support the scenario of multiple cores in the same image.

After that, you can use the `j`, `r`, `p`, `w`, and `lw` functions to go about as usual in a client:

```
(j "#lisp")
(r "Good morning, #lisp.")
(j "#clasp")
(lw)
(w "#lisp")
(r "Oh boy.")
(p "#lisp" "#clasp")
```

Disconnecting will merely require `(disconnect *core* 'freenode)`. Note that you can also have multiple connections at the same time, just call `connect` again as desired.
