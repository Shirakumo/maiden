## About
This agent provides a facility to allow blocking and excluding channels, clients, users, or other parts from event or command processing. This is useful in case of spammers, or if some functionality does not provide an explicit kill switch, but must be turned off for a specific environment.

## How To
Blocking happens over rules, which are a composition of clauses. The syntax is as most easily explained through some examples:

```
(channel "foo")
(or (user "dios") (user "gott"))
(and (prefix "::") (user "no-commands-user"))
```

Logical combinations can be made via `and`, `or`, and `not`. Tests can be made with `channel`, `client`, `user`, `regex`, and `prefix`. You can also define additional clauses for these rules using `define-clause`.

Rules can also be defined and managed over the interface.
