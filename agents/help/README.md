## About
This is an agent that provides a generic help and introspection facility, with the hopes that it will make it easier to discover the available commands and features that the system offers.

## How To
The `help` command should do pretty much everything. The following terms are handled specially:

* `uptime`
* `about`

Otherwise the term is first attempted to be interpreted as a command name, otherwise as the name of a consumer, and finally as a command search term.

    ::help about
    ::help list
    ::help about command
