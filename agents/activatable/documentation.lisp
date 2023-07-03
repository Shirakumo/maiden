(in-package #:org.shirakumo.maiden.agents.activatable)

(docs:define-docs
  (function short-package-name
    "Returns the shortest of all the package's nicknames.")

  (function trim-package-prefix
    "Removes maiden package prefixes.")

  (function normalize-module-name
    "Attempts to normalise the module name.

This:
1. Takes the symbol-name for symbols, the short-package-
   name for packages, or a straight-up string.
2. Downcases it.
3. Trims the package prefix.

See SHORT-PACKAGE-NAME
See TRIM-PACKAGE-PREFIX")

  (function normalize-ident
    "Normalise the thing into an ident.

THING can be one of
- CHANNEL-EVENT -- Calls itself again using a cons of
                   client name and channel name.
- CLIENT-EVENT  -- Calls itself again using a cons of
                   client name and NIL.
- CONS          -- Returns a fresh cons where both parts
                   are a downcased string or NIL. Either
                   can be an entity, a string-designator
                   or NIL.")

  (function activate
    "Activate the modules on the given client ident.

See NORMALIZE-IDENT
See NORMALIZE-MODULE-NAME")

  (function deactivate
    "Deactivate the modules on the given client ident.

See NORMALIZE-IDENT
See NORMALIZE-MODULE-NAME")

  (function active-p
    "Returns whether the given module is active on the given ident.

See NORMALIZE-IDENT
See NORMALIZE-MODULE-NAME")

  (function list-active
    "Lists all active modules for the given ident.

See NORMALIZE-IDENT")

  (type activatable-handler
    "Mixin class for a handler that can be de/activated on a client/channel basis.

You must pass the name of the module the handler should
belong to as an initarg.

See MODULE
See ACTIVATE
See DEACTIVATE
See ACTIVE-P")
  
  (type activatable
    "This agent manages the activation of other agents that might want to be blocked on certain channels to avoid unwanted responses.")
  
  (command activate
    "Activate modules on the current channel. By default modules are deactivated.")

  (command deactivate
    "Deactivate modules on the current channel."))
