(in-package #:org.shirakumo.maiden.agents.help)

(docs:define-docs
  (type help
    "This agent provides generic help commands that should provide information for the user and help in discovering the system interactively.")

  (function find-consumer
    "Attempt to find a consumer on the core that matches the name somehow.")

  (command about
    "This is the generic interface for the help system. Depending on what it finds for the term, it dispatches to other help commands.")

  (command about-self
    "This displays a blurb that describes some information about the bot and its current status.")

  (command about-uptime
    "This displays information about the running time of the bot.")

  (command about-command
    "This displays available information about a command that the bot supports.")

  (command list-consumers
    "List all the consumers (systems) that are running on the bot.")

  (command about-consumer
    "This displays documentation information about a consumer (system) that's running on the bot.")

  (command about-term
    "This searches for a command that approximately matches the given term. It displays a list of up to ten matches, in decreasing order of similarity."))
