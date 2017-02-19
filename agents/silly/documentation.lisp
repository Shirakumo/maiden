#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.silly)

(docs:define-docs
  (variable *silly-functions*
    "This holds a map from names to silly functions.

A silly function should take two arguments, the first
being the name of the sender who sent the message, and
the second being a message text string.

The silly function should, if it wants to handle the
message, return a string which will be displayed to the
user.

See SILLY-FUNCTION
See REMOVE-SILLY-FUNCTION
See DISPATCH-SILLY
See DEFINE-SILLY")

  (function silly-function
    "Accessor to the silly function of the given name.

See *SILLY-FUNCTIONS*
See REMOVE-SILLY-FUNCTION")

  (function remove-silly-function
    "Removes the silly function of the given name.

See SILLY-FUNCTION
See *SILLY-FUNCTIONS*")

  (function dispatch-silly
    "Dispatch the sender and message to the silly functions and gather all resulting messages into a list.

See *SILLY-FUNCTIONS*")

  (function define-silly
    "Define a new silly function.

See *SILLY-FUNCTIONS*
See SILLY-FUNCTION")

  (function define-simple-silly
    "Define a simple silly function.

The message is matched against REGEX and the regex'
groups are destructured into the ARGS variables.
The body should then be a format string followed by
the format arguments.

See DEFINE-SILLY
See CL-PPCRE:REGISTER-GROUPS-BIND
See CL:FORMAT")

  (function cut-to-first-vowel
    "Cut the word up until the first vowel, if possible. If not, just return the whole word.")
  
  (type silly
    "This module implements silly commands and responses.")

  (command eight
    "8")

  (command jerkcity
    "Respond with a randomly selected jerkcity comic strip.")

  (command roll
    "Roll some dice.   Note that this is not provided with the intention of providing gambling means.")

  (variable *fortunes*
    "Holds a list of \"fortune\" messages.

Should be populated by the fortunes.txt file.

See FORTUNE")

  (function fortune
    "Pick a fortune message for a user of the given name at the given time.

The fortune to pick is decided on a hash of the time and name.
It is thus deterministic, but highly volatile. In order to make
it deterministic for a whole day, the timestamp is reduced to
a counter for days.

See *FORTUNES*")

  (command fortune
    "Display the fortune of today for you or for a user. It changes daily!"))
