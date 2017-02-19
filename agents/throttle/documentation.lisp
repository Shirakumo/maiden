#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.throttle)

(docs:define-docs
  (type throttle
    "Provides throttling to prevent spamming the bot with commands.")

  (function attempts
    "Accessor to the number of attempts that can be made within a time-frame.

See THROTTLE
See TIME-FRAME")

  (function time-frame
    "Accessor to the number of seconds that specify a time-frame for throttling.

See THROTTLE
See ATTEMPTS")

  (function cooldown-function
    "Accessor to the symbol that designates a cooldown function used to calculate the number of seconds until the user is freed.

Must be one of the following:
- :CONSTANT     The cooldown is as much as the cooldown step.
- :LINEAR       The cooldown is increased linearly for each
                additional attempt within the cooldown.
- :EXPONENTIAL  The cooldown is increased exponentially for
                each additional attempt within the cooldown.

See THROTTLE
See COOLDOWN-STEP
See COOLDOWN-MAX")

  (function cooldown-step
    "Accessor to the step used to increase the cooldown according to the cooldown function.

See THROTTLE
See COOLDOWN-FUNCTION
See COOLDOWN-MAX")

  (function cooldown-max
    "Accessor to the maximum amount of seconds that the user can be held in cooldown.

See THROTTLE
See COOLDOWN-FUNCTION
See COOLDOWN-STEP")

  (function records
    "Accessor to the hash table associating user names to throttling records.

See THROTTLE
See RECORD")

  (function record
    "Access the throttling record for the given user.

See RECORDS
See THROTTLE")

  (type record
    "Container class to hold information about the throttling of a user.

See ATTEMPTS
See TIMESTAMP
See TIMEOUT")

  (function attempts
    "The current number of attempts that have been made by the user within the time-frame.

See RECORD")

  (function timestamp
    "The universal-time timestamp that specifies the time at which the timeframe began or the timeout began.

See RECORD")

  (function timeout
    "The number of seconds for which the user has been timed out. 

This is relative to TIMESTAMP.

See RECORD")

  (function clear-tax
    "Clear the tax on the user's record.

This effectively makes them unthrottled.

See RECORD")

  (function tax
    "This function taxes the user for one attempt.

If too many attempts have been made too quickly, the user is
timed out. If further attempts are made while timed out, the
timeout is increased gradually according to the throttle's
timeout function and step. Once the user has passed their
timeout, the tax is cleared again.

See THROTTLE
See RECORD")

  (function ensure-cooldown-function
    "Ensure that the given thing designates a valid cooldown function.

Returns the cooldown function symbol.

If it is not a valid designator, an error is signalled.

See COOLDOWN-FUNCTION")

  (command view-config
    "Shows the current configuration that the throttling works with.")

  (command set-config
    "Update the configuration values for the throttling behaviour.")

  (command clear-tax
    "Clear the throttling tax from a user."))
