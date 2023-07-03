(in-package #:org.shirakumo.maiden.agents.time)

(docs:define-docs
  (variable *timezone-api*
    "The address for the Google maps timezone resolution API.")

  (function timezone-data
    "Returns the JSON object returned by the API for the given location.

See *TIMEZONE-API*")

  (function timezone
    "Returns information about the timezone active at the location.

Returns two values-- the name of the zone and its ID.

See TIMEZONE")

  (function time
    "Returns the current, local time in universal-time at the given location.

See TIMEZONE-DATA")

  (function user-location
    "Attempt to find a suitable location for the user's timezone.

Looks at the user's :TIMEZONE and :LOCATION data-values.

See MAIDEN:DATA-VALUE")
  
  (type time
    "This agent provides all sorts of time, date, and timezone utilities.")

  (command timezone-location
    "Shows the time zone of a location.")

  (command time-dwim
    "With no argument, the time for the requester is shown, if their location is known. Otherwise, the time for the given user or location is shown, if possible.")

  (command time-location
    "Show the time for a location.")

  (command time-user
    "Show the time for a user. Note that this only works if the location of the user is known.")

  (command time-between
    "Show the time difference between two locations.")

  (command time-between-users
    "Show the time difference between two users. Note that this only works if the location of the user is known."))
