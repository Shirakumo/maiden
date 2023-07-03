(in-package #:org.shirakumo.maiden.agents.weather)

(docs:define-docs
  (variable *weather-api*
    "Address to the DarkSky weather API endpoint.")

  (function weather-data
    "Returns the JSON object data from the weather API.

Units are in SI, and data is only returned for the
given time-frame, which can be one of the following:
 :CURRENTLY :MINUTELY :HOURLY :DAILY :FLAGS :ALERTS

See the DarkSky API documentation for more information.
See MAIDEN-API-ACCESS:REQUEST-AS")

  (function location-weather-data
    "Returns the JSON object data from the weather API for the given location.

Returns the resolved location as its second value.

See WEATHER-DATA
See MAIDEN-LOCATION:COORDINATES")

  (function format-weather-data
    "Format the given JSON object representing current weather data in a human-readable way.")

  (function day-of-week
    "Returns a string for the day of the week of the given unix-time.")

  (function format-daily-forecast
    "Format the given JSON object representing a daily forecast in a human-readable way.")
  
  (type weather
    "This agent provides utilities to retrieve weather information about a location.")

  (command weather-dwim
    "Retrieve weather information about yourself, a user, or a location.")

  (command weather-location
    "Retrieve weather information about a specific location.")

  (command forecast-location
    "Retrieve a weather forecast for the next week for a specified location.")

  (command weather-user
    "Retrieve weather information for a user. Note that this only works if the location of the user is known.")

  (command forecast-user
    "Retrieve a weather forecast for the next week for a user. Note that this only works if the location of the user is known."))
