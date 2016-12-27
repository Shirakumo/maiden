#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.weather)

(docs:define-docs
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
