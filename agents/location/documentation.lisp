#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.location)

(docs:define-docs
  (type location
    "This agent helps with location information such as geocoding using the Google Maps API.")

  (command query-address
    "Try to discover the actual address of an approximate location.")

  (command query-coordinates
    "Attempt to find the latitude and longitude coordinates of the location."))
