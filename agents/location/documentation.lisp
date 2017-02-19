#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.location)

(docs:define-docs
  (variable *geocode-api*
    "Address of the google maps geocode API.")

  (function geo-information
    "Retrieve geolocation information about the given location.

Errors are signalled in the following cases:
- No results could be found for the location
- You exceeded the maximum number of queries allowed
- Some other failure on behalf of the Google API.

See *GEOCODE-API*")

  (function coordinates
    "Return the coordinates of a location.

Returns two values:
- A list of the latitude and longitude of the location
- The long name of the resolved address of the location

See GEO-INFORMATION")

  (function address
    "Attempt to find the closes address for the location.

See GEO-INFORMATION")
  
  (type location
    "This agent helps with location information such as geocoding using the Google Maps API.")

  (command query-address
    "Try to discover the actual address of an approximate location.")

  (command query-coordinates
    "Attempt to find the latitude and longitude coordinates of the location."))
