#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:maiden-user)
(defpackage #:maiden-weather
  (:nicknames #:org.shirakumo.maiden.agents.weather)
  (:use #:cl #:maiden #:maiden-api-access #:maiden-storage #:maiden-accounts)
  ;; weather.lisp
  (:export
   #:weather-data
   #:location-coordinates
   #:location-weather-data
   #:format-weather-data
   #:format-forecast-data
   #:weather)
  (:shadowing-import-from #:cl #:identity))
