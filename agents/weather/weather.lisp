#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.weather)

(defparameter *weather-api* "https://api.forecast.io/forecast/~a/~f,~f")

(defun weather-data (apikey lat lng)
  (let* ((data (request-as :json (format NIL *weather-api* apikey lat lng)
                           :get '(("units" "si") ("exclude" "hourly,minutely,daily,flags,alerts")))))
    (cond ((consp data)
           (cdr (json-v data "currently")))
          (T
           (error "Forecast.io failed to perform your request for an unknown reason.")))))

(defun location-weather-data (apikey location)
  (multiple-value-bind (loc resolved-location) (maiden-location:coordinates location)
    (cond ((not loc)
           (error "Could not determine any location called ~s." location))
          (T
           (values (weather-data apikey (first loc) (second loc))
                   resolved-location)))))

(defun format-weather-data (data)
  (flet ((d (field) (cdr (assoc field data :test #'equalp))))
    (let ((summary (d "summary"))
          (temperature (round (d "temperature")))
          (apparent (round (d "apparentTemperature")))
          (humidity (round (* 100 (d "humidity"))))
          (wind (round (d "windSpeed")))
          (pressure (round (d "pressure"))))
      (format NIL "~a at ~d°C~:[ (feels like ~d°C)~;~*~], ~d% humidity, ~dkm/h wind, ~dhPa pressure."
              summary temperature (= temperature apparent) apparent humidity wind pressure))))

(define-consumer weather (agent)
  ())

(maiden-commands:define-command (weather set-api-key) (c ev key)
  :command "set weather api key"
  (with-storage (c)
    (setf (value :api-key) key)
    (Reply ev "API key set.")))

(defun get-api-key (c)
  (or (with-storage (c) (value :api-key))
      (error "You must set an API key before you can use this service. See http://forecast.io/ to get a key and finally set it with `set weather api key <key>`.")))

(maiden-commands:define-command (weather weather-location) (c ev location)
  :command "weather in"
  (multiple-value-bind (data resolved-location) (location-weather-data (get-api-key c) location)
    (reply ev "Weather in ~a: ~a" resolved-location (format-weather-data data))))

(maiden-commands:define-command (weather weather-user) (c ev user)
  :command "weather for"
  (let ((location (maiden-location:user-location user)))
    (multiple-value-bind (data resolved-location) (location-weather-data (get-api-key c) location)
      (reply ev "Weather for ~a in ~a: ~a" (name account) resolved-location (format-weather-data data)))))
