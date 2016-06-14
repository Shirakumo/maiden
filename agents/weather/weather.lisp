#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.weather)

(defparameter *weather-api* "https://api.forecast.io/forecast/~a/~f,~f")

(defun weather-data (apikey lat lng &key (time-frame :currently))
  (let* ((data (request-as :json (format NIL *weather-api* apikey lat lng)
                           :get `(("units" "si") ("exclude" ,(format NIL "~{~(~a~)~^,~}" (remove time-frame '(:currently :minutely :hourly :daily :flags :alerts))))))))
    (cond ((consp data)
           (cdr (json-v data (string-downcase time-frame))))
          (T
           (error "Forecast.io failed to perform your request for an unknown reason.")))))

(defun location-weather-data (apikey location &key (time-frame :currently))
  (multiple-value-bind (loc resolved-location) (maiden-location:coordinates location)
    (cond ((not loc)
           (error "Could not determine any location called ~s." location))
          (T
           (values (weather-data apikey (first loc) (second loc) :time-frame time-frame)
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

(defun day-of-week (unix-time)
  (case (local-time:timestamp-day-of-week
         (local-time:unix-to-timestamp unix-time))
    (0 "Sunday")
    (1 "Monday")
    (2 "Tuesday")
    (3 "Wednesday")
    (4 "Thursday")
    (5 "Friday")
    (6 "Saturday")))

(defun format-daily-forecast (data)
  (flet ((d (field) (cdr (assoc field data :test #'equalp))))
    (format NIL "~{~{~a ~d-~d°C~}~^, ~}"
            (loop for dat in (d "data")
                  collect (list (day-of-week (json-v dat "time"))
                                (round (json-v dat "temperatureMin"))
                                (round (json-v dat "temperatureMax")))))))

(define-consumer weather (agent)
  ())

(maiden-commands:define-command (weather set-api-key) (c ev key)
  :command "set weather api key"
  :advice (not public)
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

(maiden-commands:define-command (weather forecast-location) (c ev location)
  :command "forecast in"
  (multiple-value-bind (data resolved-location) (location-weather-data (get-api-key c) location :time-frame :daily)
    (reply ev "Forecast in ~a: ~a" resolved-location (format-daily-forecast data))))

(maiden-commands:define-command (weather weather-user) (c ev user)
  :command "weather for"
  (multiple-value-bind (data resolved-location)
      (location-weather-data (get-api-key c) (data-value :location user))
    (reply ev "Weather for ~a in ~a: ~a" user resolved-location (format-weather-data data))))

(maiden-commands:define-command (weather weather-user) (c ev user)
  :command "forecast for"
  (multiple-value-bind (data resolved-location)
      (location-weather-data (get-api-key c) (data-value :location user) :time-frame :daily)
    (reply ev "Forecast for ~a in ~a: ~a" user resolved-location (format-daily-forecast data))))
