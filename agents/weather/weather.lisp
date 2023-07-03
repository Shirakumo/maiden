(in-package #:org.shirakumo.maiden.agents.weather)

(defparameter *weather-api* "https://api.openweathermap.org/data/2.5/onecall")

(defun weather-data (apikey lat lng &key (time-frame :current))
  (let* ((data (request-as :json *weather-api*
                           :get `(("units" "metric")
                                  ("lat" ,(format NIL "~f" lat))
                                  ("lon" ,(format NIL "~f" lng))
                                  ("appid" ,apikey)
                                  ("exclude" ,(format NIL "~{~(~a~)~^,~}" (remove time-frame '(:current :minutely :hourly :daily :alerts))))))))
    (cond ((consp data)
           (json-v data (string-downcase time-frame)))
          (T
           (error "API failed to perform your request for an unknown reason.")))))

(defun location-weather-data (apikey location &key (time-frame :current))
  (multiple-value-bind (loc resolved-location) (maiden-location:coordinates location)
    (cond ((not loc)
           (error "Could not determine any location called ~s." location))
          (T
           (values (weather-data apikey (first loc) (second loc) :time-frame time-frame)
                   resolved-location)))))

(defun format-weather-data (data)
  (flet ((d (field &optional (data data)) (cdr (assoc field (cdr data) :test #'equalp))))
    (let ((summary (d "description" (first (d "weather"))))
          (temperature (round (d "temp")))
          (apparent (round (d "feels_like")))
          (humidity (round (d "humidity")))
          (wind (round (d "wind_speed")))
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
  (flet ((d (field &optional (data data)) (cdr (assoc field data :test #'equalp))))
    (format NIL "~{~{~a ~d-~d°C~}~^, ~}"
            (loop for dat in data
                  collect (list (day-of-week (d "dt" dat))
                                (round (d "min" (d "temp" dat)))
                                (round (d "max" (d "temp" dat))))))))

(define-consumer weather (agent)
  ())

(define-command (weather set-api-key) (c ev key)
  :command "set weather api key"
  :advice (not public)
  (with-storage (c)
    (setf (value :api-key) key)
    (Reply ev "API key set.")))

(defun get-api-key (c)
  (or (with-storage (c) (value :api-key))
      (error "You must set an API key before you can use this service. See http://forecast.io/ to get a key and finally set it with `set weather api key <key>`.")))

(define-command (weather weather-dwim) (c ev &optional signifier)
  :command "weather"
  (cond ((not signifier)
         (relay ev 'weather-user :user (name (user ev))))
        ((find-user signifier (client ev))
         (relay ev 'weather-user :user signifier))
        (T
         (relay ev 'weather-location :location signifier))))

(define-command (weather weather-location) (c ev &string location)
  :command "weather in"
  (multiple-value-bind (data resolved-location) (location-weather-data (get-api-key c) location)
    (reply ev "Weather in ~a: ~a" resolved-location (format-weather-data data))))

(define-command (weather forecast-location) (c ev &string location)
  :command "forecast in"
  (multiple-value-bind (data resolved-location) (location-weather-data (get-api-key c) location :time-frame :daily)
    (reply ev "Forecast in ~a: ~a" resolved-location (format-daily-forecast data))))

(define-command (weather weather-user) (c ev user)
  :command "weather for"
  (multiple-value-bind (data resolved-location)
      (location-weather-data (get-api-key c) (data-value :location user))
    (reply ev "Weather for ~a in ~a: ~a" user resolved-location (format-weather-data data))))

(define-command (weather forecast-user) (c ev user)
  :command "forecast for"
  (multiple-value-bind (data resolved-location)
      (location-weather-data (get-api-key c) (data-value :location user) :time-frame :daily)
    (reply ev "Forecast for ~a in ~a: ~a" user resolved-location (format-daily-forecast data))))
