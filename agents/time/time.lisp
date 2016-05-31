#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.time)

(defparameter *timezone-api* "https://maps.googleapis.com/maps/api/timezone/json")

;; (maiden-accounts:define-fields
;;   (timezone () "The time zone the account's user is currently in."))

(defun timezone-data (location &optional (time (get-unix-time)))
  (let* ((location (if (listp location) location (maiden-location:coordinates location)))
         (data (request-as :json *timezone-api* :get `(("sensor" "false")
                                                       ("timestamp" ,time)
                                                       ("location" ,(format NIL "~f,~f" (first location) (second location))))))
         (status (json-v data "status")))
    (cond ((string-equal status "ok")
           (list :zone-id (json-v data "timeZoneId")
                 :zone (json-v data "timeZoneName")
                 :dst-offset (json-v data "dstOffset")
                 :offset (json-v data "rawOffset")))
          ((string-equal status "zero_results")
           (values))
          ((string-equal status "over_query_limit")
           (error "Exceeded allowed amount of queries against the Google Maps API."))
          (T
           (error "Google Maps failed to perform your request for an unknown reason.")))))

(defun timezone (location)
  (let ((data (timezone-data location)))
    (values (getf data :zone) (getf data :zone-id))))

(defun time (location)
  (let ((data (timezone-data location)))
    (+ (get-universal-time)
       (getf data :offset)
       (getf data :dst-offset))))

(defun user-location (user)
  (or (data-value :timezone user)
      (data-value :location user)
      (error "I don't know where ~a is located." user)))

(define-consumer time ()
  ())

(define-command (time timezone-location) (c ev location)
  :command "timezone of"
  (let* ((data (timezone-data location))
         (secs (+ (getf data :offset) (getf data :dst-offset))))
    (reply ev "The time zone for ~s is ~a (UTC~@f)"
           (getf data :zone) (/ secs 60))))

(define-command (time time-location) (c ev location)
  :command "time in"
  (reply ev "The time in ~a is ~a." location (format-absolute-time (time location))))

(define-command (time time-user) (c ev user)
  :command "time for"
  (reply ev "The time for ~a is ~a." user (format-absolute-time (time (user-location user)))))

(define-command (time time-between) (c ev from to)
  :command "time between"
  (let* ((data-from (timezone-data from))
         (data-to (timezone-data to))
         (diff (- (+ (getf data-to :offset) (getf data-to :dst-offset))
                  (+ (getf data-from :offset) (getf data-from :dst-offset))))
         (to-time (format-absolute-time (+ (get-universal-time) (getf data-to :offset) (getf data-to :dst-offset)))))
    (if (< (abs diff) 60)
        (reply ev "Both are in the same timezone, which is currently at ~a." to-time)
        (reply ev "The time in ~a is ~a ~a (~a)."
               to (format-relative-time (abs diff)) (if (<= 0 diff) "later" "earlier") to-time))))

(define-command (time time-between-users) (c ev from to)
  :command "time between users"
  (do-issue (core ev) 'time-between :from (user-location from) :to (user-location to)))
