#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.location)

(defparameter *geocode-api* "https://maps.googleapis.com/maps/api/geocode/json")

;; (maiden-accounts:define-fields
;;   (location () "A physical location where the user currently resides."))

(defun geo-information (location &optional key)
  (let* ((data (request-as :json *geocode-api* :get `(("sensor" "false") ("address" ,location) ("key" ,(or key "")))))
         (status (json-v data "status")))
    (cond ((string-equal status "ok")
           (json-v data "results" 0))
          ((string-equal status "zero_results")
           (error "No location called ~s could be found." location))
          ((string-equal status "over_query_limit")
           (error "Exceeded allowed amount of queries against the Google Maps API."))
          (T
           (error "Google Maps failed to perform your request for an unknown reason.")))))

(defun coordinates (location &optional key)
  (let ((data (geo-information location key)))
    (values (list (json-v data "geometry" "location" "lat")
                  (json-v data "geometry" "location" "lng"))
            (json-v data "address_components" 0 "long_name"))))

(defun address (location &optional key)
  (let ((data (geo-information location key)))
    (json-v data "formatted_address")))

(define-consumer location (agent)
  ())

(define-command (location query-address) (c ev &string location)
  :command "address of"
  (maiden-storage:with-storage (c)
    (reply ev "I think the address for ~s is ~a." location (address location (maiden-storage:value :api-key)))))

(define-command (location query-coordinates) (c ev &string location)
  :command "coordinates of"
  (maiden-storage:with-storage (c)
    (multiple-value-bind (coordinates location) (coordinates location)
      (reply ev "~s is located at ~flat ~flng."
             location (first coordinates) (second coordinates) (maiden-storage:value :api-key)))))
