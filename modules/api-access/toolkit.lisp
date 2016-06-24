#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.modules.api-access)

(defun construct-url (url get)
  (format NIL "~a?~{~2{~a=~a~}~^&~}"
          url (loop for (key val) in get
                    collect (list (drakma:url-encode (princ-to-string key) :utf8)
                                  (drakma:url-encode (princ-to-string val) :utf8)))))

(defun request (url &key get post (method :get) (external-format :utf8) other-args)
  (let ((drakma:*text-content-types* (list* '("application" . "xml")
                                            '("application" . "xhtml+xml")
                                            '("application" . "json")
                                            '("application" . "x-sexp")
                                            '("application" . "x-lisp")
                                            drakma:*text-content-types*))
        (url (construct-url url get)))
    (v:debug :maiden-api-access "Requesting ~s by ~a~@[ with params ~s~]"
             url method post)
    (apply #'drakma:http-request
           url
           :parameters (loop for (key val) in post collect (cons key val))
           :method method
           :external-format-in external-format
           :external-format-out external-format
           :decode-content T
           :preserve-uri T
           other-args)))

(defun parse-to (type input)
  (ecase type
    (:string input)
    (:json (jsown:parse input))
    ((:html :xml) (plump:parse input))
    (:sexp (read-from-string input))))

(defun request-as (type url &rest args &key get post external-format other-args)
  (declare (ignore get post external-format other-args))
  (let ((values (multiple-value-list (apply #'request url args))))
    (values-list (list* (parse-to type (first values)) (rest values)))))

(defun json-v (json &rest path)
  (if path
      (cond ((and (consp json) (eql (car json) :obj))
             (apply #'json-v (cdr (assoc (first path) (cdr json) :test #'string=)) (rest path)))
            ((consp json)
             (apply #'json-v (nth (first path) json) (rest path)))
            (T
             (error "Don't know how to traverse ~s by ~a" json path)))
      json))
