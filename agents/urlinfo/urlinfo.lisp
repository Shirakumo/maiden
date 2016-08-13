#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.urlinfo)

(defun fetch (url)
  (multiple-value-bind (stream status headers url)
      (drakma:http-request url :want-stream T)
    (unless (= status 200)
      (error "Failed to fetch URL, received status code ~a." status))
    (values (puri:render-uri url NIL) (cdr (assoc :content-type headers)) stream)))

(defun process-until (string function stream)
  (loop with index = 0
        for c = (read-char stream)
        do (cond ;; We might have a match here...
                 ((char= c (aref string index))
                  (incf index)
                  (when (= index (length string))
                    (return)))
                 ;; We mismatched, replay what we squandered.
                 ((< 0 index)
                  (dotimes (i index)
                    (funcall function (aref string i)))
                  (funcall function c)
                  (setf index 0))
                 ;; Nothing unusual, just process.
                 (T
                  (funcall function C)))))

(defun maybe-title (type stream)
  (when (search "html" type :test #'char-equal)
    (process-until "<title>" (lambda (c) c) stream)
    (with-output-to-string (s)
      (process-until "</title>" (lambda (c) (write-char c s)) stream))))

(defun nicer-title (title)
  (cond ((not title) NIL)
        ((string= "" title) "<empty>")
        (T (flet ((r (a b m) (cl-ppcre:regex-replace-all a m b)))
             (string-trim " " (r "  +" " " (r "[\\n\\r]+" " " title)))))))

(defun short-url (url &optional (url-cutoff 50))
  (if (< (length url) url-cutoff)
      url
      (format NIL "~a..." (subseq url 0 (- url-cutoff 3)))))

(defun nicer-content-type (type)
  (macrolet ((with-content-type-case (thing &body cases)
               `(cond ,@(loop for (string . body) in cases
                              if (eql string T) collect `(T ,@body)
                              else collect `((find ,thing ',(enlist string)
                                                   :test (lambda (a b) (search b a)))
                                             ,@body)))))
    (with-content-type-case type
      (("text/html") "Website (HTML)")
      (("application/xhtml+xml") "Website (XHTML)")
      (("image/jpeg" "image/jpg") "Image (JPEG)")
      (("image/png" "image/mpng") "Image (PNG)")
      (("image/svg+xml" "image/svg") "Image (SVG)")
      (("audio/mp3" "audio/mpeg") "Audio (MP3)")
      (T (or (cl-ppcre:register-groups-bind (type NIL format) ("([\\w]+)/(x-)?(.+)" type)
               (format NIL "~:(~a~) (~:@(~a~))" type format))
             type)))))

(defun urlinfo (url &optional (url-cutoff 30))
  (when (string= "http" url :end2 4)
    (multiple-value-bind (url type stream) (fetch url)
      (unwind-protect
           (let ((title (maybe-title type stream)))
             (format NIL "~va ~a~@[, Title: ~a~]"
                     url-cutoff (short-url url url-cutoff) (nicer-content-type type) (nicer-title title)))
        (close stream)))))

(defun find-urls-in-string (string)
  (cl-ppcre:all-matches-as-strings "[a-zA-Z-]+://[\\w\\d\\-.]+\\.\\w{2,}[\\w\\d\\-%+?=&@#.:;/]*" string))

(define-consumer urlinfo (agent)
  ())

(define-command (urlinfo test) (c ev url)
  :command "test url"
  (reply ev "~a" (urlinfo url)))

(define-handler (urlinfo auto-respond message-event) (c ev message)
  :class activatable-handler
  (dolist (url (find-urls-in-string message))
    (reply ev "~a" (urlinfo url))))
