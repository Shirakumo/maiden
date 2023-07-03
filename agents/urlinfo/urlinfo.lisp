(in-package #:org.shirakumo.maiden.agents.urlinfo)

(defun fetch (url)
  (multiple-value-bind (stream status headers url)
      (let ((drakma:*text-content-types* '(("text")
                                           ("application" . "xhtml+xml"))))
        (drakma:http-request url :want-stream T :connection-timeout 2))
    (unless (= status 200)
      (error "Failed to fetch URL, received status code ~a." status))
    (values (puri:render-uri url NIL) (cdr (assoc :content-type headers)) stream)))

(defun process-until (string function stream &optional limit)
  (loop with index = 0
        for i from 0
        for c = (read-char stream)
        do (cond ;; We might have a match here...
                 ((char-equal c (aref string index))
                  (incf index)
                  (when (= index (length string))
                    (return)))
                 ;; We mismatched, replay what we squandered.
                 ((< 0 index)
                  (dotimes (i index)
                    (funcall function (aref string i)))
                  (funcall function c)
                  (setf index 0))
                 ((and limit (< limit i))
                  (return))
                 ;; Nothing unusual, just process.
                 (T
                  (funcall function C)))))

(defun maybe-title (type stream)
  (when (search "html" type :test #'char-equal)
    (process-until "<title>" (lambda (c) c) stream)
    (plump-dom:decode-entities
     (with-output-to-string (s)
       (process-until "</title>" (lambda (c) (write-char c s)) stream 100)))))

(defun nicer-title (title)
  (cond ((not title) NIL)
        ((string= "" title) "<empty>")
        (T (flet ((r (a b m) (cl-ppcre:regex-replace-all a m b)))
             (string-trim " " (r "  +" " " (r "[\\n\\r]+" " " title)))))))

(defun short-url (url &optional (url-cutoff 50))
  (setf url (cl-ppcre:regex-replace "^[^:]*://" url ""))
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
      (T (or (cl-ppcre:register-groups-bind (type NIL format) ("^([\\w]+)/(x-)?(.+?)(;.*)?$" type)
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

(define-command (urlinfo test) (c ev &string url)
  :command "test url"
  (reply ev "~a" (urlinfo url)))

(define-handler (urlinfo auto-respond (and message-event passive-event)) (c ev message)
  :class activatable-handler
  :module #.*package*
  (unless (matches (username (client ev)) (user ev))
    (dolist (url (find-urls-in-string message))
      (reply ev "~a" (urlinfo url)))))
