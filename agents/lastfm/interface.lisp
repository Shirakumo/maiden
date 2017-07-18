#|
 This file is a part of Maiden
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.lastfm)

(defvar *lastfm-api* "http://ws.audioscrobbler.com/2.0/")

(define-consumer lastfm (agent)
  ((streams :initform NIL :accessor streams)))

(defun lastfm-request (function &rest args)
  (unless (maiden-storage:value :api-key)
    (error "Please configure a last.fm API key."))
  (request-as :json *lastfm-api*
              :get (list* (list "method" function)
                          (list "api_key" (maiden-storage:value :api-key))
                          (list "format" "json")
                          (loop for (key val) on args by #'cddr
                                when val
                                collect (list (string-downcase key)
                                              (princ-to-string val))))))

(defun user/get-recent-tracks (user &key (limit 50)
                                         (page 1)
                                         from
                                         to
                                         extended)
  (lastfm-request "user.getrecenttracks"
                  :user user
                  :limit limit
                  :page page
                  :from from
                  :to to
                  :extended (if extended 1 0)))

(define-command (lastfm set-api-key) (c ev api-key)
  :command "set last.fm api key"
  :advice (not public)
  (setf (maiden-storage:value :api-key) api-key)
  (reply ev "API key updated."))

(define-command (lastfm recently-played-for) (c ev user)
  :command "list recent tracks for"
  (let ((tracks (json-v (user/get-recent-tracks user :limit 5) "recenttracks" "track")))
    (if tracks
        (let ((tracks (loop for track in tracks
                            collect (list (format-time (unix-to-universal (parse-integer (json-v track "date" "uts"))))
                                          (json-v track "name")
                                          (json-v track "artist" "#text")))))
          (reply ev "~{~{~a: ~a by ~a~}~^~%~}" tracks))
        (reply ev "~a has no last.fm profile, or nothing scrobbled so far."
               user))))

(define-command (lastfm recently-played) (c ev)
  :command "list recent tracks"
  (issue (make-instance 'recently-played-for :user (name (user ev)) :dispatch-event ev)
         (core ev)))

(define-command (lastfm currently-playing-for) (c ev user)
  :command "currently playing for"
  (let ((track (first (json-v (user/get-recent-tracks user :limit 1) "recenttracks" "track"))))
    (if (and track (json-v track "@attr") (equal (json-v track "@attr" "nowplaying") "true"))
        (reply ev "~@(~a~) is now listening to ~a by ~a"
               user (json-v track "name") (json-v track "artist" "#text"))
        (reply ev "~@(~a~) does not seem to be listening to anything we know about right now."
               user))))

(define-command (lastfm currently-playing) (c ev)
  :command "currently playing"
  (issue (make-instance 'currently-playing-for :user (name (user ev)) :dispatch-event ev)
         (core ev)))

(define-command (lastfm stream-scrobbles-for) (c ev user)
  :command "stream scrobbles for"
  :advice (not public)
  (let ((data (list (channel ev) (name (user ev)))))
    (push (list* (bt:make-thread (lambda () (stream-scrobbles data))) data)
          (streams c))
    (reply ev "Scrobbles for ~a are now being streamed here." user)))

(defun stream-scrobbles (data)
  (with-simple-restart (abort "Exit scrobble streaming.")
    (destructuring-bind (channel user) data
      (loop with mbid = NIL
            for track = (first (json-v (user/get-recent-tracks user :limit 1) "recenttracks" "track"))
            while (car data)
            do (when (and track (json-v track "@attr") (equal (json-v track "@attr" "nowplaying") "true"))
                 (unless (equal mbid (json-v track "mbid"))
                   (reply channel "~@(~a~) is now listening to ~a by ~a"
                          user (json-v track "name") (json-v track "artist" "#text"))
                   (setf mbid (json-v track "mbid"))))
               (sleep 5)))))

(define-command (lastfm stop-scrobbles-stream) (c ev &optional user)
  :command "stop scrobbles stream"
  :advice (not public)
  (setf (streams c)
        (loop for data in (streams c)
              for (thread channel username) = data
              for match = (and (eql channel (channel ev))
                               (or (not user) (string-equal user username)))
              when match do (setf (car data) NIL)
              unless match collect data))
  (reply ev "Scrobbles stream stopped~@[ for ~a~]" user))
