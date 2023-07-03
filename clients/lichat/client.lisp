(in-package #:org.shirakumo.maiden.clients.lichat)

(defclass lichat-user (simple-user)
  ())

(defun find-conversation-channel (user)
  (loop for channel in (channels user)
        when (and (anonymous-p channel)
                  (every (lambda (u)
                           (or (matches u user)
                               (matches u (username (client user)))))
                         (users channel)))
        do (return channel)))

(defun make-anonymous-channel (client &rest users)
  (let ((message (make-instance 'lichat-cmd:create :channel NIL :from (username client))))
    (with-awaiting (client lichat-rpl:join) (ev channel)
        (send message client)
      :filter `(= id ,(slot-value message 'id))
      :timeout 5
      (dolist (user users)
        (lichat-cmd:pull client user channel))
      channel)))

(defmethod reply ((user lichat-user) message &rest args)
  (let ((channel (or (find-conversation-channel user)
                     (make-anonymous-channel (client user) user))))
    (lichat-cmd:message (client user) channel (apply #'format NIL message args))))

(defclass lichat-channel (simple-channel)
  ())

(defmethod anonymous-p ((channel lichat-channel))
  (char= #\@ (char (name channel) 0)))

(defmethod reply ((channel lichat-channel) message &rest args)
  (lichat-cmd:message (client channel) channel (apply #'format NIL message args)))

(define-consumer lichat-client (text-tcp-client reconnecting-client timeout-client simple-user-channel-client)
  ((servername :initform NIL :accessor servername)
   (username :initarg :username :accessor username)
   (password :initarg :password :accessor password)
   (bridge :initarg :bridge :accessor bridge))
  (:default-initargs
   :username "Maiden"
   :password NIL
   :port 1111
   :bridge ()))

(defmethod initialize-instance :after ((client lichat-client) &key channels)
  (dolist (channel channels)
    (setf (find-channel channel client)
          (make-instance 'lichat-channel :client client :name channel)))
  (unless (name client)
    (setf (name client) (host client))))

(defmethod initiate-connection ((client lichat-client))
  ;; Open socket
  (call-next-method)
  ;; Handle connection sequence
  (send (make-instance 'lichat-cmd:connect
                       :client client
                       :password (password client)
                       :from (username client))
        client)
  (unless (nth-value 1 (usocket:wait-for-input (socket client) :timeout 10))
    (usocket:socket-close (socket client))
    (error "Failed to connect: timed out."))
  (let ((ev (receive client)))
    (unless (typep ev 'lichat-rpl:connect)
      (error "Failed to connect, received ~a instead of CONNECT." ev))
    (process ev client))
  ;; After methods will take care of setting up read thread.
  )

(defmethod close-connection :before ((client lichat-client))
  (when (client-connected-p client)
    ;; Can't use dispatch mechanism here since the connection will be
    ;; gone once our handler picks the event up for sending.
    (ignore-errors
     (send (make-instance 'lichat-cmd:disconnect :from (username client)) client))))

(defmethod close-connection :after ((client lichat-client))
  (clrhash (user-map client))
  (loop for channel being the hash-values of (channel-map client)
        do (clrhash (user-map channel))))

(defmethod handle-connection :around ((client lichat-client))
  (with-simple-restart (abort "Exit the connection handling.")
    (handler-bind ((lichat-protocol:wire-condition
                     (lambda (err)
                       (v:error :maiden.client.lichat.connection "Parse error: ~a" err)
                       (loop with stream = (socket-stream client)
                             for char = (read-char stream NIL)
                             until (or (not char) (char= #\Nul char)))
                       (invoke-restart 'continue))))
      (call-next-method))))

(defmethod process ((update update) (client lichat-client))
  (dolist (core (cores client))
    (issue update core)))

(defmethod send ((list list) (client lichat-client))
  (dolist (item list list)
    (send item client)))

(defmethod send ((object update) (client lichat-client))
  (let ((stream (socket-stream client)))
    (print-event object stream)
    (finish-output stream)))

(defmethod receive ((client lichat-client))
  (let ((update (parse-event client (socket-stream client))))
    (v:trace :maiden.client.lichat.connection "Received update: ~a" update)
    update))

(defmethod handle-connection-idle ((client lichat-client))
  (send (make-instance 'lichat-cmd:ping :from (username client) :client client) client))

(defmethod authenticate ((user lichat-user) (client lichat-client))
  (let ((message (make-instance 'lichat-rpl:user-info
                                :client client
                                :from (username client)
                                :target (name user))))
    (with-awaiting (client lichat-rpl:user-info) (ev registered)
        (send message client)
      :filter `(equal id ,(id message))
      :timeout 2
      registered)))

;; FIXME: Currently we are very "trusting" in that we hope the server won't
;;        deliver any malicious or bogus events from channels or users we
;;        cannot know about. Such messages would confuse this system and
;;        potentially lead to a memory exhaustion.
(defmethod ensure-user ((name string) (client lichat-client))
  (or (find-user name client)
      (setf (find-user name client)
            (make-instance 'lichat-user :name name :client client))))

(defmethod ensure-channel ((name string) (client lichat-client))
  (or (find-channel name client)
      (setf (find-channel name client)
            (make-instance 'lichat-channel :name name :client client))))

(define-handler (lichat-client handle-init lichat-rpl:connect) (client ev)
  :match-consumer 'client
  (setf (servername client) (name (slot-value ev 'user)))
  (loop for channel being the hash-keys of (channel-map client)
        do (lichat-cmd:join client channel)))

(define-handler (lichat-client send lichat-cmd:update) (client ev)
  :match-consumer 'client
  (send ev client))

(define-handler (lichat-client pong lichat-rpl:ping) (client ev)
  :match-consumer 'client
  (lichat-cmd:pong client))

(define-handler (lichat-client reconnect lichat-rpl:connection-unstable) (client ev)
  :match-consumer 'client
  #++(initiate-connection (close-connection client)))

(define-handler (lichat-client track-join lichat-rpl:join) (client ev channel user)
  :match-consumer 'client
  (cond ((matches user (username client))
         (setf (gethash (name channel) (channel-map client)) channel))
        (T
         (setf (find-channel (name channel) user) channel)
         (setf (find-user (name user) channel) user))))

(define-handler (lichat-client track-leave lichat-rpl:leave) (client ev channel user)
  :match-consumer 'client
  (cond ((matches user (username client))
         (remove-channel channel client))
        (T
         (remove-user (name user) channel)))
  ;; Prune empty
  (loop for user being the hash-values of (user-map client)
        do (when (= 0 (hash-table-count (channel-map client)))
             (remove-user user client)))
  (loop for channel being the hash-values of (channel-map client)
        do (when (= 0 (hash-table-count (user-map client)))
             (remove-user user client))))

(define-handler (lichat-client bridge (and message-event channel-event passive-event)) (client ev message user)
  (when (string/= "" message)
    (if (eq client (client ev))
        (let ((target (loop for (client channel target) in (bridge client)
                            when (equalp target (name (channel ev)))
                            return (list client channel))))
          (when (and target (null (bridge ev)))
            (let* ((target-client (consumer (first target) (first (cores client))))
                   (channel (when target-client (find-channel (second target) target-client))))
              (when channel
                (reply channel "<~a> ~a" (name user) message)))))
        (let ((target (loop for (client channel target) in (bridge client)
                            when (and (equalp client (name (client ev)))
                                      (equalp channel (name (channel ev))))
                            return target)))
          (when (and target
                     (or (not (equal (name (user ev)) (username (client ev))))
                         (char/= #\< (char message 0))))
            (do-issue (first (cores client)) lichat-cmd:message
              :client client :from (username client) :channel target :text message :bridge (name user)))))))
