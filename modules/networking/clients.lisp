(in-package #:org.shirakumo.maiden.modules.networking)

(defgeneric client-connected-p (client))
(defgeneric close-connection (client))
(defgeneric initiate-connection (client))
(defgeneric handle-connection (client))
(defgeneric handle-connection-error (err client))
(defgeneric handle-connection-idle (tcp-client))
(defgeneric process (message tcp-client))
(defgeneric send (thing tcp-client))
(defgeneric receive (tcp-client))
(defgeneric accept (socket tcp-server))
(defgeneric make-tcp-server-client (server socket))

(define-consumer remote-client (client)
  ())

(defmethod print-object ((client remote-client) stream)
  (print-unreadable-object (client stream :type T)
    (format stream "~:[~a~;~:*~a~*~]~@[ ~s~]" (name client) (id client) (when (client-connected-p client) :connected))))

(defmethod start :after ((client remote-client))
  (unless (client-connected-p client)
    (initiate-connection client)))

(defmethod stop :before ((client remote-client))
  (when (client-connected-p client)
    (close-connection client)))

(defmethod initiate-connection :around ((client remote-client))
  (call-next-method)
  client)

(defmethod initiate-connection :after ((client remote-client))
  (broadcast (cores client) 'connection-initiated :client client)
  (v:info :maiden.client.connection "~a connection established." client))

(defmethod close-connection :around ((client remote-client))
  (call-next-method)
  client)

(defmethod close-connection :after ((client remote-client))
  (broadcast (cores client) 'connection-closed :client client))

(define-consumer ip-client (remote-client)
  ((host :initarg :host :accessor host)
   (port :initarg :port :accessor port))
  (:default-initargs
   :host (error "HOST required.")))

(defmethod print-object ((client ip-client) stream)
  (print-unreadable-object (client stream :type T)
    (format stream "~:[~a~;~:*~a~*~] ~@[~s ~]~s ~a:~a"
            (name client) (id client) (when (client-connected-p client) :connected) :host (host client) (port client))))

(define-consumer socket-client (ip-client)
  ((socket :initform NIL :accessor socket)
   (read-thread :initform NIL :accessor read-thread)
   (send-lock :initform NIL :accessor send-lock)
   (recv-lock :initform NIL :accessor recv-lock)))

(defmethod initialize-instance :after ((client socket-client) &key)
  (setf (send-lock client) (bt:make-recursive-lock (format NIL "send lock ~a" client)))
  (setf (recv-lock client) (bt:make-recursive-lock (format NIL "recv lock ~a" client))))

(defmethod client-connected-p ((client socket-client))
  (socket client))

(defmethod initiate-connection :around ((client socket-client))
  (let ((success NIL))
    (unwind-protect (multiple-value-prog1 (call-next-method)
                      (setf success T))
      (unless success
        (ignore-errors (usocket:socket-close (socket client)))
        (setf (socket client) NIL)))))

(defmethod initiate-connection :after ((client socket-client))
  (let ((read-thread (read-thread client)))
    (unless (and read-thread (bt:thread-alive-p read-thread))
      (setf (read-thread client) (deeds::make-thread (lambda () (handle-connection client))
                                                     (format NIL "~a read thread" client))))))

(defmethod close-connection :around ((client socket-client))
  (handler-bind ((error (lambda (err)
                          (warn 'client-connection-closed-uncleanly-warning :client client :closing-error err)
                          (when (find-restart 'continue)
                            (continue)))))
    (unwind-protect
         (call-next-method)
      (setf (socket client) NIL))))

(defmethod close-connection ((client socket-client))
  (let ((read-thread (read-thread client)))
    (unwind-protect
         (when (and read-thread (bt:thread-alive-p read-thread))
           (cond ((eql (bt:current-thread) read-thread)
                  (when (find-restart 'abort)
                    (abort)))
                 (T
                  (bt:interrupt-thread read-thread (lambda () (invoke-restart 'abort))))))
      (when (socket client)
        (ignore-errors (usocket:socket-close (socket client)))))))

(defmethod handle-connection :around ((client client))
  (unwind-protect
       (handler-bind ((error (lambda (err)
                               (v:error :maiden.client.connection "~a encountered severe error in connection handling: ~a" client err)
                               (v:debug :maiden.client.connection err))))
         (with-retry-restart (continue "Retry handling the connection.")
           (restart-case
               (handler-bind ((error #'maybe-invoke-debugger))
                 (handler-bind (((or usocket:socket-error cl:stream-error client-timeout-error)
                                  (lambda (err)
                                    (invoke-restart 'handle-error err))))
                   (call-next-method)))
             (abort (&optional err)
               :report "Exit the connection handling."
               (declare (ignore err)))
             (handle-error (&optional err)
               :report "Attempt to recover by handling the connection error."
               (handle-connection-error err client)))))
    (v:debug :maiden.client.connection "~a Exiting connection handling"
             client)
    (close-connection client)))

(defmethod receive :around ((client socket-client))
  (bt:with-recursive-lock-held ((recv-lock client))
    (call-next-method)))

(defmethod send :around (message (client socket-client))
  (bt:with-recursive-lock-held ((send-lock client))
    (call-next-method)))

(define-consumer reconnecting-client (socket-client)
  ((failures :initform 0 :accessor failures)
   (max-failures :initarg :max-failures :accessor max-failures)
   (backoff :initarg :backoff :accessor backoff)
   (interval :initarg :interval :accessor interval)
   (max-reconnect-delay :initarg :max-reconnect-delay :accessor max-reconnect-delay))
  (:default-initargs
   :max-failures NIL
   :backoff :exponential
   :interval 2
   :max-reconnect-delay (* 60 60)))

(defmethod handle-connection-error (err (client reconnecting-client))
  (when err (v:debug :maiden.client.reconnection err))
  (v:warn :maiden.client.reconnection "~a Encountered a connection error. Attempting to reconnect..." client)
  (cond ((eq (bt:current-thread) (read-thread client))
         ;; We cannot call CLOSE-CONNECTION as it would end
         ;; our own thread with the restart invocation.
         ;; We don't care if it fails to close gracefully.
         (ignore-errors (close (socket-stream client)))
         (ignore-errors (usocket:socket-close (socket client)))
         (broadcast (cores client) 'connection-closed :client client))
        (T
         (ignore-errors (close-connection client))))
  (setf (failures client) 0)
  (loop (when (and (max-failures client)
                   (< (max-failures client) (failures client)))
          (v:error :maiden.client.reconnection "~a Exceeded maximum reconnection attempts." client)
          (error 'client-reconnection-exceeded-error :client client))
        (incf (failures client))
        (sleep (min (ecase (backoff client)
                      (:constant (interval client))
                      (:linear (* (interval client) (failures client)))
                      (:exponential (expt (interval client) (failures client))))
                    (max-reconnect-delay client)))
        (handler-case
            (progn (initiate-connection client)
                   (setf (failures client) 0)
                   (continue))
          (error (err)
            (v:error :maiden.client.reconnection "~a Failed to reconnect: ~a" client err)))))

(define-consumer timeout-client (remote-client)
  ((timeout :initarg :timeout :accessor timeout)
   (last-received-time :initform NIL :accessor last-received-time))
  (:default-initargs
   :timeout 120))

(defmethod receive :after ((client timeout-client))
  (setf (last-received-time client) (get-universal-time)))

(defmethod handle-connection-idle :before ((client timeout-client))
  (when (and (last-received-time client) (< (timeout client) (- (get-universal-time) (last-received-time client))))
    (error 'client-timeout-error :timeout (- (get-universal-time) (last-received-time client)) :client client)))

(define-consumer text-client (socket-client)
  ((encoding :initarg :encoding :accessor encoding)
   (buffer :initarg :buffer :accessor buffer))
  (:default-initargs
   :encoding :utf-8
   :buffer :line))

(defmethod initiate-connection :around ((client text-client))
  (with-default-encoding ((encoding client))
    (call-next-method)))

(defmethod receive :around ((client text-client))
  (handler-bind (#+sbcl (sb-int:stream-decoding-error
                          (lambda (err)
                            (v:warn :maiden.client.receive err)
                            (invoke-restart 'sb-int:attempt-resync))))
    (call-next-method)))

(defmethod receive ((client text-client))
  (etypecase (buffer client)
    ((eql :line) (read-line (socket-stream client)))
    (string (read-sequence (buffer client) (socket-stream client)))))

(defmethod send ((message string) (client text-client))
  (let ((stream (socket-stream client)))
    (write-string message stream)
    (finish-output stream)))

(define-consumer tcp-client (socket-client)
  ((element-type :initform '(unsigned-byte 8) :reader element-type)
   (idle-interval :initarg :idle-interval :accessor idle-interval)
   (ssl :initform NIL :initarg :ssl :accessor ssl)
   (socket-stream :initform NIL :accessor socket-stream))
  (:default-initargs
   :idle-interval 10))

(defmethod client-connected-p ((client tcp-client))
  (and (call-next-method)
       (socket-stream client)
       (open-stream-p (socket-stream client))))

(defmethod initiate-connection ((client tcp-client))
  (with-slots (host port) client
    (unless (and (socket client) (open-stream-p (socket-stream client)))
      (cond ((ssl client)
             (let* ((socket (usocket:socket-connect host port :element-type '(unsigned-byte 8)))
                    (format (cond ((equal (element-type client) '(unsigned-byte 8)) NIL)
                                  ((equal (element-type client) 'character) :utf-8)))
                    (context (apply #'cl+ssl:make-context
                                    :verify-callback NIL
                                    :verify-mode cl+ssl:+ssl-verify-none+
                                    (etypecase (ssl client) ((eql T) ()) (list (ssl client)))))
                    (s (usocket:socket-stream socket)))
               (setf (socket client) socket)
               (cl+ssl:with-global-context (context)
                 (setf (socket-stream client) (cl+ssl:make-ssl-client-stream (cl+ssl:stream-fd s)
                                                                             :verify :optional
                                                                             :hostname (host client)
                                                                             :close-callback (lambda () (close s) (cl+ssl:ssl-ctx-free context))
                                                                             :external-format format)))))
            (T
             (setf (socket client) (usocket:socket-connect host port :element-type (element-type client)))
             (setf (socket-stream client) (usocket:socket-stream (socket client))))))))

(defmethod handle-connection ((client tcp-client))
  (with-retry-restart (continue "Discard the message and continue.")
    (let ((time (get-universal-time)))
      (loop (usocket:wait-for-input (socket client) :timeout 1)
            (cond ((not (socket client))
                   (return))
                  ((<= (idle-interval client)
                       (- (get-universal-time) time))
                   (setf time (get-universal-time))
                   (handle-connection-idle client))
                  ((find (usocket:socket-state (socket client)) '(:read :read-write))
                   (process (receive client) client)))))))

(defmethod handle-connection-idle ((client tcp-client))
  client)

(define-consumer text-tcp-client (text-client tcp-client)
  ((element-type :initform 'character :reader element-type)))

(define-consumer tcp-server (socket-client)
  ((clients :initform () :accessor clients)))

(defmethod (setf clients) :around (clients (server tcp-server))
  (bt:with-recursive-lock-held ((lock server))
    (call-next-method)))

(defmethod initiate-connection ((server tcp-server))
  (with-slots (host port) server
    (setf (socket server) (usocket:socket-listen host port))))

(defmethod close-connection :after ((server tcp-server))
  (loop for client = (car (clients server))
        while client
        do (close-connection client)
           (loop while (client-connected-p client)
                 do (sleep 0.1))
           (pop (clients server))))

(defmethod handle-connection ((server tcp-server))
  (loop for socket = (usocket:socket-accept (socket server) :element-type '(unsigned-byte 8))
        do (accept socket server)))

(defmethod accept (socket (server tcp-server))
  (initiate-connection (make-tcp-server-client server socket)))

(defmethod make-tcp-server-client ((server tcp-server) socket)
  (make-instance 'tcp-server-client
                 :server server :socket socket
                 :host (usocket:get-peer-address socket)
                 :port (usocket:get-peer-port socket)
                 :name NIL))

(define-consumer tcp-server-client (tcp-client)
  ((server :initarg :server :accessor server)
   (socket :initarg :socket :accessor socket))
  (:default-initargs
   :server (error "SERVER required.")))

(defmethod handle-connection-error (err (client tcp-server-client))
  (abort))

(defmethod initialize-instance :after ((client tcp-server-client) &key)
  (push client (clients (server client))))

(defmethod close-connection :after ((client tcp-server-client))
  (bt:with-recursive-lock-held ((lock (server client)))
    (setf (clients (server client)) (remove client (clients (server client))))))
