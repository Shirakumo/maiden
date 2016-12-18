#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.throttle)

(define-consumer throttle (agent)
  ((attempts :accessor attempts)
   (time-frame :accessor time-frame)
   (cooldown-function :accessor cooldown-function)
   (cooldown-step :accessor cooldown-step)
   (cooldown-max :accessor cooldown-max)
   (records :initform (make-hash-table :test 'eql) :accessor records)))

(defmethod initialize-instance :after ((throttle throttle) &key attempts time-frame cooldown-function cooldown-step cooldown-max)
  (setf (attempts throttle) (or attempts (value :attempts) 3))
  (setf (time-frame throttle) (or time-frame (value :time-frame) 5))
  (setf (cooldown-function throttle) (or cooldown-function (value :cooldown :function) :linear))
  (setf (cooldown-step throttle) (or cooldown-step (value :cooldown :step) 10))
  (setf (cooldown-max throttle) (or cooldown-max (value :cooldown :max) (* 60 60 24))))

(defmethod (setf cooldown-function) :before (value (throttle throttle))
  (ecase value (:constant) (:linear) (:exponential)))

(define-stored-accessor throttle attempts :attempts)
(define-stored-accessor throttle time-frame :time-frame)
(define-stored-accessor throttle cooldown-function :cooldown :function)
(define-stored-accessor throttle cooldown-step :cooldown :step)
(define-stored-accessor throttle cooldown-max :cooldown :maximum)

(defmethod record (user throttle)
  (gethash user (records throttle)))

(defmethod (setf record) (value user throttle)
  (setf (gethash user (records throttle)) value))

(defclass record ()
  ((attempts :initarg :attempts :accessor attempts)
   (timestamp :initarg :timestamp :accessor timestamp)
   (timeout :initarg :timeout :accessor timeout))
  (:default-initargs
   :attempts 0
   :timestamp (get-universal-time)
   :timeout 0))

(defmethod clear-tax (user (throttle throttle))
  (setf (record user throttle) (make-instance 'record)))

;; FIXME: People with an excemption permission should not be taxed.
(defmethod tax (user (throttle throttle))
  (let ((record (or (record user throttle)
                    (clear-tax user throttle))))
    (with-accessors ((attempts attempts) (timestamp timestamp) (timeout timeout)) record
      (cond ((< timestamp (get-universal-time) (+ timestamp timeout))
             (incf attempts)
             (let ((counter (- attempts (attempts throttle))))
               (setf timeout (min (cooldown-max throttle)
                                  (ecase (cooldown-function throttle)
                                    (:constant (cooldown-step throttle))
                                    (:linear (* (cooldown-step throttle) counter))
                                    (:exponential (expt (cooldown-step throttle) counter)))))))
            ((< timestamp (get-universal-time) (+ timestamp (time-frame throttle)))
             (incf attempts)
             (when (< (attempts throttle) attempts)
               (setf timeout (cooldown-step throttle))))
            (T
             (setf timeout 0)
             (setf attempts 1)
             (setf timestamp (get-universal-time))))
      record)))

(defun ensure-cooldown-function (cooldown-function)
  (or (find cooldown-function '(:constant :linear :exponential) :test #'string-equal)
      (error "Invalid cooldown function ~s. Must be one of ~a"
             cooldown-function '(:constant :linear :exponential))))

(define-handler (throttle block-commands command-event) (c ev dispatch-event)
  :before '(:main)
  :class deeds:locally-blocking-handler
  (when (typep dispatch-event 'user-event)
    (let* ((record (tax (user dispatch-event) c)))
      (when (< 0 (timeout record))
        (reply dispatch-event "Please calm down. You are on cooldown for ~d second~:p."
               (- (+ (timestamp record) (timeout record)) (get-universal-time)))
        (cancel ev)))))

(define-command (throttle view-config) (c ev)
  :command "view throttle configuration"
  (reply ev "Throttling happens after ~d attempt~:p within ~a, which incurs a ~(~a~) cooldown of ~a for each additional attempt, up to a maximal cooldown of ~a."
         (attempts c) (format-relative-time (time-frame c))
         (cooldown-function c) (format-relative-time (cooldown-step c))
         (format-relative-time (cooldown-max c))))

(define-command (throttle set-config) (c ev &key attempts time-frame cooldown-function cooldown-step cooldown-max)
  :command "set throttle configuration"
  :advice (not public)
  (when attempts
    (setf (attempts c) (parse-integer attempts)))
  (when time-frame
    (setf (time-frame c) (parse-integer time-frame)))
  (when cooldown-function
    (setf (cooldown-function c) (ensure-cooldown-function cooldown-function)))
  (when cooldown-step
    (setf (cooldown-step c) (parse-integer cooldown-step)))
  (when cooldown-max
    (setf (cooldown-max c) (parse-integer cooldown-max)))
  (reply ev "Throttle configuration updated."))

(define-command (throttle clear-tax) (c ev user &key client)
  :command "clear throttling for"
  :advice (not public)
  (clear-tax (or (find-user user (if client
                                     (or (consumer client (core ev))
                                         (error "No client named ~s found." client))
                                     (client ev)))
                 (error "No user with name ~s found." user))
             c)
  (reply ev "Throttling tax has been lifted from ~a." user))
