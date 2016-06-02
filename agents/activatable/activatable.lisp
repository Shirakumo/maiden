#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.activatable)

(defun short-package-name (pkg)
  (let ((name (package-name pkg)))
    (dolist (nick (package-nicknames pkg) name)
      (when (< (length nick) (length name))
        (setf name nick)))))

(defun trim-package-prefix (name)
  (cond ((starts-with "maiden-" name) (subseq name 7))
        ((starts-with "org.shirakumo.maiden." name) (subseq name 21))
        (T name)))

(defun normalize-module-name (name)
  (trim-package-prefix
   (string-downcase
    (etypecase name
      (string name)
      (package (short-package-name name))
      (symbol (symbol-name name))))))

(defun normalize-ident (thing)
  (etypecase thing
    (channel-event
     (normalize-ident
      (cons (name (client thing)) (name (channel thing)))))
    (client-event
     (normalize-ident
      (cons (name (client thing)) NIL)))
    (cons
     (destructuring-bind (client . channel) thing
       (cons (string-downcase
              (etypecase client
                (entity (name client))
                (T (string client))))
             (string-downcase
              (etypecase channel
                (entity (name channel))
                (null NIL)
                (T (string channel)))))))))

(defun activate (ident &rest modules)
  (let ((ident (normalize-ident ident)))
    (with-storage ('activatable)
      (setf (value ident) (union (value ident) (mapcar #'normalize-module-name modules)
                                 :test #'string-equal)))))

(defun deactivate (ident &rest modules)
  (let ((ident (normalize-ident ident)))
    (with-storage ('activatable)
      (setf (value ident) (set-difference (value ident) (mapcar #'normalize-module-name modules)
                                          :test #'string-equal)))))

(defun active-p (ident module)
  (with-storage ('activatable)
    (member (normalize-module-name module)
            (value (normalize-ident ident))
            :test #'string-equal)))

(defclass activatable-handler (deeds:queued-handler)
  ((module :initarg :module :reader module))
  (:default-initargs
   :module *package*))

(defmethod handle :around ((event client-event) (handler activatable-handler))
  (when (active-p event (module handler))
    (call-next-method)))

(define-consumer activatable (agent)
  ())

(define-command (activatable activate) (c ev &rest modules)
  (apply #'activate ev modules)
  (reply ev "Modules activated."))

(define-command (activatable deactivate) (c ev &rest modules)
  (apply #'deactivate ev modules)
  (reply ev "Modules deactivated."))
