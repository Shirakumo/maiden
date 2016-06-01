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

(defun activate (&rest modules)
  (with-storage ('activatable)
    (dolist (module modules)
      (setf (value (normalize-module-name module)) T))))

(defun deactivate (&rest modules)
  (with-storage ('activatable)
    (dolist (module modules)
      (remvalue (normalize-module-name module)))))

(defun active-p (module)
  (with-storage ('activatable)
    (value (normalize-module-name module))))

(defclass activatable-handler (queued-handler)
  ((module :initarg :module :reader module))
  (:default-initargs
   :module *package*))

(defmethod handle :around (event (handler activatable-handler))
  (when (active-p (module handler))
    (call-next-method)))

(define-consumer activatable (agent)
  ())

(define-command (activatable activate) (c ev &rest modules)
  (apply activate modules)
  (reply ev "Modules activated."))

(define-command (activatable deactivate) (c ev &rest modules)
  (apply deactivate modules)
  (reply ev "Modules deactivated."))
