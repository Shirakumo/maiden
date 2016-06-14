#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.quicklisp)

(define-consumer quicklisp (agent)
  ())

(defun dists-and-versions (&optional (dists (ql-dist:all-dists)))
  (loop for dist in dists
        collect (list (ql-dist:name dist) (ql-dist:version dist))))

(defun dist-for-system (system)
  (loop for dist in (ql-dist:all-dists)
        thereis (and (ql-dist:find-system-in-dist system dist)
                     dist)))

(defun check-dists-available (dists)
  (dolist (dist dists)
    (unless (ql-dist:find-dist dist)
      (error "No such dist ~s." sys))))

(defun check-systems-available (systems)
  (dolist (sys systems)
    (unless (ql-dist:find-system sys)
      (error "No such system ~s." sys))))

(defun check-systems-upgradable (systems)
  (dolist (sys systems)
    (unless (asdf:find-system sys)
      (error "No such system ~s." sys))
    (when (find sys (ql:list-local-systems) :test #'string-equal)
      (error "Cannot upgrade ~s as it is a local system. Manual intervention is required."
             system))))

(define-command (quicklisp version) (c ev &optional system)
  :command "show version"
  (let* ((system (or system "maiden"))
         (dist (dist-for-system system)))
    (reply ev "~a is on version ~a~{ (~a ~a)~}"
           system (asdf:component-version (asdf:find-system system T))
           (when dist (list (ql-dist:name dist) (ql-dist:version dist))))))

(define-command (quicklisp update) (c ev &rest dists)
  :advice ((not public))
  (let ((dists (or dists (mapcar #'ql-dist:name (ql-dist:all-dists)))))
    (check-dist-available dists)
    (reply ev "Updating ~{~a~^, ~}..." dists)
    (dolist (dist dists)
      (ql:update-dist dist :prompt NIL))
    (reply ev "Update done. ~{~{~a ~a~}~^, ~}" (dists-and-versions dists))))

(define-command (quicklisp upgrade) (c ev &rest systems)
  :advice ((not public))
  (let ((systems (or systems (asdf/operate:already-loaded-systems))))
    (check-systems-upgradable systems)
    (reply ev "Upgrading ~{~a~^, ~}...")
    (dolist (dist (remove-duplicates (mapcar #'dist-for-system systems)))
      (when dist (ql:update-dist dist :prompt NIL)))
    (dolist (sys systems)
      (ql:quickload sys :prompt NIL))
    (reply ev "~{~a~^, ~} upgraded." system)))

(define-command (quicklisp quickload) (c ev &rest systems)
  :advice ((not public))
  (check-systems-available systems)
  (ql:quickload systems :prompt NIL)
  (reply ev "~{~a~^, ~} quickloaded." systems))

(define-command (quicklisp uninstall) (c ev &rest systems)
  :advice ((not public))
  (check-systems-available systems)
  (dolist (sys systems)
    (ql-dist:uninstall (ql-dist:find-system sys)))
  (reply ev "~{~a~^, ~} uninstalled. Note that the code for the selected systems is still active if it was loaded."
         systems))

(define-command (quicklisp install-dist) (c ev url &key replace)
  :command "install dist"
  :advice ((not public))
  (ql-dist:install-dist url :prompt NIL :replace replace)
  (reply ev "Dist installed."))

(define-command (quicklisp uninstall-dist) (c ev dist)
  :command "uninstall dist"
  :advice ((not public))
  (check-dists-available (list dist))
  (ql-dist:uninstall (ql-dist:find-dist dist))
  (reply ev "Dist uninstalled."))
