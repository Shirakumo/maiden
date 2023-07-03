(in-package #:org.shirakumo.maiden.agents.permissions)

(defvar *tokens* ())

(defun random-string (length)
  (with-output-to-string (out)
    (loop repeat length do (write-char (elt "abcdefghijklmnopqrstuvwxyz0123456789" (random 36)) out))))

(defun generate-token ()
  (let ((token (random-string 32)))
    (push token *tokens*)
    token))

(define-condition permission-denied (error)
  ((user :initarg :user)
   (perm :initarg :perm))
  (:default-initargs
   :user (error "USER required")
   :perm (error "PERM required"))
  (:report (lambda (c s) (format s "User ~s does not have access to ~s."
                                 (name (slot-value c 'user)) (slot-value c 'perm)))))

(defgeneric normalize-permission (perm))

(defmethod normalize-permission ((perm list))
  perm)

(defmethod normalize-permission ((perm string))
  (normalize-permission (cl-ppcre:split "[./]+" (string-downcase perm))))

(defmethod normalize-permission ((perm symbol))
  (normalize-permission (string-downcase perm)))

(defmethod normalize-permission ((package package))
  (normalize-permission (package-name package)))

(defmethod normalize-permission ((class standard-class))
  (let ((name (class-name class)))
    (append (normalize-permission (symbol-package name))
            (normalize-permission (symbol-name name)))))

(defmethod normalize-permission ((cmd maiden-commands:command-event))
  (normalize-permission (class-of cmd)))

(defmethod normalize-permission ((client client))
  (list* (string-downcase (name client))
         (normalize-permission (class-of client))))

(defmethod normalize-permission ((user user))
  (list* (string-downcase (name user))
         (normalize-permission (client user))))

(defun separate-grant-deny (perms)
  (let ((grant ())
        (deny ()))
    (loop for perm in perms
          for norm = (normalize-permission perm)
          do (if (equal (car norm) "!")
                 (push (cdr norm) deny)
                 (push norm grant)))
    (values grant deny)))

(defun perm-match-p (perm &rest matches)
  (let ((perm (normalize-permission perm)))
    (multiple-value-bind (grant deny) (separate-grant-deny matches)
      (flet ((branch-match (allow)
               (loop for p on perm
                     for a on allow
                     always (or (string-equal (car a) "*")
                                (string-equal (car p) (car a)))
                     finally (return (not (cdr a))))))
        (and (some #'branch-match grant)
             (notany #'branch-match deny))))))

(defun user-perm (user &optional client type)
  (typecase user
    (user (normalize-permission user))
    (T (list (string-downcase user) (string-downcase client) (string-downcase type)))))

(defun administrator-p (user &optional client type)
  (with-storage ('permissions)
    (apply #'perm-match-p (user-perm user client type) (value :administrators))))

(defun add-administrator (name &optional client type)
  (with-storage ('permissions)
    (pushnew (user-perm name client type) (value :administrators)
             :test #'perm-match-p)))

(defun remove-administrator (name &optional client type)
  (with-storage ('permissions)
    (setf (value :default-permissions)
          (remove (user-perm name client type) (value :administrators)
                  :test (lambda (a b) (perm-match-p b a))))))

(defun add-default-permission (perm)
  (with-storage ('permissions)
    (pushnew (normalize-permission perm) (value :default-permissions)
             :test #'perm-match-p)))

(defun remove-default-permission (perm)
  (with-storage ('permissions)
    (setf (value :default-permissions)
          (remove (normalize-permission perm) (value :default-permissions)
                  :test (lambda (a b) (perm-match-p b a))))))

(defun allowed-p (user perm)
  (with-storage ('permissions)
    (multiple-value-bind (user-grant user-deny) (separate-grant-deny (data-value :permissions user))
      (or (and (authenticated-p user) (administrator-p user))
          ;; User specific permissions must take precedence, so we have to
          ;; test for denies specifically after the default permissions to
          ;; make it possible to deny a user a permission that might be
          ;; granted by default.
          (and (or (apply #'perm-match-p perm user-grant)
                   (apply #'perm-match-p perm (value :default-permissions)))
               (not (apply #'perm-match-p perm user-deny)))))))

(defun grant (perm user)
  (let ((perm (normalize-permission perm)))
    (pushnew perm (data-value :permissions user))
    (setf (data-value :permissions user)
          (remove (cons "!" perm) (data-value :permissions user) :test #'equal))))

(defun deny (perm user)
  (let ((perm (normalize-permission perm)))
    (pushnew (cons "!" perm) (data-value :permissions user))
    (setf (data-value :permissions user)
          (remove perm (data-value :permissions user) :test #'equal))))

(defun check-allowed (user perm)
  (unless (allowed-p user perm)
    (error 'permission-denied :user user :perm perm)))

(defmacro with-permission ((user permission) &body body)
  `(when (allowed-p ,user ,permission)
     ,@body))

(define-consumer permissions (agent)
  ())

(define-handler (permissions check command-event) (c ev dispatch-event)
  :before '(:main)
  :class deeds:locally-blocking-handler
  :add-to-consumer NIL
  (unless (or (find 'public (advice ev) :test #'string=)
              (allowed-p (user dispatch-event) ev))
    (cancel ev)
    (reply dispatch-event "I can't let you do that, ~a." (name (user dispatch-event)))))

(define-command (permissions check-access) (c ev perm)
  :command "check access"
  (reply ev "Access to this permission is ~:[denied~;granted~]." (allowed-p (user ev) perm)))

(define-command (permissions grant) (c ev user perm)
  :advice (not public)
  (grant perm (ensure-user user (client ev)))
  (reply ev "Permission to ~a for ~a granted." perm user))

(define-command (permissions deny) (c ev user perm)
  :advice (not public)
  (deny perm (ensure-user user (client ev)))
  (reply ev "Permission to ~a for ~a denied." perm user))

(define-command (permissions add-administrator) (c ev user &optional client type)
  :command "add administrator"
  :advice (not public)
  (add-administrator user client type)
  (reply ev "~a is now an administrator." user))

(define-command (permissions remove-administrator) (c ev user &optional client type)
  :command "remove administrator"
  :advice (not public)
  (remove-administrator user client type)
  (reply ev "~a is no longer an administrator." user))

(define-command (permissions add-default-permission) (c ev perm)
  :command "add default permission"
  :advice (not public)
  (add-default-permission perm)
  (reply ev "Default permission to ~a granted." perm))

(define-command (permissions remove-default-permission) (c ev perm)
  :command "remove default permission"
  :advice (not public)
  (remove-default-permission perm)
  (reply ev "Default permission to ~a revoked." perm))

(define-command (permissions administrate-self) (c ev token)
  :command "upgrade to administrator"
  (cond ((find token *tokens* :test #'string=)
         (setf *tokens* (remove token *tokens* :test #'string=))
         (add-administrator (user ev))
         (reply ev "You have successfully been upgraded to administrator."))
        (T
         (error "That is not a valid token. Please generate one using (MAIDEN-PERMISSIONS:GENERATE-TOKEN)."))))
