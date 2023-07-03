(in-package #:org.shirakumo.maiden.agents.accounts)

;; Reroute to account if it is present for the user.
(defmethod data-value :around (field (user user))
  (if (eql field 'account)
      (call-next-method)
      (let ((account (account user :error NIL)))
        (or (and account (field field account user))
            (call-next-method)))))

;; We also implement methods for account names
(defmethod data-value (field (name symbol))
  (data-value field (account name)))

(defmethod data-value (field (name string))
  (data-value field (account name)))

(define-consumer accounts (agent)
  ())

(defmethod start :after ((accounts accounts))
  (load-all-accounts))

(defun random-string (length)
  (with-output-to-string (out)
    (loop repeat length do (write-char (elt "abcdefghijklmnopqrstuvwxyz0123456789" (random 36)) out))))

;; FIXME: hash passwords
(define-command (accounts login) (c ev password &optional account)
  :command "login"
  :advice public
  (let ((account (or (account (or account (user ev)) :error NIL)
                     (account (name (user ev)) :error NIL))))
    (cond ((not account)
           (error "This identity is not associated with any account."))
          ((account (user ev) :error NIL)
           (error "I still remember you, ~a." (name (user ev))))
          ((string/= password (password account))
           (error "Invalid password."))
          (T
           (setf (account (user ev)) account)
           ;; Since we now "know" this user we can force
           ;; authentication on it globally. Some other modules
           ;; that work independently of the accounts rely on
           ;; authentication being present to make user features
           ;; work, so it is a good idea for us to do this. Esp.
           ;; since this mitigates the problem of some clients
           ;; potentially not being able to provide any way to
           ;; authenticate users natively, we can now provide a
           ;; way to provide it from our side.
           (setf (slot-value (user ev) 'authenticated) T)
           (reply ev "Welcome back, ~a." (name (user ev)))))))

(define-command (accounts logout) (c ev)
  :command "logout"
  :advice public
  (cond ((not (account (account (user ev))))
         (error "This identity is not associated with any account."))
        (T
         (setf (account (user ev)) NIL)
         (reply ev "Goodbye, ~a." (name (user ev))))))

(define-command (accounts create) (c ev &key password name)
  :command "create account"
  :advice public
  (let ((name (if (or (not name) (string= name "")) (name (user ev)) name))
        (password (if (or (not password) (string= password "")) (random-string 6) password)))
    (when (account (identity (user ev)) :error NIL)
      (error "This identity is already tied to ~a." (account ev)))
    (let ((account (make-instance 'account :name name :password password)))
      (setf (account (user ev)) account)
      (reply ev "Your account ~a has been created with the password ~s." name password)
      (cond ((authenticated-p (user ev))
             (add-identity account ev))
            (T
             (setf (slot-value (user ev) 'authenticated) T)
             (reply ev "Since this identity is not authenticated, you will not be logged in automatically in the future."))))))

(define-command (accounts destroy) (c ev)
  :command "destroy account"
  :advice public
  (let ((account (account (user ev) :error NIL)))
    (unless account
      (error "Please log in to this account first."))
    (delete-account account)
    (setf (account (user ev)) NIL)
    (reply ev "Your account ~a has been deleted and you have been logged out." (name account))))

(define-command (accounts update-password) (c ev new-password)
  :command "update password"
  :advice public
  (let ((account (account (user ev))))
    (setf (password account) new-password)
    (reply ev "Your password has been updated.")))

(define-command (accounts associate) (c ev account password)
  :command "associate with account"
  :advice public
  (let ((account (account account)))
    (when (account (identity (user ev)) :error NIL)
      (error "This identity is already associated with ~a." (account (identity ev))))
    (unless (authenticate (user ev) (client ev))
      (error "You must be authenticated to associate this identity with the account."))
    (unless (string= (password account) password)
      (error "Invalid password."))
    (add-identity account ev)
    (setf (account (user ev)) account)
    (reply ev "Identity associated. You will now be automatically logged in.")))

(define-command (accounts deassociate) (c ev)
  :command "deassociate from account"
  :advice public
  (let ((account (account (user ev))))
    (remove-identity account ev)
    (reply ev "Identity deassociated.")))

(define-command (accounts field) (c ev field &optional account)
  :command "get"
  :advice public
  (let* ((account (account (or account (user ev))))
         (value (field field account (user ev))))
    (if value
        (reply ev "The field is set to ~s." value)
        (reply ev "The field is not set to anything." field))))

(define-command (accounts set-field) (c ev field value &optional account)
  :command "set"
  :advice public
  (let ((account (account (or account (user ev)))))
    (setf (field field account (user ev)) value)
    (reply ev "The field has been set to ~s." value)))

(define-command (accounts test-authentication) (c ev &optional user)
  :command "test authentication"
  :advice public
  (let ((user (or user (name (user ev)))))
    (reply ev "~@(~:[~a is not authenticated.~;~a is authenticated.~]~)"
           (authenticate user (client ev)) user)))
