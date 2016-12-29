#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.chatlog)

(defun connection (designator)
  (with-storage (designator)
    (list (defaulted-value "chatlog" :database)
          (defaulted-value "chatlog" :user)
          (defaulted-value NIL :password)
          (defaulted-value "localhost" :host)
          :port (defaulted-value 5432 :port))))

(defun (setf connection) (values designator)
  (destructuring-bind (&key (database "chatlog")
                            (user "chatlog")
                            (password NIL)
                            (host "localhost")
                            (port 5432)) values
    (with-storage (designator)
      (setf (value :database) database)
      (setf (value :user) user)
      (setf (value :password) password)
      (setf (value :host) host)
      (setf (value :port) port))))

(defmacro with-db ((&optional (designator ''chatlog)) &body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk () ,@body))
       (if postmodern:*database*
           (,thunk)
           (postmodern:with-connection (connection ,designator)
             (,thunk))))))

(defun prepared-statement (statement &rest variables)
  (cl-postgres:prepare-query postmodern:*database* "" statement)
  (cl-postgres:exec-prepared postmodern:*database* "" variables
                             (cl-postgres:row-reader (fields)
                               (loop while (cl-postgres:next-row)
                                     collect (loop for field across fields
                                                   collect (cl-postgres:next-field field))))))

(defun initialize-database (&rest args &key database user password host port)
  (declare (ignore database user password host port))
  (setf (connection 'chatlog) args)
  (with-db ()
    (unless (postmodern:table-exists-p "channels")
      (postmodern:execute "CREATE TABLE \"channels\" (
                             \"id\" serial PRIMARY KEY,
                             \"server\" varchar(64) NOT NULL,
                             \"channel\" varchar(64) NOT NULL,
                             UNIQUE(\"server\",\"channel\"))"))
    (unless (postmodern:table-exists-p "chatlog")
      (postmodern:execute "CREATE TABLE \"chatlog\" (
                             \"id\" serial PRIMARY KEY,
                             \"channel-id\" int REFERENCES \"channels\"(\"id\") ON DELETE CASCADE,
                             \"nick\" varchar(36) NOT NULL,
                             \"time\" bigint NOT NULL,
                             \"type\" character(1) NOT NULL,
                             \"message\" text NOT NULL)")
      (postmodern:execute "CREATE INDEX \"chatlog_channel-id_index\" 
                             ON \"chatlog\" (\"channel-id\")")))
  T)

(defmethod channel-designator ((event channel-event))
  (channel-designator (channel event)))

(defmethod channel-designator ((user user))
  (channel-designator (cons (name (client user)) (name user))))

(defmethod channel-designator ((channel channel))
  (channel-designator (cons (name (client channel)) (name channel))))

(defmethod channel-designator ((spec cons))
  (setf (car spec) (string-downcase (car spec)))
  (setf (cdr spec) (string-downcase (cdr spec)))
  spec)

(defmethod user-designator ((channel channel))
  (name channel))

(defmethod user-designator ((user user))
  (name user))

(defmethod user-designator ((name string))
  name)

(defun channel-exists-p (channel-ish)
  (let ((channel (channel-designator channel-ish)))
    (with-db ()
      (not (null (prepared-statement "SELECT * FROM \"channels\" WHERE \"server\"=$1 AND \"channel\"=$2"
                                     (car channel) (cdr channel)))))))

(defun add-channel (channel-ish)
  (let ((channel (channel-designator channel-ish)))
    (with-db ()
      (when (channel-exists-p channel)
        (error "The channel ~a is already logged." channel))
      (prepared-statement "INSERT INTO \"channels\" (\"server\",\"channel\") VALUES ($1,$2)"
                          (car channel) (cdr channel))))
  channel-ish)

(defun remove-channel (channel-ish)
  (let ((channel (channel-designator channel-ish)))
    (with-db ()
      (unless (channel-exists-p channel)
        (error "The channel ~a wasn't logged to begin with." channel))
      (prepared-statement "REMOVE FROM \"channels\" WHERE \"server\"=$1 AND \"channel\"=$2"
                          (car channel) (cdr channel))))
  channel-ish)

(defun type->char (type)
  (case type
    ((:message) "m")
    ((:action :self) "a")
    ((:nick :name) "n")
    ((:quit :disconnect) "q")
    ((:leave :part) "p")
    ((:enter :join) "j")
    ((:kick) "k")
    ((:mode) "o")
    ((:topic) "t")
    (T (string type))))

(defun record-message (type channel user message &rest format-args)
  (let ((channel (channel-designator channel))
        (user (user-designator user)))
    (with-db ()
      (prepared-statement "INSERT INTO \"chatlog\" (\"channel-id\",\"nick\",\"time\",\"type\",\"message\")
                           VALUES ((SELECT \"id\" FROM \"channels\" WHERE \"channels\".\"server\"=$1
                                                                    AND \"channels\".\"channel\"=$2),
                                   $3,$4,$5,$6)"
                          (car channel) (cdr channel) user (get-unix-time) (type->char type)
                          (apply #'format NIL message format-args)))))

(defun process-back-queue (c)
  (with-db ()
    (loop for (type channel user message format-args) in (back-queue c)
          do (when (channel-exists-p channel)
               (apply #'record-message type channel user message format-args))
             (pop (back-queue c)))))

(defun maybe-record-message (c type channel user message &rest format-args)
  (bt:with-lock-held ((lock c))
    (handler-case
        (with-db ()
          (push (list type channel user message format-args)
                (back-queue c))
          (process-back-queue c))
      (error (err)
        (v:debug :maiden.chatlog "Failed to record message: ~a" err)))))
