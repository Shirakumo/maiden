#|
 This file is a part of Maiden
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.lookup)

(defvar *lookup-functions* (make-hash-table :test 'equalp))

(defun lookup-function (archive)
  (or (gethash (string archive) *lookup-functions*)
      (error "Unknown archive ~s." archive)))

(defun (setf lookup-function) (function archive)
  (setf (gethash (string archive) *lookup-functions*) function))

(defun remove-lookup-function (archive)
  (remhash (string archive) *lookup-functions*))

(defun list-archives ()
  (loop for archive being the hash-keys of *lookup-functions*
        collect archive))

(defmacro define-lookup-function (archive args &body body)
  `(setf (lookup-function ',archive)
         (lambda ,args ,@body)))

(defun look-up (archive term)
  (funcall (lookup-function archive) term))

(defmacro define-webpage-lookup (archive args &body body)
  `(define-lookup-function ,archive ,args
     (multiple-value-bind (root code headers url)
         (request-as :html (progn ,@body))
       (declare (ignore headers))
       (when (/= code 200)
         (error "~s not found in ~a." ,(first args) ',archive))
       (list (list ,(first args)
                   (puri:render-uri url NIL)
                   (lquery:$1 root "title" (text)))))))

(defun longest (things)
  (let ((longest (first things)))
    (dolist (thing (rest things) longest)
      (when (< (length longest) (length thing))
        (setf longest thing)))))

(defun table-find (term table)
  (let ((term (cl-ppcre:split " +" term)))
    (loop for (matches . data) in table
          ;; Attempt to do exact match first.
          do (dolist (match matches)
               (when (loop for part in term
                           always (string-equal part match))
                 (return-from table-find
                   (list (list* match data)))))
          ;; Otherwise collect if fuzzy matching.
          when (loop for match in matches
                     thereis (loop for part in term
                                   always (search part match :test #'char-equal)))
          collect (list* (longest matches) data))))

(defmacro define-table-lookup (archive &body entries)
  (let ((term (gensym "TERM"))
        (entries (loop for entry in entries
                       collect (destructuring-bind (matches url &optional title) entry
                                 (list (enlist matches)
                                       url
                                       (or title (format NIL "~{~@(~a~)~^ ~}" (enlist matches))))))))
    `(define-lookup-function ,archive (,term)
       (or (table-find ,term ',entries)
           (error "~s not found in ~a." ,term ',archive)))))

(defun split-section-title (text)
  (let ((section (with-output-to-string (out)
                   (loop for c across text
                         do (if (find c "0123456789.")
                                (write-char c out)
                                (return))))))
    (when (string/= "" section)
      (list section
            (string-right-trim "." section)
            (string-left-trim " " (subseq text (length section)))))))

(defun generate-table-from-staple-page (url)
  (let ((doc (etypecase url
               (string (multiple-value-bind (doc ret) (drakma:http-request url)
                         (if (= 200 ret)
                             doc
                             (error "No such page ~s" url))))
               (pathname url)))
        (entries ()))
    (lquery:$ (initialize doc)
      "#documentation" "h1,h2,h3,h4,h5,h6"
      (each (lambda (a)
              (let ((id (lquery:$1 a (attr :id)))
                    (text (string-trim '(#\Space #\Tab #\Return #\Linefeed) (lquery:$1 a (text)))))
                (push `((,text ,@(split-section-title text))
                        ,(format NIL "~a~@[#~a~]" url id)
                        ,text)
                      entries))
              T)))
    (lquery:$ (initialize doc)
      "#symbol-index article"
      (each (lambda (a)
              (cl-ppcre:register-groups-bind (b c d) ("([^ ]+ ([^:]+:(.+)))" (lquery:$1 a (attr :id)))
                (push `((,b ,c ,d)
                        ,(format NIL "~a~a" url (lquery:$1 a ".name a" (attr :href)))
                        ,(format NIL "~@(~a~)~a" (char b 0) (subseq b 1)))
                      entries))
              T)))
    (nreverse entries)))

(defmacro define-staple-doc-lookup (archive url)
  `(define-table-lookup ,archive
     ,@(generate-table-from-staple-page url)))
