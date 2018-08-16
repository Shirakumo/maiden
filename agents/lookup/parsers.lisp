#|
 This file is a part of Maiden
 (c) 2017 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.lookup)

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
      "#symbol-index article, #index article"
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

(defun url-html (url &key (encoding :utf-8))
  (multiple-value-bind (doc ret) (drakma:http-request url :force-binary T)
    (if (= 200 ret)
        (plump:parse (babel:octets-to-string doc :encoding encoding))
        (error "Could not fetch ~s." url))))

(defun parse-usocket-docs (&optional (url "https://common-lisp.net/project/usocket/api-docs.shtml"))
  (let ((doc (url-html url)))
    (lquery:$ doc
      ".sym>span"
      (map (lambda (node)
             (let* ((type-ish (plump:attribute node "class"))
                    (type (cond ((string= type-ish "function-name") "function")
                                ((string= type-ish "class-name") "class")
                                ((string= type-ish "var-name") "variable")))
                    (name (lquery:$1 node (text)))
                    (anchor (lquery:$1 node "a" (attr :name))))
               (list (list (format NIL "~a ~a" type name)
                           name)
                     (format NIL "~a#~a" url anchor)
                     (format NIL "~a ~a" type name))))))))

(defun parse-alexandria-docs (&optional (url "https://common-lisp.net/project/alexandria/draft/alexandria.html"))
  (let ((doc (url-html url)))
    (flet ((extract-type (string)
             (let ((dash (position #\— string))
                   (colon (position #\: string)))
               (subseq string (+ 2 dash) colon))))
      (lquery:$ doc
        ".defun" (combine (plump:first-child) ">b" "a[name]")
        (map-apply (lambda (type-text def-name link)
                     (let ((type (extract-type (plump:text type-text)))
                           (name (lquery:$1 def-name (text))))
                       (list (list (format NIL "~(~a~) ~a" type name)
                                   name)
                             (format NIL "~a#~a" url (lquery:$1 link (attr :name)))
                             (format NIL "~a ~a" type name)))))))))

(defun parse-weitz-docs (url &optional (encoding :utf-8))
  (let ((doc (url-html url :encoding encoding)))
    (lquery:$ doc
      "p a.none[name]"
      (map (lambda (node)
             (let* ((name (or (lquery:$1 node "b" (text))
                              (lquery:$1 node (next-all "b") (text))))
                    (text (plump:text (plump:parent node))))
               (cl-ppcre:register-groups-bind (type) ("\\[([^\\]]+)\\]" text)
                 (when (and type name)
                   (let ((fullname (format NIL "~a ~a" type name)))
                     (list (list fullname name)
                           (format NIL "~a#~a" url (plump:attribute node "name"))
                           fullname))))))))))

(defmacro define-weitz-doc-lookup (name url &optional (encoding :utf-8))
  `(define-table-lookup ,name
     (("About") ,url ,(format NIL "About ~:(~a~)" name))
     ,@(delete NIL (coerce (parse-weitz-docs url encoding) 'list))))

(defun parse-cffi-docs (&optional (url "https://common-lisp.net/project/cffi/manual/cffi-manual.html"))
  (let ((doc (url-html url)) (results ()))
    (lquery:$ doc "h2.chapter,h3.section"
      (each (lambda (node)
              (let ((name (plump:text node))
                    (anchor (lquery:$1 node (prev "a") (attr :name))))
                (when anchor
                  (push (list (list name) (format NIL "~a#~a" url anchor))
                        results)))
              T)))
    (lquery:$ doc "h3.heading"
      (each (lambda (node)
              (let ((anchor (lquery:$1 node (prev "a") (attr :name)))
                    (names (cl-ppcre:split "\\s*,+\\s*" (plump:text node)))
                    (types (lquery:$ node (next-all "dl") (text))))
                (loop for name in names
                      for type across types
                      do (cl-ppcre:register-groups-bind (type) ("^\\s*([^:]+):" type)
                           (when (and anchor type)
                             (let ((full-name (format NIL "~a ~a" type name)))
                               (push (list (list full-name name)
                                           (format NIL "~a#~a" url anchor)
                                           full-name)
                                     results)))))
                T))))
    (nreverse results)))
