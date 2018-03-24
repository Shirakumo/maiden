#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.dictionary)

(define-consumer dictionary (agent)
  ())

(defun synonyms (word &key (language "en"))
  (let* ((word (oxenfurt:find-word word :synonyms T :source-lang language))
         (lexical-entry (first (oxenfurt:lexical-entries word)))
         (entry (first (oxenfurt:entries lexical-entry)))
         (synonyms ()))
    (labels ((process-sense (sense)
               (unless (find :rare (oxenfurt:registers sense))
                 (mapc #'process-sense (oxenfurt:subsenses sense))
                 (dolist (synonym (oxenfurt:synonyms sense))
                   (push (oxenfurt:text synonym) synonyms)))))
      (mapc #'process-sense (oxenfurt:senses entry))
      (nreverse (delete-duplicates synonyms :test #'string-equal)))))

(defun antonyms (word &key (language "en"))
  (let* ((word (oxenfurt:find-word word :antonyms T :source-lang language))
         (lexical-entry (first (oxenfurt:lexical-entries word)))
         (entry (first (oxenfurt:entries lexical-entry)))
         (antonyms ()))
    (labels ((process-sense (sense)
               (unless (find :rare (oxenfurt:registers sense))
                 (mapc #'process-sense (oxenfurt:subsenses sense))
                 (dolist (antonym (oxenfurt:antonyms sense))
                   (push (oxenfurt:text antonym) antonyms)))))
      (mapc #'process-sense (oxenfurt:senses entry))
      (nreverse (delete-duplicates antonyms :test #'string-equal)))))

(defun definitions (word &key (language "en") kind)
  (let* ((word (oxenfurt:find-word word
                                   :source-lang language
                                   :filters (when kind `(:lexical-category ,kind))))
         (definitions ()))
    (dolist (lexical-entry (oxenfurt:lexical-entries word))
      (dolist (entry (oxenfurt:entries lexical-entry))
        (dolist (sense (oxenfurt:senses entry))
          (dolist (definition (oxenfurt:definitions sense))
            (push definition definitions)))))
    (nreverse definitions)))

(defun pronunciations (word &key (language "en") kind)
  (let* ((word (oxenfurt:find-word word
                                   :source-lang language
                                   :filters (when kind `(:lexical-category ,kind))))
         (pronunciations ()))
    (dolist (lexical-entry (oxenfurt:lexical-entries word))
      (dolist (pronunciation (oxenfurt:pronunciations lexical-entry))
        (push (oxenfurt:phonetic-spelling pronunciation) pronunciations)))
    (nreverse (delete-duplicates pronunciations :test #'string-equal))))

(defun etymologies (word &key (language "en") kind)
  (let* ((word (oxenfurt:find-word word
                                   :source-lang language
                                   :filters (when kind `(:lexical-category ,kind))))
         (etymologies ()))
    (dolist (lexical-entry (oxenfurt:lexical-entries word))
      (dolist (entry (oxenfurt:entries lexical-entry))
        (dolist (etymology (oxenfurt:etymologies entry))
          (push etymology etymologies))))
    (nreverse (delete-duplicates etymologies :test #'string-equal))))

(defun description (word &key (language "en") kind)
  (with-output-to-string (out)
    (let* ((word (oxenfurt:find-word word
                                     :source-lang language
                                     :filters (when kind `(:lexical-category ,kind))))
           (lex (first (oxenfurt:lexical-entries word))))
      (format out "~a /~a/ " (oxenfurt:text lex) (oxenfurt:phonetic-spelling (first (oxenfurt:pronunciations lex))))
      (dolist (lexical-entry (oxenfurt:lexical-entries word))
        (format out "~&(~(~a~)): ~a"
                (oxenfurt:lexical-category lexical-entry)
                (first (oxenfurt:definitions (first (oxenfurt:senses (first (oxenfurt:entries lexical-entry)))))))))))

(defmacro with-lookup-handling ((c ev word) &body body)
  (let ((err (gensym "ERR")))
    `(handler-case
         (maiden-storage:with-storage (,c)
           (let ((oxenfurt:*app-id* (maiden-storage:value :app-id))
                 (oxenfurt:*app-key* (maiden-storage:value :app-key)))
             ,@body))
       (oxenfurt:api-call-failed (,err)
         (if (= 404 (oxenfurt:result ,err))
             (reply ,ev "~s could not be found in the dictionary." ,word)
             (reply ,ev "Failed to contact the Oxford dictionary."))))))

(define-command (dictionary set-keys) (c ev &key app-id app-key)
  :command "set oxford keys"
  :advice (not public)
  (maiden-storage:with-storage (c)
    (when app-id (setf (maiden-storage:value :app-id) app-id))
    (when app-key (setf (maiden-storage:value :app-key) app-key))
    (reply ev "The Oxford API keys have been updated.")))

(define-command (dictionary description) (c ev word &key (language "en") kind)
  :command "define"
  (with-lookup-handling (c ev word)
    (reply ev "~a" (description word :language language :kind kind))))

(define-command (dictionary synonyms) (c ev word &key (language "en"))
  :command "synonyms of"
  (with-lookup-handling (c ev word)
    (reply ev "~{~a~^, ~}" (synonyms word :language language))))

(define-command (dictionary antonyms) (c ev word &key (language "en"))
  :command "antonyms of"
  (with-lookup-handling (c ev word)
    (reply ev "~{~a~^, ~}" (antonyms word :language language))))

(define-command (dictionary etymology) (c ev word &key (language "en") kind)
  :command "etymology of"
  (with-lookup-handling (c ev word)
    (reply ev "~{~a~^~%~}" (etymologies word :language language :kind kind))))