#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.markov)

(defun read-vec-using (reader stream)
  (let* ((num (fast-io:read32-be stream))
         (vec (make-array num :adjustable T :fill-pointer T)))
    (dotimes (i num vec)
      (setf (aref vec i) (funcall reader stream)))))

(defun read-word (stream)
  (let ((vec (make-array 5 :element-type '(unsigned-byte 8) :initial-element 0 :adjustable T :fill-pointer 0)))
    (loop for char = (fast-io:readu8 stream)
          until (= 0 char)
          do (vector-push-extend char vec))
    (babel:octets-to-string vec :encoding :utf-8)))

(defun write-word (word stream)
  (fast-io:fast-write-sequence
   (babel:string-to-octets word :encoding :utf-8) stream)
  (fast-io:writeu8 0 stream))

(defun read-words (stream)
  (let ((i -1) (map (make-hash-table :test 'equal)))
    (values (read-vec-using
             (lambda (s)
               (let ((word (read-word s)))
                 (setf (gethash word map) (incf i))
                 word)) stream)
            map)))

(defun write-words (words stream)
  (fast-io:write32-be (length words) stream)
  (loop for word across words do (write-word word stream)))

(defun read-ref (stream)
  (fast-io:read32-be stream))

(defun write-ref (ref stream)
  (fast-io:write32-be ref stream))

(defun read-chain (stream)
  (values (read-ref stream)
          (read-ref stream)
          (read-vec-using #'read-ref stream)))

(defun write-chain (first second refs stream)
  (write-ref first stream)
  (write-ref second stream)
  (fast-io:write32-be (length refs) stream)
  (loop for ref across refs do (write-ref ref stream)))

(defun read-chains (stream)
  (let* ((num (fast-io:read32-be stream))
         (map (make-hash-table :test 'eql :size num)))
    ;; It isn't as simple as looping NUM times. We need to loop NUM times
    ;; but only count when FIRST changes, as multiple "lines" for each
    ;; FIRST item may be in the file.
    (loop with prev = NIL
          with i = 0
          do (multiple-value-bind (first second refs) (read-chain stream)
               (unless (eql first prev) (incf i) (setf prev first))
               (setf (gethash second
                              (or (gethash first map)
                                  (setf (gethash first map)
                                        (make-hash-table :test 'eql))))
                     refs))
          until (= i num))
    map))

(defun write-chains (chains stream)
  (fast-io:write32-be (hash-table-count chains) stream)
  (loop for first being the hash-keys of chains
        for map being the hash-values of chains
        do (loop for second being the hash-keys of map
                 for refs being the hash-values of map
                 do (write-chain first second refs stream))))

(defun read-generator (source)
  (etypecase source
    (string
     (read-generator (pathname source)))
    (pathname
     (with-open-file (stream source :direction :input
                                    :element-type '(unsigned-byte 8)
                                    :if-does-not-exist NIL)
       (if stream
           (read-generator stream)
           (make-instance 'generator))))
    (stream
     (fast-io:with-fast-input (buffer NIL source)
       (read-generator buffer)))
    (fast-io::input-buffer
     (let ((generator (make-instance 'generator)))
       (multiple-value-bind (index refs) (read-words source)
         (setf (words generator) index)
         (setf (word-map generator) refs)
         (setf (chains generator) (read-chains source)))
       generator))))

(defun write-generator (generator target)
  (etypecase target
    (string
     (write-generator generator (pathname target)))
    (pathname
     (with-open-file (stream target :direction :output
                                    :element-type '(unsigned-byte 8)
                                    :if-exists :supersede)
       (write-generator generator stream)))
    (stream
     (fast-io:with-fast-output (buffer target)
       (write-generator generator buffer)))
    (fast-io::output-buffer
     (write-words (words generator) target)
     (write-chains (chains generator) target)))
  target)
