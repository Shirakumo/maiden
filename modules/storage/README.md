## About Maiden-Storage
This offers convenient persistent and thread-safe storage for Maiden components.

## How To
For the most part you will want to refer to [Ubiquitous](http://shinmera.github.io/ubiquitous/) for docs, since that is the underlying system used for persistence and storage management.

Time for a simple example.

     (maiden:define-consumer tester () ())
     
The `with-storage` macro takes care of establishing the proper context where the storage is accessible through `value`.
     
     (maiden:define-instruction (tester write-storage) (c ev field value)
       (maiden-storage:with-storage (c)
         (setf (maiden-storage:value field) value)))
     
     (maiden:define-query (tester read-storage) (c ev field)
       (maiden-storage:with-storage (c)
         (maiden-storage:value field)))

Let's construct a test case.

     (defvar *core* (maiden:make-simple-core 'tester))

     (write-storage *core* :test "Hey!")
     (read-storage *core* :test)
     
As apparent, the access works as intended. Just to make sure things are also persisted to disk, let's check that out.

     (alexandria:read-file-into-string (ubiquitous:designator-pathname 'tester :lisp))

And sure enough the file is filled with the info we stored.
