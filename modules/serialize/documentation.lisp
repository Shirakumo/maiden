#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.modules.serialize)

;; serialize.lisp
(docs:define-docs
  (variable *finders*
    "Association list tying a \"type\" to a function that can resolve an instance of that \"type\" from an ID.

See FIND-INSTANCE")

  (variable *event-code*
    "The CL-STORE code assigned to events.")

  (variable *core-code*
    "The CL-STORE code assigned to cores.")

  (variable *consumer-code*
    "The CL-STORE code assigned to consumers.")

  (variable *footer-buffer*
    "The buffer to store the gzip footer in.")

  (function find-instance
    "Attempts to resolve the ID of the given \"type\" to an instance using the finders.

See *FINDERS*")

  (function serialize
    "Serialize the given object to the given target stream.

The data is serialized, GZIP compressed, and then put to the stream.")

  (function deserialize
    "Deserialize an object from the given stream.

FINDERS must be a suitable alist that can find COREs and CONSUMERs by their ID.

See FIND-INSTANCE"))
