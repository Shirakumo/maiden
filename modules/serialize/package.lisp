(in-package #:maiden-user)
(defpackage #:maiden-serialize
  (:nicknames #:org.shirakumo.maiden.modules.serialize)
  (:use #:cl #:maiden)
  (:export
   #:serialize
   #:deserialize))

(use-package '#:maiden-serialize '#:maiden-user)
