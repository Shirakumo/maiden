#|
 This file is a part of Maiden
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)

(pushnew :deeds-no-startup *features*)

(asdf:defsystem maiden
  :version "3.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A modern and extensible chat bot framework."
  :homepage "https://github.com/Shinmera/maiden"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "conditions")
               (:file "event")
               (:file "entity")
               (:file "consumer")
               (:file "core")
               (:file "agent")
               (:file "client")
               (:file "documentation"))
  :depends-on (:deeds
               :verbose
               :trivial-garbage
               :bordeaux-threads
               :closer-mop
               :uuid
               :form-fiddle
               :documentation-utils))
