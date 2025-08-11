(pushnew :deeds-no-startup *features*)

(asdf:defsystem maiden
  :version "3.1.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A modern and extensible chat bot framework."
  :homepage "https://shinmera.com/docs/maiden/"
  :bug-tracker "https://shinmera.com/project/maiden/issues"
  :source-control (:git "https://shinmera.com/project/maiden.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "conditions")
               (:file "event")
               (:file "standard-events")
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
               :lambda-fiddle
               :documentation-utils
               :trivial-indent))
