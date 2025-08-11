(asdf:defsystem maiden-weather
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Simple weather data access through forecast.io"
  :homepage "https://shinmera.com/docs/maiden/"
  :bug-tracker "https://shinmera.com/project/maiden/issues"
  :source-control (:git "https://shinmera.com/project/maiden.git")
  :serial T
  :components ((:file "package")
               (:file "weather")
               (:file "documentation"))
  :depends-on (:maiden-commands
               :maiden-storage
               :maiden-location
               :maiden-api-access
               :maiden-client-entities
               :local-time))
