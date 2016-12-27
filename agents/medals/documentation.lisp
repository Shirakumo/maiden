#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.medals)

(docs:define-docs
  (type medals
    "This implements a simple 'medal' system, where users can be awarded random medals.")

  (command show
    "Displays the awarded medals for a user or yourself.")

  (command award
    "Award medals to a user. The medals can be literally anything.")

  (command take
    "Take away medals from a user."))
