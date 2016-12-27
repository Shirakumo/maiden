#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.agents.time)

(docs:define-docs
  (type time
    "This agent provides all sorts of time, date, and timezone utilities.")

  (function timezone-location
    "Shows the time zone of a location.")

  (function time-dwim
    "With no argument, the time for the requester is shown, if their location is known. Otherwise, the time for the given user or location is shown, if possible.")

  (function time-location
    "Show the time for a location.")

  (function time-user
    "Show the time for a user. Note that this only works if the location of the user is known.")

  (function time-between
    "Show the time difference between two locations.")

  (function time-between-users
    "Show the time difference between two users. Note that this only works if the location of the user is known."))
