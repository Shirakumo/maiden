#|
 This file is a part of Maiden
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.maiden.clients.irc)

(define-condition message-too-long-warning (message-condition client-condition warning)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Message exceeds length limit of ~a." *send-length-limit*))))
