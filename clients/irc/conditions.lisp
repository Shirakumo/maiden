(in-package #:org.shirakumo.maiden.clients.irc)

(define-condition message-too-long-warning (message-condition client-condition warning)
  ()
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Message exceeds length limit of ~a." *send-length-limit*))))
