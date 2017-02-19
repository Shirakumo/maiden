(in-package #:cl-user)

(defclass symb-command (staple:symb-function)
  ())

(defmethod staple:symb-function ((symb symb-command))
  (maiden-commands::invoker
   (maiden-commands:command-invoker (staple:symb-symbol symb))))

(defmethod staple:symb-documentation ((symb symb-command))
  (documentation (staple:symb-symbol symb) 'maiden-commands:command))

(defmethod staple:symb-arguments ((symb symb-command))
  (maiden-commands::lambda-list
   (maiden-commands:command-invoker (staple:symb-symbol symb))))

(defmethod staple:symb-type-order ((symb (eql 'symb-command)))
  (1+ (staple:symb-type-order 'symb-function)))

(staple:define-simple-converter symb-command maiden-commands:command-invoker)
