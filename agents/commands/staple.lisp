(in-package #:cl-user)

(defclass symb-command (staple:symb-function)
  ())

(defmethod staple:symb-name ((symb symb-command))
  (maiden-commands:prefix
   (maiden-commands:command-invoker (slot-value symb 'symbol))))

(defmethod staple:symb-function ((symb symb-command))
  (maiden-commands::invoker
   (maiden-commands:command-invoker (slot-value symb 'symbol))))

(defmethod staple:symb-documentation ((symb symb-command))
  (documentation (slot-value symb 'symbol) 'maiden-commands:command))

(defmethod staple:symb-arguments ((symb symb-command))
  (maiden-commands::lambda-list
   (maiden-commands:command-invoker (slot-value symb 'symbol))))

(defmethod staple:symb-type-order ((symb (eql 'symb-command)))
  (1+ (staple:symb-type-order 'symb-function)))

(staple:define-simple-converter symb-command maiden-commands:command-invoker)
