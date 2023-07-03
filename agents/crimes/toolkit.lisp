(in-package #:org.shirakumo.maiden.agents.crimes)

(defmacro rotatef-list (place)
  (let ((val (gensym "VALUE")))
    `(let ((,val ,place))
       (when ,val
         (setf (cdr (last ,val)) (list (car ,val)))
         (setf ,place (cdr ,val))))))

(defmacro push-to-end (item place)
  (let ((value (gensym "VALUE")))
    `(let ((,value ,place))
       (if ,value
           (setf (cdr (last ,value)) (list ,item))
           (setf ,place (list ,item))))))
