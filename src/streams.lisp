(in-package :cl-query)

(defmacro delay (expr)
  `(lambda () ,expr))

(defun force (delayed-expr)
  (funcall delayed-expr))

(defmacro cons-stream (head tail)
  `(cons ,head (delay ,tail)))

(defun stream-car (stream) (car stream))
(defun stream-cdr (stream) (force (cdr stream)))
(defun stream-null? (stream) (null stream))

