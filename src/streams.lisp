(in-package :cl-query)

(defmacro delay (expr)
  "Delays evaluation of EXPR by wrapping it in a lambda.
   Returns a promise (function with no arguments)."
  `(lambda () ,expr))

(defun force (delayed-expr)
  "Forces evaluation of a delayed expression.
   DELAYED-EXPR should be a promise created by DELAY."
  (funcall delayed-expr))

(defmacro cons-stream (head tail)
  "Constructs a stream with HEAD (evaluated) and TAIL (delayed)."
  `(cons ,head (delay ,tail)))

(defun stream-car (stream) (car stream))
(defun stream-cdr (stream) (force (cdr stream)))
(defun stream-null? (stream) (null stream))

