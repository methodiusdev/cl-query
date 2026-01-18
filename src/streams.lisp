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

(defconstant empty-stream nil
  "The emtpy stream constant")

(defun stream-null? (stream) (null stream))

(defun stream-car (stream)
  (if (stream-null? stream)
      (error "STREAM-CAR: stream is empty")
      (car stream)))

(defun stream-cdr (stream)
  (if (stream-null? stream)
      (error "STREAM-CDR: stream is empty")
      (force (cdr stream))))

(defun create-stream (&rest elements)
  "Returns a newly allocated stream with ELEMENTS or empty stream."
  (if (null elements)
      empty-stream
      (cons-stream (car elements)
		   (apply #'create-stream (cdr elements)))))

(defun list->stream (list)
  )

(defun stream->list (stream)
  )

(defun vector->stream (stream)
  )
