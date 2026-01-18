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

(defun integers-from (n)
  "Creates and returns an infinite stream of integers starting from n"
  (cons-stream n (integers-from (1+ n))))

(defun list->stream (list)
  "Converts LIST to a stream."
  (apply #'create-stream list))

(defun stream->list (stream &optional (max-elements nil))
  "Converts STREAM to a list.
   If MAX-ELEMENTS specified - convert at most that many elements.
   WARN: This will hang on infinite streams without MAX-ELEMENTS."
  (cond ((stream-null? stream) '())
	((and max-elements (<= max-elements 0)) '())
	(t (cons (stream-car stream)
		 (stream->list (stream-cdr stream)
			       (if max-elements (1- max-elements) nil))))))

(defun vector->stream (vector)
  "Converts VECTOR to a stream."
  (labels ((vec->stream (index)
	     (if (>= index (length vector))
		 empty-stream
		 (cons-stream (aref vector index)
			      (vec->stream (1+ index))))))
    (vec->stream 0)))
