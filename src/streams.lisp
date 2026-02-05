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

(defun stream-length (stream &optional max)
  "Returns the length of STREAM.
   If MAX is specified, counts at most MAX elements.
   WARN: This will hang on infinite streams without MAX!"
  (labels ((length-iter (s count)
	     (cond ((and max (<= max count)) count)
		   ((stream-null? s) count)
		   (t (length-iter (stream-cdr s) (1+ count))))))
    (length-iter stream 0)))

(defun stream-ref (stream n)
  "Returns the N-th elements of STREAM (0-indexed).
   Signals an error if stream has fever than N+1 elements."
  (cond
    ((stream-null? stream)
     ; make this work properly
     (error "STREAM-REF: Index ~a out of bounds" n))
    ((= n 0) (stream-car stream))
    (t (stream-ref (stream-cdr stream) (1- n)))))

;;WHERE
(defun stream-filter (predicate stream)
  "Returns a stream containing only elements of STREAM that satisfy PREDICATE.
   PREDICATE should be a function of one argument returning boolean."
  (cond ((stream-null? stream) empty-stream)
	((funcall predicate (stream-car stream))
	 (cons-stream (stream-car stream)
		      (stream-filter predicate (stream-cdr stream))))
	(t (stream-filter predicate (stream-cdr stream)))))
;;SELECT
(defun stream-map (fn stream)
  "Applies FN to each element of STREAM, returning a new stream.
   FN should be a function of one argument."
  (if (stream-null? stream)
      empty-stream
      (cons-stream (funcall fn (stream-car stream))
		   (stream-map fn (stream-cdr stream)))))
;;TAKE
(defun stream-take (n stream)
  "Returns a new stream containing the first N elements of STREAM."
  (if (or (<= n 0) (stream-null? stream))
      empty-stream
      (cons-stream (stream-car stream)
		   (stream-take (1- n) (stream-cdr stream)))))
;;SKIP
(defun stream-drop (n stream)
  "Returns a stream with the first N elements removed."
  (cond ((<= n 0) stream)
	((stream-null? stream) empty-stream)
	(t (stream-drop (1- n) (stream-cdr stream)))))
;;AGGREGATE
(defun stream-fold (fn init stream &optional (max-elements nil))
  "Folds STREAM using FN and initial value INIT (left fold).
   FN should be a function of two arguments: (accumulator element).
   If MAX-ELEMENTS is specified, processes at most that many elements.
   WARN: This will hang on infinite streams without MAX-ELEMENTS!"
  (if (or (and max-elements (<= max-elements 0))
	  (stream-null? stream))
      init
      (stream-fold fn
		   (funcall fn init (stream-car stream))
		   (stream-cdr stream)
		   (if max-elements (1- max-elements) nil))))
;;DISTINCT
(defun stream-distinct (stream &key (test #'eql))
  "Returns a stream with duplicate elements removed.
   TEST is the equality predicate (default: #'eql).
   Note: This maintains a seen-set in memory, so it's not suitable for
   truly infinite streams with infinite distinct values."
  (labels ((distinct-helper (s seen)
             (cond
               ((stream-null? s) empty-stream)
               ((member (stream-car s) seen :test test)
                ;; Already seen, skip it
                (distinct-helper (stream-cdr s) seen))
               (t
                ;; New element, add to seen and include in output
                (cons-stream (stream-car s)
                            (distinct-helper (stream-cdr s)
                                           (cons (stream-car s) seen)))))))
    (distinct-helper stream '())))

(defstruct group
  "Represents a group with a key and stream of values"
  key
  values)

;;; TODO: Make an optimized version when used with ORDER-BY
(defun stream-group-by (key-fn stream &key (test #'eql))
  "Groups elements of STREAM by the result of KEY-FN.
   Returns a stream of GROUP structures, each containing a key and a stream of values.
   TEST is the equality predicate for comparing keys (default: #'eql).

   Note: This is NOT fully lazy - it must materialize the entire input stream
   to group elements."
  ;; First, materialize the stream into a list
  ;; (yes, this for sure needs to be optimized later)
  (let ((elements (stream->list stream)))
    (if (null elements)
        empty-stream
        (let ((groups (make-hash-table :test test)))
          ;; Build hash table of key -> list of values
          (dolist (elem elements)
            (let ((key (funcall key-fn elem)))
              (push elem (gethash key groups))))
          ;; Convert hash table to stream of groups
          (let ((group-list '()))
            (maphash (lambda (key values)
                       (push (make-group
                              :key key
                              :values (list->stream (reverse values)))
                             group-list))
                     groups)
            (list->stream group-list))))))
