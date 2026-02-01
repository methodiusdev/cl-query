(in-package :cl-query)

(defclass query ()
  ((source-stream
    :accessor source-stream
    :initarg :source
    :documentation "The underlying stream.")
   ;; TODO: consider metadata. how should it look like?
   ;; maybe will need a rework when provider patterns introduced.
   )
  (:documentation "Represents a query over a stream.
   Provides an interface for chaining operations."))

(defun from (source)
  "Creates a query from SOURCE. SOURCE can be: list, vecror, stream, query."
  (etypecase source
    (query source)
    (vector (make-instance 'query :source (vector->stream source)))
    ;; In this implementation a stream is
    ;; basically a cons cell with lambda in cdr.
    (cons
     (if (functionp (cdr source))
	 (make-instance 'query :source source)
	 ;; Otherwise, treat as a list
	 (make-instance 'query :source (list->stream source))))))

(defgeneric where (query predicate)
  (:documentation "Filters elements that satisfy PREDICATE."))

(defmethod where ((q query) predicate)
  (make-instance 'query
                 :source (stream-filter predicate (source-stream q))))

(defgeneric select (query selector)
  (:documentation "Projects each element using SELECTOR function."))

(defmethod select ((q query) selector)
  (make-instance 'query
		 :source (stream-map selector (source-stream q))))

(defgeneric take (query n)
  (:documentation "Takes first N elements."))

(defmethod take ((q query) n)
  (make-instance 'query
                 :source (stream-take n (source-stream q))))

(defgeneric skip (query n)
  (:documentation "Skips first N elements."))

(defmethod skip ((q query) n)
  (make-instance 'query
                 :source (stream-drop n (source-stream q))))

(defgeneric distinct (query &key test)
  (:documentation "Removes duplicate elements."))

(defmethod distinct ((q query) &key (test #'eql))
  (make-instance 'query
                 :source (stream-distinct (source-stream q) :test test)))

(defgeneric aggregate (query fn &optional initial-value max-elements)
  (:documentation "Aggregates elements using FN."))

(defmethod aggregate ((q query) fn &optional (initial-value 0) max-elements)
  (stream-fold fn initial-value (source-stream q) max-elements))

(defgeneric group-by (query key-fn &key test)
  (:documentation "Groups elements by KEY-FN."))

(defmethod group-by ((q query) key-fn &key (test #'eql))
  (make-instance 'query
                 :source (stream-group-by key-fn (source-stream q) :test test)))

(defgeneric to-list (query &optional max-elements)
  (:documentation "Materializes the query into a list."))

(defmethod to-list ((q query) &optional max-elements)
  (stream->list (source-stream q) max-elements))

(defmacro -> (initial-form &rest forms)
  "Threads the result of each form as the first argument to the next form."
  (if forms
      (let ((threaded (if (listp (car forms))
                          `(,(caar forms) ,initial-form ,@(cdar forms))
                          `(,(car forms) ,initial-form))))
        `(-> ,threaded ,@(cdr forms)))
      initial-form))

#+(or)
(-> (from '(1 2 3 4 5))
    (where (lambda (n) (<= n 3)))
    (skip 1)
    (take 1)
    (select (lambda (n) (sqrt n)))
    (to-list))
