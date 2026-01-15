(in-package #:cl-query/tests)

(5am:def-suite streams-tests
  :description "Test suite for streams.")

(5am:in-suite streams-tests)

(5am:test delay-and-force
  "Test that delay actually delays evaluation and force executes it."
  (let* ((evaluated nil)
	 (delayed (delay (setf evaluated t))))
    ;; Should not be yet evaluated
    (5am:is (null evaluated))
    ;; Now force evaluation
    (force delayed)
    ;; Evaluated now
    (5am:is (not (null evaluated)))))

(5am:test cons-stream-basics
  (let ((s (cons-stream 1 (cons-stream 2 '()))))
    (5am:is (= 1 (stream-car s)))
    (5am:is (= 2 (stream-car (stream-cdr s))))
    (5am:is (stream-null? (stream-cdr (stream-cdr s))))))

(5am:test lazy-evaluation
  "Test that stream tail is not evaluated until needed."
  (let* ((tail-evaluated? nil)
	 (s (cons-stream 1 (setf tail-evaluated? t))))
    ;; Stream is created, but no one touched its tail
    (5am:is (null tail-evaluated?))
    ;; Touch the head. Tail should not materialize
    (stream-car s)
    (5am:is (null tail-evaluated?))
    ;; Touch the tail! Check if cdr evals
    (stream-cdr s)
    (5am:is (not (null tail-evaluated?)))))
