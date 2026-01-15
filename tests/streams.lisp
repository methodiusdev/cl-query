(in-package #:cl-query/tests)

(5am:def-suite streams-tests
  :description "Test suite for streams.")

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
