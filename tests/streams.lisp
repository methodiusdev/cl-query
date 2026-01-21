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
  "Test basic cons stream construction."
  (let ((s (cons-stream 1 (cons-stream 2 empty-stream))))
    (5am:is (= 1 (stream-car s)))
    (5am:is (= 2 (stream-car (stream-cdr s))))
    (5am:is (stream-null? (stream-cdr (stream-cdr s))))))

(5am:test stream-creation
  "Test stream creation."
  (let ((s (create-stream 'a 'b)))
    (5am:is (eq 'a (stream-car s)))
    (5am:is (eq 'b (stream-car (stream-cdr s))))
    (5am:is (stream-null? (stream-cdr (stream-cdr s))))))

(5am:test empty-stream-test
  "Test empty stream behavior."
  (5am:is (stream-null? empty-stream))
  (5am:signals error (stream-car empty-stream))
  (5am:signals error (stream-cdr empty-stream)))

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

(5am:test infinite-integers-from
  "Test infinite stream of integers."
  (let ((s (integers-from 100)))
    (5am:is (= 100 (stream-car s)))
    (5am:is (= 101 (stream-car (stream-cdr s))))))

(5am:test list-to-stream-conversion
  "Test converting lists to streams."
  (let ((s (list->stream '(:a :b :c :d :e))))
    (5am:is (eq :a (stream-car s)))
    (5am:is (eq :b (stream-car (stream-cdr s))))))

(5am:test streams-to-list-conversion
  "Test converting streams to lists."
  (let ((s (create-stream 1 2 3 4 5)))
    (5am:is (equal '(1 2 3 4 5) (stream->list s)))))

(5am:test vector-to-stream-conversion
  "Test converting vectors to streams."
  (let ((s (vector->stream #(1 2 3 4 5))))
    (5am:is (= 1 (stream-car s)))
    (5am:is (= 2 (stream-car (stream-cdr s))))))

(5am:test stream-ref-access
  "Test random access to stream elements."
  (let ((s (list->stream '(a b c d e))))
    (5am:is (eq 'a (stream-ref s 0)))
    (5am:is (eq 'c (stream-ref s 2)))
    (5am:is (eq 'e (stream-ref s 4)))
    (5am:signals error (stream-ref s 10))))

(5am:test stream-length-computation
  "Test stream length calculation."
  (5am:is (= 0 (stream-length empty-stream)))
  (5am:is (= 5 (stream-length (list->stream '(1 2 3 4 5)))))
  ;; Test max parameter with infinite stream
  (5am:is (= 100 (stream-length (integers-from 0) 100))))

(5am:test stream-take-operation
  "Test taking N elements from stream."
  (let ((s (integers-from 0)))
    (5am:is (equal '(0 1 2) (stream->list (stream-take 3 s))))
    (5am:is (equal '() (stream->list (stream-take 0 s))))
    (5am:is (equal '() (stream->list (stream-take -5 s))))))

(5am:test stream-drop-operation
  "Test dropping N elements from stream."
  (let ((s (list->stream '(1 2 3 4 5))))
    (5am:is (equal '(3 4 5) (stream->list (stream-drop 2 s))))
    (5am:is (equal '(1 2 3 4 5) (stream->list (stream-drop 0 s))))
    (5am:is (stream-null? (stream-drop 10 s)))))

(5am:test stream-map-operation
  "Test mapping function over stream."
  (let ((s (list->stream '(1 2 3 4 5))))
    (let ((doubled (stream-map (lambda (x) (* x 2)) s)))
      (5am:is (equal '(2 4 6 8 10) (stream->list doubled)))))
  ;; Test with infinite stream
  (let ((squares (stream-map (lambda (x) (* x x)) (integers-from 0))))
    (5am:is (equal '(0 1 4 9 16) (stream->list (stream-take 5 squares))))))

(5am:test stream-filter-operation
  "Test filtering stream elements."
  (let ((s (integers-from 0)))
    (let ((evens (stream-filter #'evenp s)))
      (5am:is (equal '(0 2 4 6 8) (stream->list (stream-take 5 evens))))))
  ;; Test filter that removes everything
  (let ((s (list->stream '(1 3 5 7))))
    (5am:is (stream-null? (stream-filter #'evenp s)))))

(5am:test stream-fold-operation
  "Test folding/reducing stream."
  (let ((s (list->stream '(1 2 3 4 5))))
    ;; Sum
    (5am:is (= 15 (stream-fold #'+ 0 s)))
    ;; Product
    (5am:is (= 120 (stream-fold #'* 1 s)))
    ;; List reversal
    (5am:is (equal '(5 4 3 2 1) 
               (stream-fold (lambda (acc x) (cons x acc)) '() s))))
  ;; Test with max-elements on infinite stream
  (let ((s (integers-from 1)))
    (5am:is (= 55 (stream-fold #'+ 0 s 10))))) ; sum of 1..10

(5am:test composition-map-filter
  "Test composing map and filter operations."
  (let* ((s (integers-from 0))
         (evens (stream-filter #'evenp s))
         (squares (stream-map (lambda (x) (* x x)) evens)))
    ;; Squares of even numbers: 0, 4, 16, 36, 64, ...
    (5am:is (equal '(0 4 16 36 64) (stream->list (stream-take 5 squares))))))

(5am:test composition-multiple-filters
  "Test multiple filter compositions."
  (let* ((s (integers-from 1))
         ;; Numbers divisible by 2
         (div2 (stream-filter (lambda (x) (zerop (mod x 2))) s))
         ;; Numbers divisible by 2 AND 3 (i.e., divisible by 6)
         (div6 (stream-filter (lambda (x) (zerop (mod x 3))) div2)))
    (5am:is (equal '(6 12 18 24 30) (stream->list (stream-take 5 div6))))))

(5am:test sieve-of-eratosthenes
  "Test classic Sieve of Eratosthenes for prime numbers."
  (labels ((sieve (s)
             (cons-stream 
              (stream-car s)
              (sieve (stream-filter 
                      (lambda (x) 
                        (not (zerop (mod x (stream-car s)))))
                      (stream-cdr s))))))
    (let ((primes (sieve (integers-from 2))))
      (5am:is (equal '(2 3 5 7 11 13 17 19 23 29)
                     (stream->list (stream-take 10 primes)))))))

(5am:test fibonacci-stream
  "Test generating Fibonacci sequence as stream."
  (labels ((fibs (a b)
             (cons-stream a (fibs b (+ a b)))))
    (let ((fib-stream (fibs 0 1)))
      (5am:is (equal '(0 1 1 2 3 5 8 13 21 34)
                     (stream->list (stream-take 10 fib-stream)))))))

(5am:test large-stream-handling
  "Test that large streams don't blow up memory (spot check)."
  ;; This should work without consuming much memory
  (let ((s (integers-from 0)))
    (5am:is (= 1000000 (stream-ref s 1000000)))))

(5am:test stream-of-streams
  "Test streams containing other streams."
  (let* ((s1 (list->stream '(1 2)))
         (s2 (list->stream '(3 4)))
         (ss (cons-stream s1 (cons-stream s2 empty-stream))))
    (5am:is (equal '(1 2) (stream->list (stream-car ss))))
    (5am:is (equal '(3 4) (stream->list (stream-car (stream-cdr ss)))))))

(5am:test empty-operations
  "Test operations on empty streams."
  (5am:is (= 0 (stream-length empty-stream)))
  (5am:is (stream-null? (stream-map #'identity empty-stream)))
  (5am:is (stream-null? (stream-filter #'identity empty-stream)))
  (5am:is (= 42 (stream-fold #'+ 42 empty-stream))))
