(defpackage #:cl-query
  (:use #:common-lisp)
  (:nicknames #:q)
  (:export
   #:delay
   #:force
   #:cons-stream
   #:stream-car
   #:stream-cdr
   #:empty-stream
   #:stream-null?))
