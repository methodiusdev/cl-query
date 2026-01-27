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
   #:stream-null?
   #:create-stream
   #:list->stream
   #:stream->list
   #:vector->stream
   #:integers-from
   #:stream-length
   #:stream-ref
   #:stream-filter
   #:stream-map
   #:stream-take
   #:stream-drop
   #:stream-fold
   #:stream-distinct
   #:query
   #:from
   #:select
   #:where
   #:take
   #:skip
   #:distinct
   #:->))
