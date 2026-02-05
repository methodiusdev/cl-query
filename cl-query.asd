(asdf:defsystem #:cl-query
  :description "LINQ-like query library for Common Lisp"
  :author "Kyrylo Zhulin methodius.dev@gmail.com"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:split-sequence)
  :components ((:module "src"
		:components
		((:file "package")
		 (:file "streams")
		 (:file "query")))))

(asdf:defsystem #:cl-query/tests
  :description "Tests for cl-query"
  :author "Kyrylo Zhulin methodius.dev@gmail.com"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-query
	       #:fiveam)
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "streams")))))
