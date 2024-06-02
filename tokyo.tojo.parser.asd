(defsystem #:tokyo.tojo.parser
  :description "LL(1) parser for Coalton"
  :author "Masaya Tojo"
  :license  "MIT"
  :version "0.0.0"
  :depends-on (#:coalton
               #:alexandria
               #:tokyo.tojo.iterable)
  :serial t
  :pathname "src/"
  :components ((:file "private/output-stream")
               (:file "port")
               (:file "parser")))
