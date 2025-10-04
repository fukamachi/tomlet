(defsystem "tomlex"
  :description "TOML parser for Common Lisp"
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ()
  :pathname "src"
  :serial t
  :components
  ((:file "types")
   (:file "lexer")
   (:file "parser"))
  :in-order-to ((test-op (test-op "tomlex/tests"))))

(defsystem "tomlex/tests"
  :description "Test suite for tomlex"
  :depends-on ("tomlex"
               "rove"
               "com.inuoe.jzon")
  :pathname "tests"
  :serial t
  :components
  ((:file "lexer")
   (:file "parser")
   (:file "suite"))
  :perform (test-op (o c) (symbol-call :rove '#:run :tomlex/tests)))
