(defsystem "tomlet"
  :description "TOML parser for Common Lisp"
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("cl-ppcre")
  :pathname "src"
  :serial t
  :components
  ((:file "float-utils")
   (:file "types")
   (:file "lexer")
   (:file "parser"))
  :in-order-to ((test-op (test-op "tomlet/tests"))))

(defsystem "tomlet/tests"
  :description "Test suite for tomlet"
  :depends-on ("tomlet"
               "rove"
               "com.inuoe.jzon")
  :pathname "tests"
  :serial t
  :components
  ((:file "lexer")
   (:file "parser")
   (:file "suite"))
  :perform (test-op (o c) (symbol-call :rove '#:run :tomlet/tests)))
