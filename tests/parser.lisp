(defpackage #:tomlet/tests/parser
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:float-utils #:tomlet/float-utils)))
(in-package #:tomlet/tests/parser)

;;; ===========================================================================
;;; Parser Unit Tests
;;; ===========================================================================

(deftest test-empty-input
  (testing "Empty string"
    (let ((result (tomlet:parse "")))
      (ok (hash-table-p result))
      (ok (= (hash-table-count result) 0))))

  (testing "Only whitespace and newlines"
    (let ((result (tomlet:parse (format nil "~%  ~%  ~%"))))
      (ok (hash-table-p result))
      (ok (= (hash-table-count result) 0)))))

(deftest test-boolean-values
  (testing "Boolean true"
    (let ((result (tomlet:parse "key = true")))
      (ok (hash-table-p result))
      (ok (eq (gethash "key" result) t))))

  (testing "Boolean false"
    (let ((result (tomlet:parse "key = false")))
      (ok (hash-table-p result))
      (ok (eq (gethash "key" result) nil))))

  (testing "Multiple booleans"
    (let ((result (tomlet:parse (format nil "a = true~%b = false~%c = true"))))
      (ok (eq (gethash "a" result) t))
      (ok (eq (gethash "b" result) nil))
      (ok (eq (gethash "c" result) t)))))

(deftest test-integer-values
  (testing "Simple positive integer"
    (let ((result (tomlet:parse "answer = 42")))
      (ok (= (gethash "answer" result) 42))))

  (testing "Zero"
    (let ((result (tomlet:parse "zero = 0")))
      (ok (= (gethash "zero" result) 0))))

  (testing "Multiple integers"
    (let ((result (tomlet:parse (format nil "a = 1~%b = 2~%c = 3"))))
      (ok (= (gethash "a" result) 1))
      (ok (= (gethash "b" result) 2))
      (ok (= (gethash "c" result) 3)))))

(deftest test-string-values
  (testing "Simple string"
    (let ((result (tomlet:parse "key = \"value\"")))
      (ok (string= (gethash "key" result) "value"))))

  (testing "Empty string"
    (let ((result (tomlet:parse "empty = \"\"")))
      (ok (string= (gethash "empty" result) ""))))

  (testing "String with spaces"
    (let ((result (tomlet:parse "text = \"hello world\"")))
      (ok (string= (gethash "text" result) "hello world"))))

  (testing "Multiple strings"
    (let ((result (tomlet:parse (format nil "first = \"one\"~%second = \"two\""))))
      (ok (string= (gethash "first" result) "one"))
      (ok (string= (gethash "second" result) "two")))))

(deftest test-literal-strings
  (testing "Simple literal string"
    (let ((result (tomlet:parse "path = 'C:\\\\Users\\\\name'")))
      (ok (string= (gethash "path" result) "C:\\\\Users\\\\name"))))

  (testing "Literal string with quotes"
    (let ((result (tomlet:parse "quote = 'Tom \"Dubs\" Preston-Werner'")))
      (ok (string= (gethash "quote" result) "Tom \"Dubs\" Preston-Werner")))))

(deftest test-mixed-types
  (testing "Mix of different value types"
    (let ((result (tomlet:parse (format nil "name = \"Alice\"~%age = 30~%active = true"))))
      (ok (string= (gethash "name" result) "Alice"))
      (ok (= (gethash "age" result) 30))
      (ok (eq (gethash "active" result) t)))))

(deftest test-bare-keys
  (testing "Simple bare key"
    (let ((result (tomlet:parse "key = 1")))
      (ok (= (gethash "key" result) 1))))

  (testing "Bare key with underscores"
    (let ((result (tomlet:parse "my_key = 2")))
      (ok (= (gethash "my_key" result) 2))))

  (testing "Bare key with hyphens"
    (let ((result (tomlet:parse "my-key = 3")))
      (ok (= (gethash "my-key" result) 3))))

  (testing "Bare key with numbers"
    (let ((result (tomlet:parse "key123 = 4")))
      (ok (= (gethash "key123" result) 4)))))

(deftest test-quoted-keys
  (testing "String key"
    (let ((result (tomlet:parse "\"key\" = 1")))
      (ok (= (gethash "key" result) 1))))

  (testing "String key with spaces"
    (let ((result (tomlet:parse "\"my key\" = 2")))
      (ok (= (gethash "my key" result) 2)))))

(deftest test-comments
  (testing "Comment before key-value"
    (let ((result (tomlet:parse (format nil "# This is a comment~%key = 1"))))
      (ok (= (gethash "key" result) 1))))

  (testing "Inline comment after value"
    (let ((result (tomlet:parse "key = 1 # comment")))
      (ok (= (gethash "key" result) 1))))

  (testing "Multiple comments"
    (let ((result (tomlet:parse (format nil "# Comment 1~%key1 = 1~%# Comment 2~%key2 = 2"))))
      (ok (= (gethash "key1" result) 1))
      (ok (= (gethash "key2" result) 2)))))

(deftest test-whitespace-handling
  (testing "Extra whitespace around equals"
    (let ((result (tomlet:parse "key   =   \"value\"")))
      (ok (string= (gethash "key" result) "value"))))

  (testing "Leading whitespace"
    (let ((result (tomlet:parse "   key = 1")))
      (ok (= (gethash "key" result) 1))))

  (testing "Trailing whitespace"
    (let ((result (tomlet:parse "key = 1   ")))
      (ok (= (gethash "key" result) 1))))

  (testing "Blank lines between key-value pairs"
    (let ((result (tomlet:parse (format nil "a = 1~%~%~%b = 2"))))
      (ok (= (gethash "a" result) 1))
      (ok (= (gethash "b" result) 2)))))

(deftest test-special-float-values
  (testing "Positive infinity"
    (let ((result (tomlet:parse "infinity = inf")))
      (ok (floatp (gethash "infinity" result)))
      (ok (float-utils:float-infinity-p (gethash "infinity" result)))))

  (testing "Not a number"
    (let ((result (tomlet:parse "not_a_number = nan")))
      (ok (floatp (gethash "not_a_number" result))))))

(deftest test-key-value-order
  (testing "Keys are stored in order encountered"
    (let ((result (tomlet:parse (format nil "z = 1~%a = 2~%m = 3"))))
      (ok (= (gethash "z" result) 1))
      (ok (= (gethash "a" result) 2))
      (ok (= (gethash "m" result) 3)))))

(deftest test-error-unterminated-string
  (testing "Unterminated string should error"
    (ok (signals (tomlet:parse "key = \"unterminated")
                 'tomlet:toml-parse-error))))

(deftest test-error-missing-value
  (testing "Missing value after equals should error"
    (ok (signals (tomlet:parse "key = ")
                 'tomlet:toml-parse-error))))

(deftest test-error-missing-equals
  (testing "Missing equals between key and value should error"
    (ok (signals (tomlet:parse "key value")
                 'tomlet:toml-parse-error))))

(deftest test-error-bare-key-as-value
  (testing "Bare key in value position should error"
    (ok (signals (tomlet:parse "key = barevalue")
                 'tomlet:toml-parse-error))))

(deftest test-hash-table-properties
  (testing "Result uses equal test"
    (let ((result (tomlet:parse "key = 1")))
      (ok (eq (hash-table-test result) 'equal))))

  (testing "Keys are strings"
    (let ((result (tomlet:parse "key = 1")))
      (ok (gethash "key" result))
      (ok (not (gethash 'key result))))))

(deftest test-comprehensive-document
  (testing "Complete document with various types"
    (let ((toml (format nil "# Configuration file~%~%\
                             name = \"My Application\"~%\
                             version = 1~%\
                             enabled = true~%~%\
                             # More settings~%\
                             timeout = 30~%\
                             debug = false")))
      (let ((result (tomlet:parse toml)))
        (ok (string= (gethash "name" result) "My Application"))
        (ok (= (gethash "version" result) 1))
        (ok (eq (gethash "enabled" result) t))
        (ok (= (gethash "timeout" result) 30))
        (ok (eq (gethash "debug" result) nil))))))

(deftest test-array-empty
  (testing "Empty array"
    (let ((result (tomlet:parse "arr = []")))
      (ok (vectorp (gethash "arr" result)))
      (ok (= (length (gethash "arr" result)) 0)))))

(deftest test-array-integers
  (testing "Simple integer array"
    (let ((result (tomlet:parse "numbers = [1, 2, 3]")))
      (let ((arr (gethash "numbers" result)))
        (ok (vectorp arr))
        (ok (= (length arr) 3))
        (ok (= (aref arr 0) 1))
        (ok (= (aref arr 1) 2))
        (ok (= (aref arr 2) 3)))))

  (testing "Array with trailing comma"
    (let ((result (tomlet:parse "numbers = [1, 2, 3,]")))
      (let ((arr (gethash "numbers" result)))
        (ok (= (length arr) 3))
        (ok (= (aref arr 0) 1))
        (ok (= (aref arr 1) 2))
        (ok (= (aref arr 2) 3))))))

(deftest test-array-strings
  (testing "String array"
    (let ((result (tomlet:parse "colors = [\"red\", \"green\", \"blue\"]")))
      (let ((arr (gethash "colors" result)))
        (ok (vectorp arr))
        (ok (= (length arr) 3))
        (ok (string= (aref arr 0) "red"))
        (ok (string= (aref arr 1) "green"))
        (ok (string= (aref arr 2) "blue"))))))

(deftest test-array-booleans
  (testing "Boolean array"
    (let ((result (tomlet:parse "flags = [true, false, true]")))
      (let ((arr (gethash "flags" result)))
        (ok (= (length arr) 3))
        (ok (eq (aref arr 0) t))
        (ok (eq (aref arr 1) nil))
        (ok (eq (aref arr 2) t))))))

(deftest test-array-mixed
  (testing "Heterogeneous array"
    (let ((result (tomlet:parse "mixed = [1, \"two\", true]")))
      (let ((arr (gethash "mixed" result)))
        (ok (= (length arr) 3))
        (ok (= (aref arr 0) 1))
        (ok (string= (aref arr 1) "two"))
        (ok (eq (aref arr 2) t))))))

(deftest test-array-nested
  (testing "Nested arrays"
    (let ((result (tomlet:parse "nested = [[1, 2], [3, 4]]")))
      (let ((arr (gethash "nested" result)))
        (ok (vectorp arr))
        (ok (= (length arr) 2))
        (ok (vectorp (aref arr 0)))
        (ok (vectorp (aref arr 1)))
        (ok (= (aref (aref arr 0) 0) 1))
        (ok (= (aref (aref arr 0) 1) 2))
        (ok (= (aref (aref arr 1) 0) 3))
        (ok (= (aref (aref arr 1) 1) 4))))))

(deftest test-array-multiline
  (testing "Array with newlines"
    (let ((result (tomlet:parse (format nil "arr = [~%  1,~%  2,~%  3~%]"))))
      (let ((arr (gethash "arr" result)))
        (ok (= (length arr) 3))
        (ok (= (aref arr 0) 1))
        (ok (= (aref arr 1) 2))
        (ok (= (aref arr 2) 3)))))

  (testing "Array with trailing comma and newline"
    (let ((result (tomlet:parse (format nil "arr = [~%  1,~%  2,~%  3,~%]"))))
      (let ((arr (gethash "arr" result)))
        (ok (= (length arr) 3))))))

(deftest test-array-single-element
  (testing "Single element array"
    (let ((result (tomlet:parse "single = [42]")))
      (let ((arr (gethash "single" result)))
        (ok (= (length arr) 1))
        (ok (= (aref arr 0) 42))))))

(deftest test-inline-table-empty
  (testing "Empty inline table"
    (let ((result (tomlet:parse "empty = {}")))
      (ok (hash-table-p (gethash "empty" result)))
      (ok (= (hash-table-count (gethash "empty" result)) 0)))))

(deftest test-inline-table-basic
  (testing "Simple inline table"
    (let ((result (tomlet:parse "point = {x = 1, y = 2}")))
      (let ((table (gethash "point" result)))
        (ok (hash-table-p table))
        (ok (= (gethash "x" table) 1))
        (ok (= (gethash "y" table) 2)))))

  (testing "Inline table with strings"
    (let ((result (tomlet:parse "person = {name = \"Alice\", age = 30}")))
      (let ((table (gethash "person" result)))
        (ok (string= (gethash "name" table) "Alice"))
        (ok (= (gethash "age" table) 30))))))

(deftest test-inline-table-mixed-values
  (testing "Inline table with mixed value types"
    (let ((result (tomlet:parse "config = {enabled = true, port = 8080, host = \"localhost\"}")))
      (let ((table (gethash "config" result)))
        (ok (eq (gethash "enabled" table) t))
        (ok (= (gethash "port" table) 8080))
        (ok (string= (gethash "host" table) "localhost"))))))

(deftest test-inline-table-nested
  (testing "Inline table with nested inline table"
    (let ((result (tomlet:parse "outer = {inner = {x = 1}}")))
      (let ((outer (gethash "outer" result)))
        (ok (hash-table-p outer))
        (let ((inner (gethash "inner" outer)))
          (ok (hash-table-p inner))
          (ok (= (gethash "x" inner) 1)))))))

(deftest test-inline-table-with-array
  (testing "Inline table with array value"
    (let ((result (tomlet:parse "data = {values = [1, 2, 3]}")))
      (let ((table (gethash "data" result)))
        (let ((arr (gethash "values" table)))
          (ok (vectorp arr))
          (ok (= (length arr) 3))
          (ok (= (aref arr 0) 1))
          (ok (= (aref arr 1) 2))
          (ok (= (aref arr 2) 3)))))))

(deftest test-inline-table-single-pair
  (testing "Inline table with single key-value pair"
    (let ((result (tomlet:parse "single = {key = \"value\"}")))
      (let ((table (gethash "single" result)))
        (ok (= (hash-table-count table) 1))
        (ok (string= (gethash "key" table) "value"))))))

(deftest test-dotted-keys
  (testing "Simple dotted key"
    (let ((result (tomlet:parse "a.b = 1")))
      (ok (hash-table-p (gethash "a" result)))
      (ok (= (gethash "b" (gethash "a" result)) 1))))

  (testing "Nested dotted keys"
    (let ((result (tomlet:parse "a.b.c = 2")))
      (ok (hash-table-p (gethash "a" result)))
      (let ((b-table (gethash "b" (gethash "a" result))))
        (ok (hash-table-p b-table))
        (ok (= (gethash "c" b-table) 2)))))

  (testing "Multiple dotted keys sharing prefix"
    (let ((result (tomlet:parse (format nil "a.b = 1~%a.c = 2"))))
      (let ((a-table (gethash "a" result)))
        (ok (= (gethash "b" a-table) 1))
        (ok (= (gethash "c" a-table) 2))))))

(deftest test-table-sections
  (testing "Simple table section"
    (let ((result (tomlet:parse (format nil "[section]~%key = 1"))))
      (ok (hash-table-p (gethash "section" result)))
      (ok (= (gethash "key" (gethash "section" result)) 1))))

  (testing "Multiple table sections"
    (let ((result (tomlet:parse (format nil "[section1]~%a = 1~%[section2]~%b = 2"))))
      (ok (= (gethash "a" (gethash "section1" result)) 1))
      (ok (= (gethash "b" (gethash "section2" result)) 2))))

  (testing "Nested table sections"
    (let ((result (tomlet:parse (format nil "[a.b]~%c = 3"))))
      (ok (hash-table-p (gethash "a" result)))
      (let ((b-table (gethash "b" (gethash "a" result))))
        (ok (hash-table-p b-table))
        (ok (= (gethash "c" b-table) 3)))))

  (testing "Table section with dotted keys"
    (let ((result (tomlet:parse (format nil "[section]~%a.b = 1"))))
      (let ((section (gethash "section" result)))
        (ok (hash-table-p section))
        (let ((a-table (gethash "a" section)))
          (ok (hash-table-p a-table))
          (ok (= (gethash "b" a-table) 1)))))))

(deftest test-array-of-tables
  (testing "Simple array of tables"
    (let ((result (tomlet:parse (format nil "[[products]]~%name = \"Hammer\"~%~%[[products]]~%name = \"Nail\""))))
      (let ((products (gethash "products" result)))
        (ok (vectorp products))
        (ok (= (length products) 2))
        (ok (hash-table-p (aref products 0)))
        (ok (string= (gethash "name" (aref products 0)) "Hammer"))
        (ok (hash-table-p (aref products 1)))
        (ok (string= (gethash "name" (aref products 1)) "Nail")))))

  (testing "Array of tables with multiple keys"
    (let ((result (tomlet:parse (format nil "[[products]]~%name = \"Hammer\"~%sku = 738594937~%~%[[products]]~%name = \"Nail\"~%sku = 284758393"))))
      (let ((products (gethash "products" result)))
        (ok (= (length products) 2))
        (ok (= (gethash "sku" (aref products 0)) 738594937))
        (ok (= (gethash "sku" (aref products 1)) 284758393)))))

  (testing "Nested array of tables"
    (let ((result (tomlet:parse (format nil "[[fruit]]~%name = \"apple\"~%~%[[fruit.variety]]~%name = \"red delicious\"~%~%[[fruit.variety]]~%name = \"granny smith\""))))
      (let* ((fruit (gethash "fruit" result))
             (fruit-0 (aref fruit 0))
             (varieties (gethash "variety" fruit-0)))
        (ok (vectorp fruit))
        (ok (= (length fruit) 1))
        (ok (string= (gethash "name" fruit-0) "apple"))
        (ok (vectorp varieties))
        (ok (= (length varieties) 2))
        (ok (string= (gethash "name" (aref varieties 0)) "red delicious"))
        (ok (string= (gethash "name" (aref varieties 1)) "granny smith"))))))
