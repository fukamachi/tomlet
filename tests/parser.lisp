(defpackage #:tomlex/tests/parser
  (:use #:cl
        #:rove))
(in-package #:tomlex/tests/parser)

;;; ===========================================================================
;;; Parser Unit Tests
;;; ===========================================================================

(deftest test-empty-input
  (testing "Empty string"
    (let ((result (tomlex:parse "")))
      (ok (hash-table-p result))
      (ok (= (hash-table-count result) 0))))

  (testing "Only whitespace and newlines"
    (let ((result (tomlex:parse (format nil "~%  ~%  ~%"))))
      (ok (hash-table-p result))
      (ok (= (hash-table-count result) 0)))))

(deftest test-boolean-values
  (testing "Boolean true"
    (let ((result (tomlex:parse "key = true")))
      (ok (hash-table-p result))
      (ok (eq (gethash "key" result) t))))

  (testing "Boolean false"
    (let ((result (tomlex:parse "key = false")))
      (ok (hash-table-p result))
      (ok (eq (gethash "key" result) nil))))

  (testing "Multiple booleans"
    (let ((result (tomlex:parse (format nil "a = true~%b = false~%c = true"))))
      (ok (eq (gethash "a" result) t))
      (ok (eq (gethash "b" result) nil))
      (ok (eq (gethash "c" result) t)))))

(deftest test-integer-values
  (testing "Simple positive integer"
    (let ((result (tomlex:parse "answer = 42")))
      (ok (= (gethash "answer" result) 42))))

  (testing "Zero"
    (let ((result (tomlex:parse "zero = 0")))
      (ok (= (gethash "zero" result) 0))))

  (testing "Multiple integers"
    (let ((result (tomlex:parse (format nil "a = 1~%b = 2~%c = 3"))))
      (ok (= (gethash "a" result) 1))
      (ok (= (gethash "b" result) 2))
      (ok (= (gethash "c" result) 3)))))

(deftest test-string-values
  (testing "Simple string"
    (let ((result (tomlex:parse "key = \"value\"")))
      (ok (string= (gethash "key" result) "value"))))

  (testing "Empty string"
    (let ((result (tomlex:parse "empty = \"\"")))
      (ok (string= (gethash "empty" result) ""))))

  (testing "String with spaces"
    (let ((result (tomlex:parse "text = \"hello world\"")))
      (ok (string= (gethash "text" result) "hello world"))))

  (testing "Multiple strings"
    (let ((result (tomlex:parse (format nil "first = \"one\"~%second = \"two\""))))
      (ok (string= (gethash "first" result) "one"))
      (ok (string= (gethash "second" result) "two")))))

(deftest test-literal-strings
  (testing "Simple literal string"
    (let ((result (tomlex:parse "path = 'C:\\\\Users\\\\name'")))
      (ok (string= (gethash "path" result) "C:\\\\Users\\\\name"))))

  (testing "Literal string with quotes"
    (let ((result (tomlex:parse "quote = 'Tom \"Dubs\" Preston-Werner'")))
      (ok (string= (gethash "quote" result) "Tom \"Dubs\" Preston-Werner")))))

(deftest test-mixed-types
  (testing "Mix of different value types"
    (let ((result (tomlex:parse (format nil "name = \"Alice\"~%age = 30~%active = true"))))
      (ok (string= (gethash "name" result) "Alice"))
      (ok (= (gethash "age" result) 30))
      (ok (eq (gethash "active" result) t)))))

(deftest test-bare-keys
  (testing "Simple bare key"
    (let ((result (tomlex:parse "key = 1")))
      (ok (= (gethash "key" result) 1))))

  (testing "Bare key with underscores"
    (let ((result (tomlex:parse "my_key = 2")))
      (ok (= (gethash "my_key" result) 2))))

  (testing "Bare key with hyphens"
    (let ((result (tomlex:parse "my-key = 3")))
      (ok (= (gethash "my-key" result) 3))))

  (testing "Bare key with numbers"
    (let ((result (tomlex:parse "key123 = 4")))
      (ok (= (gethash "key123" result) 4)))))

(deftest test-quoted-keys
  (testing "String key"
    (let ((result (tomlex:parse "\"key\" = 1")))
      (ok (= (gethash "key" result) 1))))

  (testing "String key with spaces"
    (let ((result (tomlex:parse "\"my key\" = 2")))
      (ok (= (gethash "my key" result) 2)))))

(deftest test-comments
  (testing "Comment before key-value"
    (let ((result (tomlex:parse (format nil "# This is a comment~%key = 1"))))
      (ok (= (gethash "key" result) 1))))

  (testing "Inline comment after value"
    (let ((result (tomlex:parse "key = 1 # comment")))
      (ok (= (gethash "key" result) 1))))

  (testing "Multiple comments"
    (let ((result (tomlex:parse (format nil "# Comment 1~%key1 = 1~%# Comment 2~%key2 = 2"))))
      (ok (= (gethash "key1" result) 1))
      (ok (= (gethash "key2" result) 2)))))

(deftest test-whitespace-handling
  (testing "Extra whitespace around equals"
    (let ((result (tomlex:parse "key   =   \"value\"")))
      (ok (string= (gethash "key" result) "value"))))

  (testing "Leading whitespace"
    (let ((result (tomlex:parse "   key = 1")))
      (ok (= (gethash "key" result) 1))))

  (testing "Trailing whitespace"
    (let ((result (tomlex:parse "key = 1   ")))
      (ok (= (gethash "key" result) 1))))

  (testing "Blank lines between key-value pairs"
    (let ((result (tomlex:parse (format nil "a = 1~%~%~%b = 2"))))
      (ok (= (gethash "a" result) 1))
      (ok (= (gethash "b" result) 2)))))

(deftest test-special-float-values
  (testing "Positive infinity"
    (let ((result (tomlex:parse "infinity = inf")))
      (ok (floatp (gethash "infinity" result)))
      (ok (sb-ext:float-infinity-p (gethash "infinity" result)))))

  (testing "Not a number"
    (let ((result (tomlex:parse "not_a_number = nan")))
      (ok (floatp (gethash "not_a_number" result))))))

(deftest test-key-value-order
  (testing "Keys are stored in order encountered"
    (let ((result (tomlex:parse (format nil "z = 1~%a = 2~%m = 3"))))
      (ok (= (gethash "z" result) 1))
      (ok (= (gethash "a" result) 2))
      (ok (= (gethash "m" result) 3)))))

(deftest test-error-unterminated-string
  (testing "Unterminated string should error"
    (ok (signals (tomlex:parse "key = \"unterminated")
                 'tomlex:toml-parse-error))))

(deftest test-error-missing-value
  (testing "Missing value after equals should error"
    (ok (signals (tomlex:parse "key = ")
                 'tomlex:toml-parse-error))))

(deftest test-error-missing-equals
  (testing "Missing equals between key and value should error"
    (ok (signals (tomlex:parse "key value")
                 'tomlex:toml-parse-error))))

(deftest test-error-bare-key-as-value
  (testing "Bare key in value position should error"
    (ok (signals (tomlex:parse "key = barevalue")
                 'tomlex:toml-parse-error))))

(deftest test-hash-table-properties
  (testing "Result uses equal test"
    (let ((result (tomlex:parse "key = 1")))
      (ok (eq (hash-table-test result) 'equal))))

  (testing "Keys are strings"
    (let ((result (tomlex:parse "key = 1")))
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
      (let ((result (tomlex:parse toml)))
        (ok (string= (gethash "name" result) "My Application"))
        (ok (= (gethash "version" result) 1))
        (ok (eq (gethash "enabled" result) t))
        (ok (= (gethash "timeout" result) 30))
        (ok (eq (gethash "debug" result) nil))))))

(deftest test-array-empty
  (testing "Empty array"
    (let ((result (tomlex:parse "arr = []")))
      (ok (vectorp (gethash "arr" result)))
      (ok (= (length (gethash "arr" result)) 0)))))

(deftest test-array-integers
  (testing "Simple integer array"
    (let ((result (tomlex:parse "numbers = [1, 2, 3]")))
      (let ((arr (gethash "numbers" result)))
        (ok (vectorp arr))
        (ok (= (length arr) 3))
        (ok (= (aref arr 0) 1))
        (ok (= (aref arr 1) 2))
        (ok (= (aref arr 2) 3)))))

  (testing "Array with trailing comma"
    (let ((result (tomlex:parse "numbers = [1, 2, 3,]")))
      (let ((arr (gethash "numbers" result)))
        (ok (= (length arr) 3))
        (ok (= (aref arr 0) 1))
        (ok (= (aref arr 1) 2))
        (ok (= (aref arr 2) 3))))))

(deftest test-array-strings
  (testing "String array"
    (let ((result (tomlex:parse "colors = [\"red\", \"green\", \"blue\"]")))
      (let ((arr (gethash "colors" result)))
        (ok (vectorp arr))
        (ok (= (length arr) 3))
        (ok (string= (aref arr 0) "red"))
        (ok (string= (aref arr 1) "green"))
        (ok (string= (aref arr 2) "blue"))))))

(deftest test-array-booleans
  (testing "Boolean array"
    (let ((result (tomlex:parse "flags = [true, false, true]")))
      (let ((arr (gethash "flags" result)))
        (ok (= (length arr) 3))
        (ok (eq (aref arr 0) t))
        (ok (eq (aref arr 1) nil))
        (ok (eq (aref arr 2) t))))))

(deftest test-array-mixed
  (testing "Heterogeneous array"
    (let ((result (tomlex:parse "mixed = [1, \"two\", true]")))
      (let ((arr (gethash "mixed" result)))
        (ok (= (length arr) 3))
        (ok (= (aref arr 0) 1))
        (ok (string= (aref arr 1) "two"))
        (ok (eq (aref arr 2) t))))))

(deftest test-array-nested
  (testing "Nested arrays"
    (let ((result (tomlex:parse "nested = [[1, 2], [3, 4]]")))
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
    (let ((result (tomlex:parse (format nil "arr = [~%  1,~%  2,~%  3~%]"))))
      (let ((arr (gethash "arr" result)))
        (ok (= (length arr) 3))
        (ok (= (aref arr 0) 1))
        (ok (= (aref arr 1) 2))
        (ok (= (aref arr 2) 3)))))

  (testing "Array with trailing comma and newline"
    (let ((result (tomlex:parse (format nil "arr = [~%  1,~%  2,~%  3,~%]"))))
      (let ((arr (gethash "arr" result)))
        (ok (= (length arr) 3))))))

(deftest test-array-single-element
  (testing "Single element array"
    (let ((result (tomlex:parse "single = [42]")))
      (let ((arr (gethash "single" result)))
        (ok (= (length arr) 1))
        (ok (= (aref arr 0) 42))))))
