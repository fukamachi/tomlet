(defpackage #:tomlet/tests/lexer
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:lexer #:tomlet/lexer)
   (#:float-utils #:tomlet/float-utils)))
(in-package #:tomlet/tests/lexer)

;;; ===========================================================================
;;; Lexer Unit Tests
;;; ===========================================================================

(deftest test-structural-tokens
  (testing "Equals sign"
    (let ((tokens (lexer:lex "=")))
      (ok (= (length tokens) 2))
      (ok (eq (lexer:token-type (first tokens)) :equals))
      (ok (eq (lexer:token-type (second tokens)) :eof))))

  (testing "Brackets"
    (let ((tokens (lexer:lex "[]")))
      (ok (= (length tokens) 3))
      (ok (eq (lexer:token-type (first tokens)) :left-bracket))
      (ok (eq (lexer:token-type (second tokens)) :right-bracket))
      (ok (eq (lexer:token-type (third tokens)) :eof))))

  (testing "Braces"
    (let ((tokens (lexer:lex "{}")))
      (ok (= (length tokens) 3))
      (ok (eq (lexer:token-type (first tokens)) :left-brace))
      (ok (eq (lexer:token-type (second tokens)) :right-brace))))

  (testing "Comma and dot"
    (let ((tokens (lexer:lex ",.")))
      (ok (= (length tokens) 3))
      (ok (eq (lexer:token-type (first tokens)) :comma))
      (ok (eq (lexer:token-type (second tokens)) :dot)))))

(deftest test-whitespace-and-newlines
  (testing "Whitespace is skipped"
    (let ((tokens (lexer:lex "  =  ")))
      (ok (= (length tokens) 2))
      (ok (eq (lexer:token-type (first tokens)) :equals))))

  (testing "Newlines are preserved"
    (let ((tokens (lexer:lex (format nil "=~%="))))
      (ok (= (length tokens) 4))
      (ok (eq (lexer:token-type (first tokens)) :equals))
      (ok (eq (lexer:token-type (second tokens)) :newline))
      (ok (eq (lexer:token-type (third tokens)) :equals))))

  (testing "Multiple newlines"
    (let ((tokens (lexer:lex (format nil "~%~%"))))
      (ok (= (length tokens) 3))
      (ok (eq (lexer:token-type (first tokens)) :newline))
      (ok (eq (lexer:token-type (second tokens)) :newline)))))

(deftest test-comments
  (testing "Comment until end of line"
    (let ((tokens (lexer:lex "# this is a comment")))
      (ok (= (length tokens) 1))
      (ok (eq (lexer:token-type (first tokens)) :eof))))

  (testing "Comment with content after newline"
    (let ((tokens (lexer:lex (format nil "# comment~%key = 1"))))
      (ok (>= (length tokens) 4))
      (ok (eq (lexer:token-type (first tokens)) :newline))
      (ok (eq (lexer:token-type (second tokens)) :bare-key))
      (ok (string= (lexer:token-value (second tokens)) "key"))))

  (testing "Inline comment"
    (let ((tokens (lexer:lex "key = 1 # comment")))
      (ok (>= (length tokens) 3))
      (ok (eq (lexer:token-type (first tokens)) :bare-key))
      (ok (eq (lexer:token-type (second tokens)) :equals))
      (ok (eq (lexer:token-type (third tokens)) :integer)))))

(deftest test-bare-keys
  (testing "Simple bare key"
    (let ((tokens (lexer:lex "key")))
      (ok (= (length tokens) 2))
      (ok (eq (lexer:token-type (first tokens)) :bare-key))
      (ok (string= (lexer:token-value (first tokens)) "key"))))

  (testing "Bare key with underscores"
    (let ((tokens (lexer:lex "my_key")))
      (ok (string= (lexer:token-value (first tokens)) "my_key"))))

  (testing "Bare key with hyphens"
    (let ((tokens (lexer:lex "my-key")))
      (ok (string= (lexer:token-value (first tokens)) "my-key"))))

  (testing "Bare key with numbers"
    (let ((tokens (lexer:lex "key123")))
      (ok (string= (lexer:token-value (first tokens)) "key123")))))

(deftest test-boolean-keywords
  (testing "true keyword"
    (let ((tokens (lexer:lex "true")))
      (ok (eq (lexer:token-type (first tokens)) :boolean))
      (ok (eq (lexer:token-value (first tokens)) t))))

  (testing "false keyword"
    (let ((tokens (lexer:lex "false")))
      (ok (eq (lexer:token-type (first tokens)) :boolean))
      (ok (eq (lexer:token-value (first tokens)) nil)))))

(deftest test-special-float-keywords
  (testing "inf keyword"
    (let ((tokens (lexer:lex "inf")))
      (ok (eq (lexer:token-type (first tokens)) :float))
      (ok (float-utils:float-infinity-p (lexer:token-value (first tokens))))))

  (testing "nan keyword"
    (let ((tokens (lexer:lex "nan")))
      (ok (eq (lexer:token-type (first tokens)) :float)))))

(deftest test-basic-strings
  (testing "Simple string"
    (let ((tokens (lexer:lex "\"hello\"")))
      (ok (= (length tokens) 2))
      (ok (eq (lexer:token-type (first tokens)) :string))
      (ok (string= (lexer:token-value (first tokens)) "hello"))))

  (testing "Empty string"
    (let ((tokens (lexer:lex "\"\"")))
      (ok (eq (lexer:token-type (first tokens)) :string))
      (ok (string= (lexer:token-value (first tokens)) ""))))

  (testing "String with spaces"
    (let ((tokens (lexer:lex "\"hello world\"")))
      (ok (string= (lexer:token-value (first tokens)) "hello world")))))

(deftest test-string-escape-sequences
  (testing "Basic escapes - newline and tab"
    (let ((tokens (lexer:lex "\"\\n\\t\"")))
      (ok (string= (lexer:token-value (first tokens))
                   (format nil "~C~C" #\Newline #\Tab)))))

  (testing "Basic escapes - quote and backslash"
    (let ((tokens (lexer:lex "\"\\\"\\\\\"")))
      (ok (string= (lexer:token-value (first tokens)) "\"\\"))))

  (testing "Unicode escape \\u"
    (let ((tokens (lexer:lex "\"\\u0041\"")))
      (ok (string= (lexer:token-value (first tokens)) "A"))))

  (testing "Unicode escape \\U"
    (let ((tokens (lexer:lex "\"\\U0001F4A9\"")))
      (ok (= (char-code (char (lexer:token-value (first tokens)) 0)) #x1F4A9))))

  (testing "Hex escape \\x"
    (let ((tokens (lexer:lex "\"\\x41\"")))
      (ok (string= (lexer:token-value (first tokens)) "A"))))

  (testing "Escape e (escape character)"
    (let ((tokens (lexer:lex "\"\\e\"")))
      (ok (= (char-code (char (lexer:token-value (first tokens)) 0)) #x1B)))))

(deftest test-multiline-basic-strings
  (testing "Simple multi-line string"
    (let ((tokens (lexer:lex (format nil "\"\"\"~%line1~%line2~%\"\"\""))))
      (ok (eq (lexer:token-type (first tokens)) :string))
      (ok (string= (lexer:token-value (first tokens))
                   (format nil "line1~%line2~%")))))

  (testing "Multi-line string with line-ending backslash"
    (let ((tokens (lexer:lex (format nil "\"\"\"The quick \\~%    brown fox\"\"\""))))
      (ok (string= (lexer:token-value (first tokens)) "The quick brown fox"))))

  (testing "Empty multi-line string"
    (let ((tokens (lexer:lex "\"\"\"\"\"\"")))
      (ok (string= (lexer:token-value (first tokens)) ""))))

  (testing "Multi-line string with quote inside"
    (let ((tokens (lexer:lex "\"\"\"She said \"hello\"\"\"\"\"")))
      ;; Input has 5 closing quotes, so 2 quotes are added to content: "She said "hello""
      (ok (string= (lexer:token-value (first tokens)) "She said \"hello\"\"")))))

(deftest test-multiline-literal-strings
  (testing "Simple multi-line literal string"
    (let ((tokens (lexer:lex (format nil "'''~%line1~%line2~%'''"))))
      (ok (eq (lexer:token-type (first tokens)) :string))
      (ok (string= (lexer:token-value (first tokens))
                   (format nil "line1~%line2~%")))))

  (testing "Multi-line literal with backslashes"
    (let ((tokens (lexer:lex "'''C:\\\\Users\\\\name'''")))
      (ok (string= (lexer:token-value (first tokens)) "C:\\\\Users\\\\name"))))

  (testing "Empty multi-line literal string"
    (let ((tokens (lexer:lex "''''''")))
      (ok (string= (lexer:token-value (first tokens)) "")))))

(deftest test-literal-strings
  (testing "Simple literal string"
    (let ((tokens (lexer:lex "'hello'")))
      (ok (eq (lexer:token-type (first tokens)) :string))
      (ok (string= (lexer:token-value (first tokens)) "hello"))))

  (testing "Literal string with special chars"
    (let ((tokens (lexer:lex "'C:\\\\Users\\\\name'")))
      (ok (string= (lexer:token-value (first tokens)) "C:\\\\Users\\\\name")))))

(deftest test-integers
  (testing "Simple positive integer"
    (let ((tokens (lexer:lex "42")))
      (ok (eq (lexer:token-type (first tokens)) :integer))
      (ok (= (lexer:token-value (first tokens)) 42))))

  (testing "Positive integer with sign"
    (let ((tokens (lexer:lex "+42")))
      (ok (eq (lexer:token-type (first tokens)) :integer))
      (ok (= (lexer:token-value (first tokens)) 42))))

  (testing "Negative integer"
    (let ((tokens (lexer:lex "-42")))
      (ok (eq (lexer:token-type (first tokens)) :integer))
      (ok (= (lexer:token-value (first tokens)) -42))))

  (testing "Integer with underscores"
    (let ((tokens (lexer:lex "1_000_000")))
      (ok (eq (lexer:token-type (first tokens)) :integer))
      (ok (= (lexer:token-value (first tokens)) 1000000))))

  (testing "Hex integer"
    (let ((tokens (lexer:lex "0xFF")))
      (ok (eq (lexer:token-type (first tokens)) :integer))
      (ok (= (lexer:token-value (first tokens)) 255))))

  (testing "Octal integer"
    (let ((tokens (lexer:lex "0o77")))
      (ok (eq (lexer:token-type (first tokens)) :integer))
      (ok (= (lexer:token-value (first tokens)) 63))))

  (testing "Binary integer"
    (let ((tokens (lexer:lex "0b1010")))
      (ok (eq (lexer:token-type (first tokens)) :integer))
      (ok (= (lexer:token-value (first tokens)) 10)))))

(deftest test-floats
  (testing "Simple float"
    (let ((tokens (lexer:lex "3.14")))
      (ok (eq (lexer:token-type (first tokens)) :float))
      (ok (< (abs (- (lexer:token-value (first tokens)) 3.14d0)) 0.001d0))))

  (testing "Float with exponent"
    (let ((tokens (lexer:lex "1.5e10")))
      (ok (eq (lexer:token-type (first tokens)) :float))
      (ok (= (lexer:token-value (first tokens)) 1.5d10))))

  (testing "Float with negative exponent"
    (let ((tokens (lexer:lex "1.5e-5")))
      (ok (eq (lexer:token-type (first tokens)) :float))
      (ok (< (abs (- (lexer:token-value (first tokens)) 1.5d-5)) 1.0d-10))))

  (testing "Float with underscores"
    (let ((tokens (lexer:lex "1_000.5")))
      (ok (eq (lexer:token-type (first tokens)) :float))
      (ok (< (abs (- (lexer:token-value (first tokens)) 1000.5d0)) 0.001d0)))))

(deftest test-position-tracking
  (testing "Line and column tracking"
    (let ((tokens (lexer:lex "key")))
      (ok (= (lexer:token-line (first tokens)) 1))
      (ok (= (lexer:token-column (first tokens)) 1))))

  (testing "Column advances"
    (let ((tokens (lexer:lex "a b")))
      (ok (= (lexer:token-column (first tokens)) 1))
      (ok (= (lexer:token-column (second tokens)) 3))))

  (testing "Line advances on newline"
    (let ((tokens (lexer:lex (format nil "a~%b"))))
      (ok (= (lexer:token-line (first tokens)) 1))
      (ok (= (lexer:token-line (second tokens)) 1)) ;; newline token
      (ok (= (lexer:token-line (third tokens)) 2))))) ;; 'b' is on line 2

(deftest test-complete-key-value-pair
  (testing "key = value"
    (let ((tokens (lexer:lex "key = \"value\"")))
      (ok (= (length tokens) 4))
      (ok (eq (lexer:token-type (first tokens)) :bare-key))
      (ok (string= (lexer:token-value (first tokens)) "key"))
      (ok (eq (lexer:token-type (second tokens)) :equals))
      (ok (eq (lexer:token-type (third tokens)) :string))
      (ok (string= (lexer:token-value (third tokens)) "value"))
      (ok (eq (lexer:token-type (fourth tokens)) :eof)))))

(deftest test-section-header
  (testing "[section]"
    (let ((tokens (lexer:lex "[section]")))
      (ok (= (length tokens) 4))
      (ok (eq (lexer:token-type (first tokens)) :left-bracket))
      (ok (eq (lexer:token-type (second tokens)) :bare-key))
      (ok (string= (lexer:token-value (second tokens)) "section"))
      (ok (eq (lexer:token-type (third tokens)) :right-bracket)))))

(deftest test-dotted-key
  (testing "a.b.c"
    (let ((tokens (lexer:lex "a.b.c")))
      (ok (>= (length tokens) 5))
      (ok (eq (lexer:token-type (first tokens)) :bare-key))
      (ok (string= (lexer:token-value (first tokens)) "a"))
      (ok (eq (lexer:token-type (second tokens)) :dot))
      (ok (eq (lexer:token-type (third tokens)) :bare-key))
      (ok (string= (lexer:token-value (third tokens)) "b"))
      (ok (eq (lexer:token-type (fourth tokens)) :dot))
      (ok (eq (lexer:token-type (fifth tokens)) :bare-key))
      (ok (string= (lexer:token-value (fifth tokens)) "c")))))

(deftest test-datetimes
  (testing "Local date"
    (let ((tokens (lexer:lex "1979-05-27")))
      (ok (eq (lexer:token-type (first tokens)) :datetime))
      (ok (tomlet/types:local-date-p (lexer:token-value (first tokens))))
      (ok (= (tomlet/types:local-date-year (lexer:token-value (first tokens))) 1979))
      (ok (= (tomlet/types:local-date-month (lexer:token-value (first tokens))) 5))
      (ok (= (tomlet/types:local-date-day (lexer:token-value (first tokens))) 27))))

  (testing "Local time"
    (let ((tokens (lexer:lex "07:32:00")))
      (ok (eq (lexer:token-type (first tokens)) :datetime))
      (ok (tomlet/types:local-time-p (lexer:token-value (first tokens))))
      (ok (= (tomlet/types:local-time-hour (lexer:token-value (first tokens))) 7))
      (ok (= (tomlet/types:local-time-minute (lexer:token-value (first tokens))) 32))
      (ok (= (tomlet/types:local-time-second (lexer:token-value (first tokens))) 0))))

  (testing "Local datetime"
    (let ((tokens (lexer:lex "1979-05-27T07:32:00")))
      (ok (eq (lexer:token-type (first tokens)) :datetime))
      (ok (tomlet/types:local-datetime-p (lexer:token-value (first tokens))))
      (ok (= (tomlet/types:local-datetime-year (lexer:token-value (first tokens))) 1979))
      (ok (= (tomlet/types:local-datetime-month (lexer:token-value (first tokens))) 5))
      (ok (= (tomlet/types:local-datetime-day (lexer:token-value (first tokens))) 27))
      (ok (= (tomlet/types:local-datetime-hour (lexer:token-value (first tokens))) 7))
      (ok (= (tomlet/types:local-datetime-minute (lexer:token-value (first tokens))) 32))
      (ok (= (tomlet/types:local-datetime-second (lexer:token-value (first tokens))) 0))))

  (testing "Offset datetime with Z"
    (let ((tokens (lexer:lex "1979-05-27T07:32:00Z")))
      (ok (eq (lexer:token-type (first tokens)) :datetime))
      (ok (tomlet/types:offset-datetime-p (lexer:token-value (first tokens))))
      (ok (= (tomlet/types:offset-datetime-year (lexer:token-value (first tokens))) 1979))
      (ok (= (tomlet/types:offset-datetime-offset (lexer:token-value (first tokens))) 0))))

  (testing "Offset datetime with timezone"
    (let ((tokens (lexer:lex "1979-05-27T00:32:00+07:00")))
      (ok (eq (lexer:token-type (first tokens)) :datetime))
      (ok (tomlet/types:offset-datetime-p (lexer:token-value (first tokens))))
      (ok (= (tomlet/types:offset-datetime-offset (lexer:token-value (first tokens))) 420)))))
