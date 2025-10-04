(defpackage #:tomlex/tests/lexer
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:lexer #:tomlex/lexer)))
(in-package #:tomlex/tests/lexer)

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
    (let ((tokens (lexer:lex "=\n=")))
      (ok (= (length tokens) 4))
      (ok (eq (lexer:token-type (first tokens)) :equals))
      (ok (eq (lexer:token-type (second tokens)) :newline))
      (ok (eq (lexer:token-type (third tokens)) :equals))))

  (testing "Multiple newlines"
    (let ((tokens (lexer:lex "\n\n")))
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
      (ok (sb-ext:float-infinity-p (lexer:token-value (first tokens))))))

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
      (ok (eq (lexer:token-type (first tokens)) :integer))))

  (testing "Negative integer"
    (let ((tokens (lexer:lex "-42")))
      (ok (eq (lexer:token-type (first tokens)) :integer)))))

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
