(defpackage #:tomlex/lexer
  (:use #:cl)
  (:local-nicknames
   (#:types #:tomlex/types))
  (:export #:token
           #:make-token
           #:token-type
           #:token-value
           #:token-line
           #:token-column
           #:lex
           #:lex-string))
(in-package #:tomlex/lexer)

;;; Token Types
;;;
;;; TOML tokens:
;;; - Structural: =, [, ], {, }, ,, ., newline, eof
;;; - Values: string, integer, float, boolean, datetime
;;; - Identifiers: bare-key (unquoted key names)

(defstruct token
  "Represents a lexical token with position information"
  (type nil :type symbol)
  (value nil :type t)
  (line 1 :type fixnum)
  (column 1 :type fixnum))

;;; Token type keywords:
;;; :equals, :left-bracket, :right-bracket, :left-brace, :right-brace
;;; :comma, :dot, :newline, :eof
;;; :string, :integer, :float, :boolean, :datetime
;;; :bare-key

(defstruct lexer
  "Lexer state"
  (input "" :type string)
  (position 0 :type fixnum)
  (line 1 :type fixnum)
  (column 1 :type fixnum))

(defun lex-string (input)
  "Lex a TOML string and return a list of tokens"
  (check-type input string)
  (let ((lexer (make-lexer :input input)))
    (loop
      with tokens = '()
      for token = (next-token lexer)
      while token
      do (push token tokens)
      until (eq (token-type token) :eof)
      finally (return (nreverse tokens)))))

(defun lex (input)
  "Alias for lex-string"
  (lex-string input))

(defun next-token (lexer)
  "Read the next token from the lexer"
  (skip-whitespace lexer)
  (when (at-end-p lexer)
    (return-from next-token (make-token :type :eof
                                         :line (lexer-line lexer)
                                         :column (lexer-column lexer))))

  (let ((ch (current-char lexer))
        (line (lexer-line lexer))
        (col (lexer-column lexer)))
    (cond
      ;; Comments
      ((char= ch #\#)
       (skip-comment lexer)
       (next-token lexer))

      ;; Newlines
      ((char= ch #\Newline)
       (advance lexer)
       (make-token :type :newline :line line :column col))

      ;; Structural characters
      ((char= ch #\=)
       (advance lexer)
       (make-token :type :equals :line line :column col))

      ((char= ch #\[)
       (advance lexer)
       (make-token :type :left-bracket :line line :column col))

      ((char= ch #\])
       (advance lexer)
       (make-token :type :right-bracket :line line :column col))

      ((char= ch #\{)
       (advance lexer)
       (make-token :type :left-brace :line line :column col))

      ((char= ch #\})
       (advance lexer)
       (make-token :type :right-brace :line line :column col))

      ((char= ch #\,)
       (advance lexer)
       (make-token :type :comma :line line :column col))

      ((char= ch #\.)
       (advance lexer)
       (make-token :type :dot :line line :column col))

      ;; Strings
      ((char= ch #\")
       (lex-string-token lexer line col))

      ((char= ch #\')
       (lex-literal-string-token lexer line col))

      ;; Bare keys and keywords (true, false, inf, nan)
      ((or (alpha-char-p ch) (char= ch #\_))
       (lex-bare-key-or-keyword lexer line col))

      ;; Numbers (including dates which start with digits)
      ((or (digit-char-p ch) (char= ch #\+) (char= ch #\-))
       (lex-number-or-datetime lexer line col))

      (t
       (error 'types:toml-parse-error
              :message (format nil "Unexpected character: ~C" ch)
              :line line
              :column col)))))

;;; Lexer utilities

(defun at-end-p (lexer)
  "Check if lexer is at end of input"
  (>= (lexer-position lexer) (length (lexer-input lexer))))

(defun current-char (lexer)
  "Get current character without advancing"
  (unless (at-end-p lexer)
    (char (lexer-input lexer) (lexer-position lexer))))

(defun peek-char-at (lexer offset)
  "Peek at character at offset positions ahead"
  (let ((pos (+ (lexer-position lexer) offset)))
    (when (< pos (length (lexer-input lexer)))
      (char (lexer-input lexer) pos))))

(defun advance (lexer)
  "Advance to next character, updating position tracking"
  (unless (at-end-p lexer)
    (let ((ch (current-char lexer)))
      (incf (lexer-position lexer))
      (if (char= ch #\Newline)
          (progn
            (incf (lexer-line lexer))
            (setf (lexer-column lexer) 1))
          (incf (lexer-column lexer)))
      ch)))

(defun skip-whitespace (lexer)
  "Skip whitespace characters (space, tab, carriage return) but not newlines"
  (loop while (and (not (at-end-p lexer))
                   (member (current-char lexer) '(#\Space #\Tab #\Return)))
        do (advance lexer)))

(defun skip-comment (lexer)
  "Skip from # to end of line"
  (loop while (and (not (at-end-p lexer))
                   (not (char= (current-char lexer) #\Newline)))
        do (advance lexer)))

;;; String lexing with escape sequences

(defun parse-hex-escape (lexer num-digits)
  "Parse a hex escape sequence of NUM-DIGITS hex digits"
  (let ((hex-chars '()))
    (dotimes (i num-digits)
      (when (at-end-p lexer)
        (error 'types:toml-parse-error
               :message (format nil "Incomplete hex escape sequence (expected ~D digits)" num-digits)))
      (let ((ch (current-char lexer)))
        (unless (or (digit-char-p ch 16))
          (error 'types:toml-parse-error
                 :message (format nil "Invalid hex digit in escape sequence: ~C" ch)))
        (push (advance lexer) hex-chars)))
    (let ((code (parse-integer (coerce (nreverse hex-chars) 'string) :radix 16)))
      (code-char code))))

(defun lex-escape-sequence (lexer)
  "Lex an escape sequence after seeing backslash"
  (when (at-end-p lexer)
    (error 'types:toml-parse-error
           :message "Incomplete escape sequence at end of input"))
  (let ((ch (advance lexer)))
    (case ch
      (#\b #\Backspace)
      (#\t #\Tab)
      (#\n #\Newline)
      (#\f #\Page)
      (#\r #\Return)
      (#\" #\")
      (#\\ #\\)
      (#\e #\Escape)
      (#\x (parse-hex-escape lexer 2))      ; \xHH
      (#\u (parse-hex-escape lexer 4))      ; \uHHHH
      (#\U (parse-hex-escape lexer 8))      ; \UHHHHHHHH
      (t (error 'types:toml-parse-error
                :message (format nil "Invalid escape sequence: \\~C" ch))))))

(defun lex-string-token (lexer line col)
  "Lex a basic or multi-line basic string"
  (advance lexer) ;; Skip opening "

  ;; Check for multi-line string (""")
  (let ((is-multiline nil))
    (when (and (not (at-end-p lexer))
               (char= (current-char lexer) #\")
               (peek-char-at lexer 1)
               (char= (peek-char-at lexer 1) #\"))
      (setf is-multiline t)
      (advance lexer) ;; Skip second "
      (advance lexer) ;; Skip third "
      ;; Skip optional newline right after opening """
      (when (and (not (at-end-p lexer))
                 (char= (current-char lexer) #\Newline))
        (advance lexer)))

    (let ((chars '()))
      (loop
        (when (at-end-p lexer)
          (error 'types:toml-parse-error
                 :message (if is-multiline
                              "Unterminated multi-line string"
                              "Unterminated string")
                 :line line
                 :column col))

        (let ((ch (current-char lexer)))
          (cond
            ;; Check for closing quotes
            ((char= ch #\")
             (if is-multiline
                 ;; Need to see """
                 (if (and (peek-char-at lexer 1)
                          (char= (peek-char-at lexer 1) #\")
                          (peek-char-at lexer 2)
                          (char= (peek-char-at lexer 2) #\"))
                     (progn
                       (advance lexer) ;; Skip first "
                       (advance lexer) ;; Skip second "
                       (advance lexer) ;; Skip third "
                       (return))
                     ;; Just a single " inside multi-line string
                     (push (advance lexer) chars))
                 ;; Single-line string - done
                 (progn
                   (advance lexer)
                   (return))))

            ;; Escape sequences
            ((char= ch #\\)
             (advance lexer) ;; Skip backslash
             ;; Line-ending backslash in multi-line strings
             (if (and is-multiline
                      (not (at-end-p lexer))
                      (char= (current-char lexer) #\Newline))
                 (progn
                   (advance lexer) ;; Skip newline
                   ;; Skip whitespace at beginning of next line
                   (loop while (and (not (at-end-p lexer))
                                    (member (current-char lexer) '(#\Space #\Tab #\Newline #\Return)))
                         do (advance lexer)))
                 ;; Regular escape sequence
                 (push (lex-escape-sequence lexer) chars)))

            ;; Newline in single-line string is error
            ((and (not is-multiline) (char= ch #\Newline))
             (error 'types:toml-parse-error
                    :message "Newline not allowed in single-line string"
                    :line line
                    :column col))

            ;; Regular character
            (t
             (push (advance lexer) chars)))))

      (make-token :type :string
                  :value (coerce (nreverse chars) 'string)
                  :line line
                  :column col))))

(defun lex-literal-string-token (lexer line col)
  "Lex a literal or multi-line literal string"
  (advance lexer) ;; Skip opening '

  ;; Check for multi-line literal string (''')
  (let ((is-multiline nil))
    (when (and (not (at-end-p lexer))
               (char= (current-char lexer) #\')
               (peek-char-at lexer 1)
               (char= (peek-char-at lexer 1) #\'))
      (setf is-multiline t)
      (advance lexer) ;; Skip second '
      (advance lexer) ;; Skip third '
      ;; Skip optional newline right after opening '''
      (when (and (not (at-end-p lexer))
                 (char= (current-char lexer) #\Newline))
        (advance lexer)))

    (let ((chars '()))
      (loop
        (when (at-end-p lexer)
          (error 'types:toml-parse-error
                 :message (if is-multiline
                              "Unterminated multi-line literal string"
                              "Unterminated literal string")
                 :line line
                 :column col))

        (let ((ch (current-char lexer)))
          (cond
            ;; Check for closing quotes
            ((char= ch #\')
             (if is-multiline
                 ;; Need to see '''
                 (if (and (peek-char-at lexer 1)
                          (char= (peek-char-at lexer 1) #\')
                          (peek-char-at lexer 2)
                          (char= (peek-char-at lexer 2) #\'))
                     (progn
                       (advance lexer) ;; Skip first '
                       (advance lexer) ;; Skip second '
                       (advance lexer) ;; Skip third '
                       (return))
                     ;; Just a single ' inside multi-line string
                     (push (advance lexer) chars))
                 ;; Single-line string - done
                 (progn
                   (advance lexer)
                   (return))))

            ;; Newline in single-line literal string is error
            ((and (not is-multiline) (char= ch #\Newline))
             (error 'types:toml-parse-error
                    :message "Newline not allowed in single-line literal string"
                    :line line
                    :column col))

            ;; Regular character (no escapes in literal strings)
            (t
             (push (advance lexer) chars)))))

      (make-token :type :string
                  :value (coerce (nreverse chars) 'string)
                  :line line
                  :column col))))

;;; Bare key and keyword lexing

(defun lex-bare-key-or-keyword (lexer line col)
  "Lex a bare key or keyword (true, false, inf, nan)"
  (let ((chars '()))
    (loop while (and (not (at-end-p lexer))
                     (or (alphanumericp (current-char lexer))
                         (char= (current-char lexer) #\_)
                         (char= (current-char lexer) #\-)))
          do (push (advance lexer) chars))
    (let ((text (coerce (nreverse chars) 'string)))
      (cond
        ((string= text "true")
         (make-token :type :boolean :value t :line line :column col))
        ((string= text "false")
         (make-token :type :boolean :value nil :line line :column col))
        ((string= text "inf")
         (make-token :type :float :value sb-ext:double-float-positive-infinity :line line :column col))
        ((string= text "nan")
         (make-token :type :float :value sb-ext:double-float-positive-infinity :line line :column col)) ;; TODO: proper NaN
        (t
         (make-token :type :bare-key :value text :line line :column col))))))

;;; Number and datetime lexing (placeholder)

(defun lex-number-or-datetime (lexer line col)
  "Lex a number (integer/float) or datetime"
  ;; TODO: Implement proper number/datetime discrimination
  (let ((chars '()))
    (loop while (and (not (at-end-p lexer))
                     (or (digit-char-p (current-char lexer))
                         (member (current-char lexer) '(#\+ #\- #\_ #\. #\e #\E #\x #\o #\b #\:))))
          do (push (advance lexer) chars))
    (let ((text (coerce (nreverse chars) 'string)))
      ;; Simple integer parsing for now
      (make-token :type :integer
                  :value (parse-integer text :junk-allowed t)
                  :line line
                  :column col))))
