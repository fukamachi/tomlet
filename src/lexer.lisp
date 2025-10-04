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

;;; String lexing (placeholder)

(defun lex-string-token (lexer line col)
  "Lex a basic or multi-line basic string"
  ;; TODO: Implement proper string lexing with escape sequences
  (advance lexer) ;; Skip opening "
  (let ((chars '()))
    (loop while (and (not (at-end-p lexer))
                     (not (char= (current-char lexer) #\")))
          do (push (advance lexer) chars))
    (unless (and (not (at-end-p lexer)) (char= (current-char lexer) #\"))
      (error 'types:toml-parse-error
             :message "Unterminated string"
             :line line
             :column col))
    (advance lexer) ;; Skip closing "
    (make-token :type :string
                :value (coerce (nreverse chars) 'string)
                :line line
                :column col)))

(defun lex-literal-string-token (lexer line col)
  "Lex a literal or multi-line literal string"
  ;; TODO: Implement proper literal string lexing
  (advance lexer) ;; Skip opening '
  (let ((chars '()))
    (loop while (and (not (at-end-p lexer))
                     (not (char= (current-char lexer) #\')))
          do (push (advance lexer) chars))
    (unless (and (not (at-end-p lexer)) (char= (current-char lexer) #\'))
      (error 'types:toml-parse-error
             :message "Unterminated literal string"
             :line line
             :column col))
    (advance lexer) ;; Skip closing '
    (make-token :type :string
                :value (coerce (nreverse chars) 'string)
                :line line
                :column col)))

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
