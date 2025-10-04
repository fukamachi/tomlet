(defpackage #:tomlex
  (:use #:cl)
  (:local-nicknames
   (#:types #:tomlex/types)
   (#:lexer #:tomlex/lexer))
  (:import-from #:tomlex/types
                #:toml-error
                #:toml-error-message
                #:toml-parse-error
                #:toml-parse-error-line
                #:toml-parse-error-column
                #:local-date
                #:make-local-date
                #:local-date-year
                #:local-date-month
                #:local-date-day
                #:local-time
                #:make-local-time
                #:local-time-hour
                #:local-time-minute
                #:local-time-second
                #:local-time-nanosecond
                #:local-datetime
                #:make-local-datetime
                #:local-datetime-year
                #:local-datetime-month
                #:local-datetime-day
                #:local-datetime-hour
                #:local-datetime-minute
                #:local-datetime-second
                #:local-datetime-nanosecond
                #:offset-datetime
                #:make-offset-datetime
                #:offset-datetime-year
                #:offset-datetime-month
                #:offset-datetime-day
                #:offset-datetime-hour
                #:offset-datetime-minute
                #:offset-datetime-second
                #:offset-datetime-nanosecond
                #:offset-datetime-offset)
  (:export #:parse
           #:parse-file
           ;; Re-export from tomlex/types
           #:toml-error
           #:toml-error-message
           #:toml-parse-error
           #:toml-parse-error-line
           #:toml-parse-error-column
           #:local-date
           #:make-local-date
           #:local-date-year
           #:local-date-month
           #:local-date-day
           #:local-time
           #:make-local-time
           #:local-time-hour
           #:local-time-minute
           #:local-time-second
           #:local-time-nanosecond
           #:local-datetime
           #:make-local-datetime
           #:local-datetime-year
           #:local-datetime-month
           #:local-datetime-day
           #:local-datetime-hour
           #:local-datetime-minute
           #:local-datetime-second
           #:local-datetime-nanosecond
           #:offset-datetime
           #:make-offset-datetime
           #:offset-datetime-year
           #:offset-datetime-month
           #:offset-datetime-day
           #:offset-datetime-hour
           #:offset-datetime-minute
           #:offset-datetime-second
           #:offset-datetime-nanosecond
           #:offset-datetime-offset))
(in-package #:tomlex)

;;; Parser State

(defstruct parser-state
  "Parser state for tracking position and context"
  (tokens '() :type list)
  (position 0 :type fixnum)
  (result (make-hash-table :test 'equal) :type hash-table)
  (current-table-path '() :type list))

(defun current-token (state)
  "Get current token without advancing"
  (when (< (parser-state-position state) (length (parser-state-tokens state)))
    (nth (parser-state-position state) (parser-state-tokens state))))

(defun peek-token (state &optional (offset 1))
  "Peek at token at offset positions ahead"
  (let ((pos (+ (parser-state-position state) offset)))
    (when (< pos (length (parser-state-tokens state)))
      (nth pos (parser-state-tokens state)))))

(defun advance-token (state)
  "Advance to next token"
  (incf (parser-state-position state)))

(defun expect-token (state expected-type)
  "Expect a token of a specific type, error if not found"
  (let ((token (current-token state)))
    (unless token
      (error 'types:toml-parse-error
             :message (format nil "Unexpected end of input, expected ~A" expected-type)))
    (unless (eq (lexer:token-type token) expected-type)
      (error 'types:toml-parse-error
             :message (format nil "Expected ~A but got ~A"
                              expected-type (lexer:token-type token))
             :line (lexer:token-line token)
             :column (lexer:token-column token)))
    (prog1 token
      (advance-token state))))

(defun skip-newlines (state)
  "Skip any newline tokens"
  (loop while (and (current-token state)
                   (eq (lexer:token-type (current-token state)) :newline))
        do (advance-token state)))

;;; Value Parsing

(defun parse-array (state)
  "Parse an array [...]"
  (expect-token state :left-bracket)
  (let ((elements '()))
    ;; Skip leading whitespace/newlines
    (loop while (and (current-token state)
                     (member (lexer:token-type (current-token state))
                             '(:newline)))
          do (advance-token state))

    ;; Empty array check
    (when (and (current-token state)
               (eq (lexer:token-type (current-token state)) :right-bracket))
      (advance-token state)
      (return-from parse-array (make-array 0)))

    ;; Parse elements
    (loop
      (push (parse-value state) elements)

      ;; Skip whitespace/newlines after value
      (loop while (and (current-token state)
                       (member (lexer:token-type (current-token state))
                               '(:newline)))
            do (advance-token state))

      (let ((token (current-token state)))
        (unless token
          (error 'types:toml-parse-error
                 :message "Unterminated array"))

        (cond
          ;; End of array
          ((eq (lexer:token-type token) :right-bracket)
           (advance-token state)
           (return))

          ;; Comma - continue to next element
          ((eq (lexer:token-type token) :comma)
           (advance-token state)
           ;; Skip whitespace/newlines after comma
           (loop while (and (current-token state)
                            (member (lexer:token-type (current-token state))
                                    '(:newline)))
                 do (advance-token state))
           ;; Allow trailing comma
           (when (and (current-token state)
                      (eq (lexer:token-type (current-token state)) :right-bracket))
             (advance-token state)
             (return)))

          (t
           (error 'types:toml-parse-error
                  :message (format nil "Expected comma or ] in array but got ~A"
                                   (lexer:token-type token))
                  :line (lexer:token-line token)
                  :column (lexer:token-column token))))))

    ;; Return as vector (reversed since we pushed)
    (coerce (nreverse elements) 'vector)))

(defun parse-value (state)
  "Parse a value from current token"
  (let ((token (current-token state)))
    (unless token
      (error 'types:toml-parse-error
             :message "Expected value but got end of input"))

    (case (lexer:token-type token)
      ;; Array
      (:left-bracket
       (parse-array state))

      ;; Simple values
      ((:boolean :integer :float :string)
       (advance-token state)
       (lexer:token-value token))

      ;; Error cases
      (:bare-key
       (error 'types:toml-parse-error
              :message (format nil "Unexpected bare key in value position: ~A"
                               (lexer:token-value token))
              :line (lexer:token-line token)
              :column (lexer:token-column token)))

      (t
       (error 'types:toml-parse-error
              :message (format nil "Unexpected token in value position: ~A"
                               (lexer:token-type token))
              :line (lexer:token-line token)
              :column (lexer:token-column token))))))

;;; Key-Value Pair Parsing

(defun parse-key (state)
  "Parse a key (bare key or quoted string)"
  (let ((token (current-token state)))
    (unless token
      (error 'types:toml-parse-error
             :message "Expected key but got end of input"))
    (unless (member (lexer:token-type token) '(:bare-key :string))
      (error 'types:toml-parse-error
             :message (format nil "Expected key but got ~A" (lexer:token-type token))
             :line (lexer:token-line token)
             :column (lexer:token-column token)))
    (prog1 (lexer:token-value token)
      (advance-token state))))

(defun parse-key-value (state)
  "Parse a key = value line"
  (let ((key (parse-key state)))
    (expect-token state :equals)
    (let ((value (parse-value state)))
      ;; Store in result hash table
      (setf (gethash key (parser-state-result state)) value))))

;;; Main Parser Entry Points

(defun parse (string)
  "Parse a TOML v1.0.0 string and return a hash table."
  (check-type string string)
  (let* ((tokens (lexer:lex string))
         (state (make-parser-state :tokens tokens)))
    ;; Parse all key-value pairs
    (loop
      (skip-newlines state)
      (let ((token (current-token state)))
        (unless token
          (return))
        (when (eq (lexer:token-type token) :eof)
          (return))
        ;; For now, only handle simple key-value pairs
        (cond
          ((member (lexer:token-type token) '(:bare-key :string))
           (parse-key-value state))
          (t
           (error 'types:toml-parse-error
                  :message (format nil "Unexpected token: ~A" (lexer:token-type token))
                  :line (lexer:token-line token)
                  :column (lexer:token-column token)))))
      (skip-newlines state))
    (parser-state-result state)))

(defun parse-file (filename)
  "Parse a TOML v1.0.0 file and return a hash table."
  (parse (uiop:read-file-string filename)))
