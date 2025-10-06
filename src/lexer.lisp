(defpackage #:tomlet/lexer
  (:use #:cl)
  (:local-nicknames
   (#:types #:tomlet/types)
   (#:float-utils #:tomlet/float-utils)
   (#:ppcre #:cl-ppcre))
  (:export #:token
           #:make-token
           #:token-type
           #:token-value
           #:token-line
           #:token-column
           #:lexer
           #:make-lexer
           #:next-token
           #:lex
           #:lex-string))
(in-package #:tomlet/lexer)

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

(defun next-token (lexer &key (context :value))
  "Read the next token from the lexer.
   CONTEXT is :key or :value - affects how ambiguous tokens are lexed.
   In :key context, numbers and dates are lexed conservatively as bare keys.
   In :value context, numbers and dates are parsed normally."
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
       (next-token lexer :context context))

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
      ;; In key context, lex conservatively as bare keys
      ((or (digit-char-p ch) (char= ch #\+) (char= ch #\-))
       (if (eq context :key)
           (lex-bare-key-conservative lexer line col)
           (lex-number-or-datetime lexer line col)))

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

(defun is-control-char-p (ch)
  "Check if character is a control character (U+0000 to U+001F, U+007F) except tab"
  (and (characterp ch)
       (let ((code (char-code ch)))
         (or (and (>= code 0) (<= code 31) (not (= code 9)))  ; U+0000-U+001F except tab
             (= code 127)))))  ; DEL (U+007F)

(defun skip-comment (lexer)
  "Skip from # to end of line, validating no control characters"
  (loop while (and (not (at-end-p lexer))
                   (not (char= (current-char lexer) #\Newline)))
        do (let ((ch (current-char lexer)))
             ;; Control characters not allowed in comments (except tab)
             (when (is-control-char-p ch)
               (error 'types:toml-parse-error
                      :message (format nil "Control character U+~4,'0X not allowed in comment"
                                       (char-code ch))
                      :line (lexer-line lexer)
                      :column (lexer-column lexer)))
             (advance lexer))))

;;; String lexing with escape sequences

(defun handle-multiline-closing-quotes (lexer quote-char)
  "Handle closing quotes in multiline strings using greedy quote counting.
Returns (values :closed content-quotes) if string is closed, where content-quotes is the number of quotes to add.
Returns (values :continue nil) if quotes are part of content (caller should re-lex them)."
  (let ((quote-count 0)
        (saved-pos (lexer-position lexer)))
    ;; Count consecutive quotes
    (loop while (and (not (at-end-p lexer))
                     (char= (current-char lexer) quote-char))
          do (incf quote-count)
             (advance lexer))

    (if (>= quote-count 3)
        ;; Found closing delimiter
        ;; Return number of content quotes (quote-count - 3)
        (values :closed (- quote-count 3))
        ;; Less than 3 quotes - restore position and caller will handle them
        (progn
          (setf (lexer-position lexer) saved-pos)
          (values :continue quote-count)))))

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
                 ;; Use greedy quote counting for multiline strings
                 (multiple-value-bind (status content-quotes)
                     (handle-multiline-closing-quotes lexer #\")
                   (if (eq status :closed)
                       (progn
                         ;; Add content quotes and close string
                         (dotimes (i content-quotes)
                           (push #\" chars))
                         (return))
                       ;; Less than 3 quotes - add them as content
                       (dotimes (i content-quotes)
                         (push (advance lexer) chars))))
                 ;; Single-line string - done
                 (progn
                   (advance lexer)
                   (return))))

            ;; Escape sequences
            ((char= ch #\\)
             (advance lexer) ;; Skip backslash
             ;; Line-ending backslash in multi-line strings
             ;; Can have whitespace before the newline
             (if (and is-multiline
                      (not (at-end-p lexer)))
                 (let ((saved-pos (lexer-position lexer)))
                   ;; Try to skip whitespace until newline
                   (loop while (and (not (at-end-p lexer))
                                    (member (current-char lexer) '(#\Space #\Tab)))
                         do (advance lexer))
                   ;; Check if we found a newline (LF or CRLF)
                   (if (and (not (at-end-p lexer))
                            (or (char= (current-char lexer) #\Newline)
                                (char= (current-char lexer) #\Return)))
                       (progn
                         ;; Handle CRLF: skip \r if followed by \n
                         (when (char= (current-char lexer) #\Return)
                           (advance lexer)
                           (when (and (not (at-end-p lexer))
                                      (char= (current-char lexer) #\Newline))
                             (advance lexer)))
                         ;; Handle LF: skip \n
                         (when (and (not (at-end-p lexer))
                                    (char= (current-char lexer) #\Newline))
                           (advance lexer))
                         ;; Skip whitespace at beginning of next line
                         (loop while (and (not (at-end-p lexer))
                                          (member (current-char lexer) '(#\Space #\Tab #\Newline #\Return)))
                               do (advance lexer)))
                       ;; Not a line-ending backslash, restore position and parse escape
                       (progn
                         (setf (lexer-position lexer) saved-pos)
                         (push (lex-escape-sequence lexer) chars))))
                 ;; Regular escape sequence (not multiline)
                 (push (lex-escape-sequence lexer) chars)))

            ;; Newline in single-line string is error
            ((and (not is-multiline) (char= ch #\Newline))
             (error 'types:toml-parse-error
                    :message "Newline not allowed in single-line string"
                    :line line
                    :column col))

            ;; Control characters not allowed in strings (except tab and newlines in multiline strings)
            ((and (is-control-char-p ch)
                  (not (and is-multiline (char= ch #\Newline))))
             (error 'types:toml-parse-error
                    :message (format nil "Control character U+~4,'0X not allowed in string"
                                     (char-code ch))
                    :line (lexer-line lexer)
                    :column (lexer-column lexer)))

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
                 ;; Use greedy quote counting for multiline strings
                 (multiple-value-bind (status content-quotes)
                     (handle-multiline-closing-quotes lexer #\')
                   (if (eq status :closed)
                       (progn
                         ;; Add content quotes and close string
                         (dotimes (i content-quotes)
                           (push #\' chars))
                         (return))
                       ;; Less than 3 quotes - add them as content
                       (dotimes (i content-quotes)
                         (push (advance lexer) chars))))
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

            ;; Control characters not allowed in literal strings (except tab and newlines in multiline strings)
            ((and (is-control-char-p ch)
                  (not (and is-multiline (char= ch #\Newline))))
             (error 'types:toml-parse-error
                    :message (format nil "Control character U+~4,'0X not allowed in literal string"
                                     (char-code ch))
                    :line (lexer-line lexer)
                    :column (lexer-column lexer)))

            ;; Regular character (no escapes in literal strings)
            (t
             (push (advance lexer) chars)))))

      (make-token :type :string
                  :value (coerce (nreverse chars) 'string)
                  :line line
                  :column col))))

;;; Bare key and keyword lexing

(defun make-nan ()
  "Create a NaN value at runtime"
  float-utils:double-float-nan)

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
        ((or (string= text "inf") (string= text "+inf"))
         (make-token :type :float :value float-utils:double-float-positive-infinity :line line :column col))
        ((string= text "-inf")
         (make-token :type :float :value float-utils:double-float-negative-infinity :line line :column col))
        ((or (string= text "nan") (string= text "+nan") (string= text "-nan"))
         (make-token :type :float :value float-utils:double-float-nan :line line :column col))
        (t
         (make-token :type :bare-key :value text :line line :column col))))))

(defun lex-bare-key-conservative (lexer line col)
  "Lex a bare key without trying datetime/number parsing.
   Used in key context where '1.2' should be two keys '1' and '2', not a float.
   Collects alphanumeric characters, digits, hyphens, and underscores."
  (let ((chars '()))
    (loop while (and (not (at-end-p lexer))
                     (let ((ch (current-char lexer)))
                       (or (alphanumericp ch)
                           (char= ch #\-)
                           (char= ch #\_))))
          do (push (advance lexer) chars))
    (make-token :type :bare-key
                :value (coerce (nreverse chars) 'string)
                :line line
                :column col)))

;;; Number and datetime lexing

(defun parse-fractional-seconds (frac-str)
  "Parse fractional seconds string to nanoseconds"
  (if (or (null frac-str) (string= frac-str ""))
      0
      ;; Remove leading dot if present, pad to 9 digits
      (let* ((cleaned (if (char= (char frac-str 0) #\.)
                          (subseq frac-str 1)
                          frac-str))
             (padded (format nil "~A~V@{~A~:*~}" cleaned (- 9 (length cleaned)) "0")))
        (parse-integer (subseq padded 0 9)))))

(defun validate-datetime-ranges (year month day hour minute second offset-hour offset-minute)
  "Validate that datetime component values are in valid ranges"
  ;; Month: 01-12
  (unless (and (>= month 1) (<= month 12))
    (error 'types:toml-parse-error
           :message (format nil "Invalid month: ~D (must be 01-12)" month)))

  ;; Day: 01-31 (basic check, more detailed below)
  (unless (and (>= day 1) (<= day 31))
    (error 'types:toml-parse-error
           :message (format nil "Invalid day: ~D (must be 01-31)" day)))

  ;; Days per month validation
  (let ((days-in-month
          (cond
            ((member month '(1 3 5 7 8 10 12)) 31)
            ((member month '(4 6 9 11)) 30)
            ;; February - check for leap year
            ((= month 2)
             (if (or (and (= (mod year 4) 0) (/= (mod year 100) 0))
                     (= (mod year 400) 0))
                 29  ; Leap year
                 28))  ; Not a leap year
            (t 31))))
    (unless (<= day days-in-month)
      (error 'types:toml-parse-error
             :message (format nil "Invalid day ~D for month ~D in year ~D" day month year))))

  ;; Hour: 00-23
  (when hour
    (unless (and (>= hour 0) (<= hour 23))
      (error 'types:toml-parse-error
             :message (format nil "Invalid hour: ~D (must be 00-23)" hour))))

  ;; Minute: 00-59
  (when minute
    (unless (and (>= minute 0) (<= minute 59))
      (error 'types:toml-parse-error
             :message (format nil "Invalid minute: ~D (must be 00-59)" minute))))

  ;; Second: 00-60 (60 for leap seconds)
  (when second
    (unless (and (>= second 0) (<= second 60))
      (error 'types:toml-parse-error
             :message (format nil "Invalid second: ~D (must be 00-60)" second))))

  ;; Offset hour: -23 to +23
  (when offset-hour
    (unless (and (>= offset-hour -23) (<= offset-hour 23))
      (error 'types:toml-parse-error
             :message (format nil "Invalid offset hour: ~D (must be -23 to +23)" offset-hour))))

  ;; Offset minute: 00-59
  (when offset-minute
    (unless (and (>= offset-minute 0) (<= offset-minute 59))
      (error 'types:toml-parse-error
             :message (format nil "Invalid offset minute: ~D (must be 00-59)" offset-minute)))))

(defun parse-datetime-string (text)
  "Parse a datetime string into appropriate struct using regex matching"
  (or
   ;; Offset datetime: YYYY-MM-DDTHH:MM:SS(.fraction)?(Z|[+-]HH:MM)
   (ppcre:register-groups-bind ((#'parse-integer y mon d h min s) frac offset)
       ("^(\\d{4})-(\\d{2})-(\\d{2})[Tt ](\\d{2}):(\\d{2}):(\\d{2})(\\.\\d+)?([Zz]|[+-]\\d{2}:\\d{2})$" text)
     (let* ((ns (parse-fractional-seconds frac))
            (off-min (cond
                       ((or (string= offset "Z") (string= offset "z")) 0)
                       (t (ppcre:register-groups-bind (sign-str (#'parse-integer oh om))
                              ("^([+-])(\\d{2}):(\\d{2})$" offset)
                            (let ((sign (if (string= sign-str "+") 1 -1)))
                              ;; Validate offset ranges
                              (unless (and (>= oh 0) (<= oh 23))
                                (error 'types:toml-parse-error
                                       :message (format nil "Invalid offset hour: ~D" oh)))
                              (unless (and (>= om 0) (<= om 59))
                                (error 'types:toml-parse-error
                                       :message (format nil "Invalid offset minute: ~D" om)))
                              (* sign (+ (* oh 60) om))))))))
       ;; Validate datetime ranges
       (validate-datetime-ranges y mon d h min s
                                (when off-min (floor (abs off-min) 60))
                                (when off-min (mod (abs off-min) 60)))
       (types:make-offset-datetime :year y :month mon :day d
                                  :hour h :minute min :second s
                                  :nanosecond ns :offset off-min)))

   ;; Local datetime: YYYY-MM-DDTHH:MM:SS(.fraction)?
   (ppcre:register-groups-bind ((#'parse-integer y mon d h min s) frac)
       ("^(\\d{4})-(\\d{2})-(\\d{2})[Tt ](\\d{2}):(\\d{2}):(\\d{2})(\\.\\d+)?$" text)
     (let ((ns (parse-fractional-seconds frac)))
       (validate-datetime-ranges y mon d h min s nil nil)
       (types:make-local-datetime :year y :month mon :day d
                                 :hour h :minute min :second s
                                 :nanosecond ns)))

   ;; Local date: YYYY-MM-DD
   (ppcre:register-groups-bind ((#'parse-integer y mon d))
       ("^(\\d{4})-(\\d{2})-(\\d{2})$" text)
     (validate-datetime-ranges y mon d nil nil nil nil nil)
     (types:make-local-date :year y :month mon :day d))

   ;; Local time: HH:MM:SS(.fraction)?
   (ppcre:register-groups-bind ((#'parse-integer h min s) frac)
       ("^(\\d{2}):(\\d{2}):(\\d{2})(\\.\\d+)?$" text)
     (let ((ns (parse-fractional-seconds frac)))
       (validate-datetime-ranges 2000 1 1 h min s nil nil)  ; Dummy date
       (types:make-local-time :hour h :minute min :second s :nanosecond ns)))

   ;; No match - invalid format
   (error 'types:toml-parse-error
          :message (format nil "Invalid datetime format: ~A" text))))


(defun validate-and-clean-number (text)
  "Validate number format and return cleaned text (underscores removed).
   Returns (values cleaned-text is-float-p).
   Signals toml-parse-error if format is invalid."

  (let ((is-float (or (find #\. text) (find #\e text) (find #\E text))))

    (cond
      ;; Float validation
      (is-float
       ;; Valid float regex: [+-]?(0|[1-9](_?[0-9])*)(\.[0-9](_?[0-9])*)?([eE][+-]?[0-9](_?[0-9])*)?
       (unless (ppcre:scan "^[+-]?(0|[1-9](_?[0-9])*)(\\.[0-9](_?[0-9])*)?([eE][+-]?[0-9](_?[0-9])*)?$" text)
         (error 'types:toml-parse-error
                :message (format nil "Invalid float format: ~A" text)))
       (values (ppcre:regex-replace-all "_" text "") t))

      ;; Integer validation
      (t
       (cond
         ;; Hex: 0x[0-9a-fA-F](_?[0-9a-fA-F])*
         ((ppcre:scan "^0[xX]" text)
          (when (ppcre:scan "[XOB]" text)  ; Capital prefix check
            (error 'types:toml-parse-error
                   :message (format nil "Invalid integer: capital prefix in ~A" text)))
          (unless (ppcre:scan "^0x[0-9a-fA-F](_?[0-9a-fA-F])*$" text)
            (error 'types:toml-parse-error
                   :message (format nil "Invalid hex integer format: ~A" text))))

         ;; Octal: 0o[0-7](_?[0-7])*
         ((ppcre:scan "^0[oO]" text)
          (when (ppcre:scan "[XOB]" text)
            (error 'types:toml-parse-error
                   :message (format nil "Invalid integer: capital prefix in ~A" text)))
          (unless (ppcre:scan "^0o[0-7](_?[0-7])*$" text)
            (error 'types:toml-parse-error
                   :message (format nil "Invalid octal integer format: ~A" text))))

         ;; Binary: 0b[01](_?[01])*
         ((ppcre:scan "^0[bB]" text)
          (when (ppcre:scan "[XOB]" text)
            (error 'types:toml-parse-error
                   :message (format nil "Invalid integer: capital prefix in ~A" text)))
          (unless (ppcre:scan "^0b[01](_?[01])*$" text)
            (error 'types:toml-parse-error
                   :message (format nil "Invalid binary integer format: ~A" text))))

         ;; Decimal: [+-]?(0|[1-9](_?[0-9])*)
         (t
          (unless (ppcre:scan "^[+-]?(0|[1-9](_?[0-9])*)$" text)
            (error 'types:toml-parse-error
                   :message (format nil "Invalid decimal integer format: ~A" text)))))

       (values (ppcre:regex-replace-all "_" text "") nil)))))

(defun parse-number-string (text)
  "Parse a number string to integer or float"
  (multiple-value-bind (clean is-float)
      (validate-and-clean-number text)
    (cond
      ;; Hex integer
      ((and (>= (length clean) 3)
            (char= (char clean 0) #\0)
            (char= (char clean 1) #\x))
       (parse-integer clean :start 2 :radix 16))

      ;; Octal integer
      ((and (>= (length clean) 3)
            (char= (char clean 0) #\0)
            (char= (char clean 1) #\o))
       (parse-integer clean :start 2 :radix 8))

      ;; Binary integer
      ((and (>= (length clean) 3)
            (char= (char clean 0) #\0)
            (char= (char clean 1) #\b))
       (parse-integer clean :start 2 :radix 2))

      ;; Float
      (is-float
       (let ((*read-default-float-format* 'double-float))
         (read-from-string clean)))

      ;; Regular integer
      (t
       (parse-integer clean)))))

(defun lex-number-or-datetime (lexer line col)
  "Lex a number (integer/float) or datetime"
  (let ((chars '()))
    ;; Collect all characters that could be part of number or datetime
    (loop while (and (not (at-end-p lexer))
                     (or (digit-char-p (current-char lexer))
                         (member (current-char lexer)
                                '(#\+ #\- #\_ #\. #\e #\E #\x #\X #\o #\O #\b #\B #\: #\T #\t #\Z #\z
                                  #\a #\A #\b #\B #\c #\C #\d #\D #\e #\E #\f #\F
                                  #\i #\I #\n #\N)) ; for inf and nan
                         ;; Include space if it might be a datetime separator (date followed by time)
                         (and (char= (current-char lexer) #\Space)
                              (let ((next-ch (peek-char-at lexer 1)))
                                (and next-ch (digit-char-p next-ch))))))
          do (push (advance lexer) chars))

    (let ((text (coerce (nreverse chars) 'string)))
      ;; Check for special float values first
      (cond
        ((or (string= text "inf") (string= text "+inf"))
         (make-token :type :float :value float-utils:double-float-positive-infinity :line line :column col))
        ((string= text "-inf")
         (make-token :type :float :value float-utils:double-float-negative-infinity :line line :column col))
        ((or (string= text "nan") (string= text "+nan") (string= text "-nan"))
         (make-token :type :float :value float-utils:double-float-nan :line line :column col))

        ;; Try to discriminate between datetime and number
        ;; Datetime has format: YYYY-MM-DD, YYYY-MM-DDTHH:MM:SS, or HH:MM:SS
        ;; Date or datetime
        ;; Must have : for time, or - in date position (not after e/E for exponent)
        ((or (and (find #\: text)
                  (or (find #\T text)  ; datetime with T separator
                      (and (>= (length text) 8)  ; time-only HH:MM:SS
                           (digit-char-p (char text 0))
                           (= (count #\: text) 2))))
             ;; Date format: YYYY-MM-DD
             ;; Must start with digit (not + or -), and have - in proper position
             (and (>= (length text) 10)
                  (digit-char-p (char text 0))
                  (find #\- text)
                  ;; Ensure - appears in date position (after 4 digits), not as sign
                  (>= (position #\- text) 4)))
         ;; Parse datetime string into appropriate struct
         (handler-case
             (make-token :type :datetime
                         :value (parse-datetime-string text)
                         :line line
                         :column col)
           (error (e)
             ;; If datetime parsing fails, it might be a bare key like "2000-datetime"
             ;; Continue collecting remaining bare key characters
             (loop while (and (not (at-end-p lexer))
                              (or (alphanumericp (current-char lexer))
                                  (char= (current-char lexer) #\_)
                                  (char= (current-char lexer) #\-)))
                   do (push (advance lexer) chars))
             (let ((full-text (coerce (nreverse chars) 'string)))
               ;; Check if the full text looks like a valid bare key
               (if (every (lambda (c) (or (alphanumericp c) (char= c #\-) (char= c #\_))) full-text)
                   (make-token :type :bare-key :value full-text :line line :column col)
                   (error 'types:toml-parse-error
                          :message (format nil "Invalid datetime: ~A (~A)" text e)
                          :line line
                          :column col))))))

        ;; Number or bare key (if parsing fails and it looks like a key)
        (t
         ;; Check if there are more bare-key characters after the collected text
         ;; If so, this is a bare key like "1key", not a number
         (if (and (not (at-end-p lexer))
                  (or (alpha-char-p (current-char lexer))
                      (char= (current-char lexer) #\_)))
             ;; Continue collecting as bare key
             (progn
               (loop while (and (not (at-end-p lexer))
                                (or (alphanumericp (current-char lexer))
                                    (char= (current-char lexer) #\_)
                                    (char= (current-char lexer) #\-)))
                     do (push (advance lexer) chars))
               (let ((full-text (coerce (nreverse chars) 'string)))
                 (make-token :type :bare-key :value full-text :line line :column col)))
             ;; Try to parse as number
             (handler-case
                 (let ((value (parse-number-string text)))
                   (make-token :type (if (integerp value) :integer :float)
                               :value value
                               :line line
                               :column col))
               (error (e)
                 ;; Check for signed inf/nan
                 (cond
                   ((or (string= text "+inf") (string= text "inf"))
                    (make-token :type :float :value float-utils:double-float-positive-infinity :line line :column col))
                   ((string= text "-inf")
                    (make-token :type :float :value float-utils:double-float-negative-infinity :line line :column col))
                   ((or (string= text "nan") (string= text "+nan") (string= text "-nan"))
                    (make-token :type :float :value float-utils:double-float-nan :line line :column col))
                   ;; If number parsing fails and text looks like a bare key, return as bare key
                   ((every (lambda (c) (or (alphanumericp c) (char= c #\-) (char= c #\_) (char= c #\+))) text)
                    (make-token :type :bare-key :value text :line line :column col))
                   (t
                    (error 'types:toml-parse-error
                           :message (format nil "Invalid number format: ~A (~A)" text e)
                           :line line
                           :column col)))))))))))
