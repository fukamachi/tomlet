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
  ;; Read the NaN literal representation
  (read-from-string "#.(sb-int:with-float-traps-masked (:invalid) (/ 0.0d0 0.0d0))"))

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
         (make-token :type :float :value sb-ext:double-float-positive-infinity :line line :column col))
        ((string= text "-inf")
         (make-token :type :float :value sb-ext:double-float-negative-infinity :line line :column col))
        ((or (string= text "nan") (string= text "+nan") (string= text "-nan"))
         (make-token :type :float :value (make-nan) :line line :column col))
        (t
         (make-token :type :bare-key :value text :line line :column col))))))

;;; Number and datetime lexing

(defun split-datetime (text)
  "Split datetime string on T, t, or space separator. Returns (date-part time-part)"
  (let ((sep-pos (or (position #\T text)
                     (position #\t text)
                     (position #\Space text))))
    (if sep-pos
        (list (subseq text 0 sep-pos)
              (subseq text (1+ sep-pos)))
        (list text nil))))

(defun parse-datetime-string (text)
  "Parse a datetime string into appropriate struct"
  (cond
    ;; Offset datetime: YYYY-MM-DD(T| )HH:MM:SS+HH:MM or with Z
    ;; Check for Z, +, or - in the time portion (after T/space)
    ((and (or (find #\T text) (find #\t text) (find #\Space text))
          (find #\: text)
          (let* ((parts (split-datetime text))
                 (time-part (second parts)))
            (and time-part
                 (or (find #\Z time-part)
                     (find #\z time-part)
                     (find #\+ time-part)
                     ;; - in time part (offset), not in date part
                     (and (find #\- time-part)
                          ;; Make sure it's not just a negative time (check it's after position 2)
                          (> (position #\- time-part :from-end t) 2))))))
     (let* ((date-time-parts (split-datetime text))
            (date-part (first date-time-parts))
            (time-offset-part (second date-time-parts))
            (date-components (mapcar #'parse-integer (uiop:split-string date-part :separator "-")))
            ;; Find offset marker from right (after seconds/fraction)
            (offset-pos (position-if (lambda (c) (member c '(#\+ #\- #\Z #\z)))
                                     time-offset-part
                                     :from-end t
                                     :start 2))  ; Skip first 2 chars (HH)
            (time-str (if offset-pos
                         (subseq time-offset-part 0 offset-pos)
                         time-offset-part))
            (offset-str (if offset-pos
                           (subseq time-offset-part offset-pos)
                           ""))
            (time-components (parse-time-components time-str))
            (offset (if (string= offset-str "")
                       0
                       (if (or (string= offset-str "Z") (string= offset-str "z"))
                           0
                           (parse-timezone-offset offset-str)))))
       (types:make-offset-datetime
        :year (first date-components)
        :month (second date-components)
        :day (third date-components)
        :hour (first time-components)
        :minute (second time-components)
        :second (third time-components)
        :nanosecond (fourth time-components)
        :offset offset)))

    ;; Local datetime: YYYY-MM-DD(T| )HH:MM:SS
    ((and (or (find #\T text) (find #\t text) (find #\Space text))
          (find #\: text))
     (let* ((date-time-parts (split-datetime text))
            (date-part (first date-time-parts))
            (time-part (second date-time-parts))
            (date-components (mapcar #'parse-integer (uiop:split-string date-part :separator "-")))
            (time-components (parse-time-components time-part)))
       (types:make-local-datetime
        :year (first date-components)
        :month (second date-components)
        :day (third date-components)
        :hour (first time-components)
        :minute (second time-components)
        :second (third time-components)
        :nanosecond (fourth time-components))))

    ;; Local date: YYYY-MM-DD
    ((and (find #\- text) (not (find #\: text)))
     (let ((components (mapcar #'parse-integer (uiop:split-string text :separator "-"))))
       (types:make-local-date
        :year (first components)
        :month (second components)
        :day (third components))))

    ;; Local time: HH:MM:SS
    ((and (find #\: text) (not (find #\- text)) (not (find #\T text)))
     (let ((components (parse-time-components text)))
       (types:make-local-time
        :hour (first components)
        :minute (second components)
        :second (third components)
        :nanosecond (fourth components))))

    (t
     (error 'types:toml-parse-error
            :message (format nil "Invalid datetime format: ~A" text)))))

(defun parse-time-components (time-str)
  "Parse HH:MM:SS[.fraction] into (hour minute second nanosecond)"
  (let* ((has-fraction (find #\. time-str))
         (main-part (if has-fraction
                       (subseq time-str 0 (position #\. time-str))
                       time-str))
         (fraction-part (if has-fraction
                           (subseq time-str (1+ (position #\. time-str)))
                           "0"))
         (components (mapcar #'parse-integer (uiop:split-string main-part :separator ":")))
         (nanoseconds (parse-fractional-seconds fraction-part)))
    (list (first components)
          (second components)
          (third components)
          nanoseconds)))

(defun parse-fractional-seconds (frac-str)
  "Parse fractional seconds string to nanoseconds"
  (if (string= frac-str "0")
      0
      (let* ((padded (format nil "~A~V@{~A~:*~}" frac-str (- 9 (length frac-str)) "0")))
        (parse-integer (subseq padded 0 9)))))

(defun parse-timezone-offset (offset-str)
  "Parse timezone offset (+HH:MM or -HH:MM) to minutes"
  (let* ((sign (if (char= (char offset-str 0) #\+) 1 -1))
         (parts (uiop:split-string (subseq offset-str 1) :separator ":"))
         (hours (parse-integer (first parts)))
         (minutes (if (second parts) (parse-integer (second parts)) 0)))
    (* sign (+ (* hours 60) minutes))))

(defun parse-number-string (text)
  "Parse a number string (with underscores removed) to integer or float"
  (let ((clean (remove #\_ text)))
    (cond
      ;; Hex integer
      ((and (>= (length clean) 3)
            (char= (char clean 0) #\0)
            (or (char= (char clean 1) #\x) (char= (char clean 1) #\X)))
       (parse-integer clean :start 2 :radix 16))

      ;; Octal integer
      ((and (>= (length clean) 3)
            (char= (char clean 0) #\0)
            (or (char= (char clean 1) #\o) (char= (char clean 1) #\O)))
       (parse-integer clean :start 2 :radix 8))

      ;; Binary integer
      ((and (>= (length clean) 3)
            (char= (char clean 0) #\0)
            (or (char= (char clean 1) #\b) (char= (char clean 1) #\B)))
       (parse-integer clean :start 2 :radix 2))

      ;; Float (contains . or e/E)
      ((or (find #\. clean) (find #\e clean) (find #\E clean))
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
         (make-token :type :float :value sb-ext:double-float-positive-infinity :line line :column col))
        ((string= text "-inf")
         (make-token :type :float :value sb-ext:double-float-negative-infinity :line line :column col))
        ((or (string= text "nan") (string= text "+nan") (string= text "-nan"))
         (make-token :type :float :value (make-nan) :line line :column col))

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
                    (make-token :type :float :value sb-ext:double-float-positive-infinity :line line :column col))
                   ((string= text "-inf")
                    (make-token :type :float :value sb-ext:double-float-negative-infinity :line line :column col))
                   ((or (string= text "nan") (string= text "+nan") (string= text "-nan"))
                    (make-token :type :float :value (make-nan) :line line :column col))
                   ;; If number parsing fails and text looks like a bare key, return as bare key
                   ((every (lambda (c) (or (alphanumericp c) (char= c #\-) (char= c #\_) (char= c #\+))) text)
                    (make-token :type :bare-key :value text :line line :column col))
                   (t
                    (error 'types:toml-parse-error
                           :message (format nil "Invalid number format: ~A (~A)" text e)
                           :line line
                           :column col)))))))))))
