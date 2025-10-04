(defpackage #:tomlet
  (:use #:cl)
  (:local-nicknames
   (#:types #:tomlet/types)
   (#:lexer #:tomlet/lexer))
  (:import-from #:tomlet/types
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
(in-package #:tomlet)

;;; Parser State

(defstruct parser-state
  "Parser state for tracking position and context"
  (tokens '() :type list)
  (position 0 :type fixnum)
  (result (make-hash-table :test 'equal) :type hash-table)
  (current-table-path '() :type list)
  ;; Track which paths are arrays of tables (for [[array.of.tables]])
  (array-table-paths (make-hash-table :test 'equal) :type hash-table))

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

(defun parse-inline-table (state)
  "Parse an inline table {key = value, ...}"
  (expect-token state :left-brace)
  (let ((table (make-hash-table :test 'equal)))
    ;; Empty inline table check
    (when (and (current-token state)
               (eq (lexer:token-type (current-token state)) :right-brace))
      (advance-token state)
      (return-from parse-inline-table table))

    ;; Parse key-value pairs
    (loop
      (let ((key-path (parse-dotted-key state)))
        (expect-token state :equals)
        (let ((value (parse-value state)))
          (if (= (length key-path) 1)
              ;; Simple key
              (let ((key (first key-path)))
                (when (nth-value 1 (gethash key table))
                  (error 'types:toml-parse-error
                         :message (format nil "Duplicate key in inline table: ~A" key)))
                (setf (gethash key table) value))
              ;; Dotted key - create nested tables
              (let* ((parent-keys (butlast key-path))
                     (final-key (car (last key-path)))
                     (parent-table (get-or-create-table table parent-keys)))
                (setf (gethash final-key parent-table) value)))))

      (let ((token (current-token state)))
        (unless token
          (error 'types:toml-parse-error
                 :message "Unterminated inline table"))

        (cond
          ;; End of inline table
          ((eq (lexer:token-type token) :right-brace)
           (advance-token state)
           (return))

          ;; Comma - continue to next pair
          ((eq (lexer:token-type token) :comma)
           (advance-token state))

          (t
           (error 'types:toml-parse-error
                  :message (format nil "Expected comma or } in inline table but got ~A"
                                   (lexer:token-type token))
                  :line (lexer:token-line token)
                  :column (lexer:token-column token))))))

    table))

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

      ;; Inline table
      (:left-brace
       (parse-inline-table state))

      ;; Simple values
      ((:boolean :integer :float :string :datetime)
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
  "Parse a key (bare key, quoted string, or keyword like inf/nan/true/false)"
  (let ((token (current-token state)))
    (unless token
      (error 'types:toml-parse-error
             :message "Expected key but got end of input"))
    (cond
      ;; Regular bare key or string
      ((member (lexer:token-type token) '(:bare-key :string))
       (prog1 (lexer:token-value token)
         (advance-token state)))

      ;; Boolean keywords (true/false) can be keys
      ((eq (lexer:token-type token) :boolean)
       (prog1 (if (lexer:token-value token) "true" "false")
         (advance-token state)))

      ;; Float keywords (inf/nan) can be keys
      ((and (eq (lexer:token-type token) :float)
            (let ((val (lexer:token-value token)))
              (or (sb-ext:float-infinity-p val)
                  (sb-ext:float-nan-p val))))
       (prog1 (cond
                ((and (sb-ext:float-infinity-p (lexer:token-value token))
                      (plusp (lexer:token-value token)))
                 "inf")
                ((and (sb-ext:float-infinity-p (lexer:token-value token))
                      (minusp (lexer:token-value token)))
                 "-inf")
                ((sb-ext:float-nan-p (lexer:token-value token))
                 "nan")
                (t (error 'types:toml-parse-error
                         :message "Unexpected float value as key")))
         (advance-token state)))

      ;; Integer/float as bare key (unquoted numbers like "1", "3.14")
      ((member (lexer:token-type token) '(:integer :float :datetime))
       (prog1 (format nil "~A" (lexer:token-value token))
         (advance-token state)))

      (t
       (error 'types:toml-parse-error
              :message (format nil "Expected key but got ~A" (lexer:token-type token))
              :line (lexer:token-line token)
              :column (lexer:token-column token))))))

(defun parse-dotted-key (state)
  "Parse a dotted key (a.b.c) and return a list of key parts"
  (let ((keys (list (parse-key state))))
    (loop while (and (current-token state)
                     (eq (lexer:token-type (current-token state)) :dot))
          do (advance-token state)  ; skip dot
             (push (parse-key state) keys))
    (nreverse keys)))

(defun get-or-create-table (table key-path)
  "Navigate/create nested tables for a dotted key path"
  (if (null key-path)
      table
      (let* ((key (first key-path))
             (existing (gethash key table)))
        (cond
          ;; Key exists and is a hash table - continue navigation
          ((hash-table-p existing)
           (get-or-create-table existing (rest key-path)))

          ;; Key doesn't exist - create new table
          ((null existing)
           (let ((new-table (make-hash-table :test 'equal)))
             (setf (gethash key table) new-table)
             (get-or-create-table new-table (rest key-path))))

          ;; Key exists but isn't a table - error
          (t
           (error 'types:toml-parse-error
                  :message (format nil "Cannot redefine key ~A as table" key)))))))

(defun parse-key-value (state)
  "Parse a key = value line (supports dotted keys)"
  (let ((key-path (parse-dotted-key state)))
    (expect-token state :equals)
    (let ((value (parse-value state))
          (current-table (get-current-table state)))
      (if (= (length key-path) 1)
          ;; Simple key - assign to current table
          (setf (gethash (first key-path) current-table) value)
          ;; Dotted key - navigate from current table and set final key
          (let* ((parent-keys (butlast key-path))
                 (final-key (car (last key-path)))
                 (parent-table (get-or-create-table current-table parent-keys)))
            (setf (gethash final-key parent-table) value))))))

;;; Table Section Parsing

(defun parse-table-header (state)
  "Parse a table header [section.name] or [[array.of.tables]]"
  (expect-token state :left-bracket)

  ;; Check if this is an array of tables [[...]]
  (let ((is-array-table nil))
    (when (and (current-token state)
               (eq (lexer:token-type (current-token state)) :left-bracket))
      (setf is-array-table t)
      (advance-token state))

    (let ((key-path (parse-dotted-key state)))
      (expect-token state :right-bracket)

      ;; If array table, expect another right bracket
      (when is-array-table
        (expect-token state :right-bracket))

      (values key-path is-array-table))))

(defun set-current-table (state key-path)
  "Set the current table context for subsequent key-value pairs"
  (setf (parser-state-current-table-path state) key-path)
  ;; Ensure the table exists
  (get-or-create-table (parser-state-result state) key-path))

(defun set-current-array-table (state key-path)
  "Set current context to a new element in an array of tables"
  (setf (parser-state-current-table-path state) key-path)

  ;; Mark this path as an array table
  (setf (gethash (format nil "~{~A~^.~}" key-path)
                 (parser-state-array-table-paths state))
        t)

  ;; Navigate to the parent table (if any)
  ;; For nested array tables like [[fruit.variety]], we need to get the last
  ;; element of the parent array [[fruit]]
  (let ((parent-table (if (> (length key-path) 1)
                          (let ((parent-path (butlast key-path)))
                            ;; Check if parent is an array table
                            (if (gethash (format nil "~{~A~^.~}" parent-path)
                                        (parser-state-array-table-paths state))
                                ;; Parent is an array table - get last element
                                (let ((parent-parent (if (> (length parent-path) 1)
                                                        (get-or-create-table (parser-state-result state)
                                                                            (butlast parent-path))
                                                        (parser-state-result state)))
                                      (parent-key (car (last parent-path))))
                                  (let ((parent-array (gethash parent-key parent-parent)))
                                    (when (and parent-array (vectorp parent-array) (> (length parent-array) 0))
                                      (aref parent-array (1- (length parent-array))))))
                                ;; Parent is a regular table
                                (get-or-create-table (parser-state-result state) parent-path)))
                          (parser-state-result state)))
        (final-key (car (last key-path))))

    ;; Get or create the array at this key
    (let ((array (gethash final-key parent-table)))
      (unless array
        ;; Create new array
        (setf array (make-array 0 :adjustable t :fill-pointer 0))
        (setf (gethash final-key parent-table) array))

      ;; Ensure it's actually an array
      (unless (vectorp array)
        (error 'types:toml-parse-error
               :message (format nil "Cannot redefine ~A as array of tables" final-key)))

      ;; Append a new table to the array
      (let ((new-table (make-hash-table :test 'equal)))
        (vector-push-extend new-table array)
        new-table))))

(defun get-current-table (state)
  "Get the current table for key-value assignments"
  (if (null (parser-state-current-table-path state))
      (parser-state-result state)
      (let ((path (parser-state-current-table-path state)))
        ;; Check if this is an array table path
        (if (gethash (format nil "~{~A~^.~}" path)
                     (parser-state-array-table-paths state))
            ;; Return the last element of the array
            ;; For nested arrays like [[fruit.variety]], we need to navigate through parent arrays
            (let* ((table (parser-state-result state))
                   (i 0))
              (loop for key in path
                    do (progn
                         (incf i)
                         (let ((path-so-far (subseq path 0 i)))
                           (if (gethash (format nil "~{~A~^.~}" path-so-far)
                                       (parser-state-array-table-paths state))
                               ;; This level is an array table - get last element
                               (let ((array (gethash key table)))
                                 (when (and array (vectorp array) (> (length array) 0))
                                   (setf table (aref array (1- (length array))))))
                               ;; Regular table - navigate normally
                               (setf table (gethash key table))))))
              table)
            ;; Regular table
            (get-or-create-table (parser-state-result state) path)))))

;;; Main Parser Entry Points

(defun parse (string)
  "Parse a TOML v1.0.0 string and return a hash table."
  (check-type string string)
  ;; Note: UTF-8 validation happens at the file reading layer in SBCL.
  ;; Invalid UTF-8 byte sequences cause stream decoding errors before
  ;; the string reaches this function, so no additional validation is needed.
  (let* ((tokens (lexer:lex string))
         (state (make-parser-state :tokens tokens)))
    ;; Parse document
    (loop
      (skip-newlines state)
      (let ((token (current-token state)))
        (unless token
          (return))
        (when (eq (lexer:token-type token) :eof)
          (return))
        (cond
          ;; Table header [section] or [[array.of.tables]]
          ((eq (lexer:token-type token) :left-bracket)
           (multiple-value-bind (key-path is-array-table)
               (parse-table-header state)
             (if is-array-table
                 (set-current-array-table state key-path)
                 (set-current-table state key-path))))

          ;; Key-value pair (including keywords, numbers, datetimes as keys)
          ((member (lexer:token-type token) '(:bare-key :string :boolean :float :integer :datetime))
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
