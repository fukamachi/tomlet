(defpackage #:tomlet
  (:use #:cl)
  (:local-nicknames
   (#:types #:tomlet/types)
   (#:lexer #:tomlet/lexer)
   (#:float-utils #:tomlet/float-utils))
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
  (:import-from #:tomlet/float-utils
                #:double-float-positive-infinity
                #:double-float-negative-infinity
                #:double-float-nan
                #:float-infinity-p
                #:float-nan-p)
  (:export #:parse
           #:parse-file
           ;; Re-export from tomlet/types
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
           #:offset-datetime-offset
           ;; Re-export from tomlet/float-utils
           #:double-float-positive-infinity
           #:double-float-negative-infinity
           #:double-float-nan
           #:float-infinity-p
           #:float-nan-p))
(in-package #:tomlet)

;;; Parser State

(defstruct parser-state
  "Parser state for tracking position and context"
  (lexer nil :type (or null lexer:lexer))
  (current-token nil :type (or null lexer:token))
  (result (make-hash-table :test 'equal) :type hash-table)
  (current-table-path '() :type list)
  ;; Track which paths are arrays of tables (for [[array.of.tables]])
  (array-table-paths (make-hash-table :test 'equal) :type hash-table)
  ;; Track which tables were created/extended via dotted keys
  ;; Key: table path string, Value: t if defined via dotted keys
  (dotted-key-paths (make-hash-table :test 'equal) :type hash-table)
  ;; Track which tables were explicitly defined with [table] headers
  ;; Key: table path string, Value: t if defined with [table] header
  (explicit-table-paths (make-hash-table :test 'equal) :type hash-table)
  ;; Track which tables are inline tables (immutable)
  (inline-table-paths (make-hash-table :test 'equal) :type hash-table))

(defun current-token (state)
  "Get current token without advancing"
  (parser-state-current-token state))

(defun advance-token (state &key (context :value))
  "Advance to next token with given context (:key or :value)"
  (setf (parser-state-current-token state)
        (lexer:next-token (parser-state-lexer state) :context context)))

(defun expect-token (state expected-type &key (next-context :value))
  "Expect a token of a specific type, error if not found.
   NEXT-CONTEXT specifies the context for lexing the next token (:key or :value)."
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
      (advance-token state :context next-context))))

(defun skip-newlines (state)
  "Skip any newline tokens.
   After newlines at top level, next token should be a key or table header."
  (loop while (and (current-token state)
                   (eq (lexer:token-type (current-token state)) :newline))
        do (advance-token state :context :key)))

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

(defun mark-inline-table (state base-path table)
  "Recursively mark a table and all its nested tables as inline tables"
  (setf (gethash (format nil "~{~A~^.~}" base-path)
                 (parser-state-inline-table-paths state))
        t)
  (loop for key being the hash-keys of table using (hash-value value)
        when (hash-table-p value)
        do (mark-inline-table state (append base-path (list key)) value)))

(defun parse-inline-table (state)
  "Parse an inline table {key = value, ...}"
  (expect-token state :left-brace :next-context :key)
  (let ((table (make-hash-table :test 'equal))
        (defined-keys (make-hash-table :test 'equal))) ; Track keys with explicit values
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
                (multiple-value-bind (existing key-exists-p)
                    (gethash key table)
                  (declare (ignore existing))
                  (when key-exists-p
                    (error 'types:toml-parse-error
                           :message (format nil "Duplicate key in inline table: ~A" key)))
                  (setf (gethash key table) value)
                  (setf (gethash key defined-keys) t)))
              ;; Dotted key - create nested tables
              (let* ((parent-keys (butlast key-path))
                     (final-key (car (last key-path))))
                ;; Check if any prefix of this dotted key was already defined as a value
                (loop for keys-tail on key-path
                      for prefix = (ldiff key-path (rest keys-tail))
                      for prefix-str = (format nil "~{~A~^.~}" prefix)
                      when (gethash prefix-str defined-keys)
                      do (error 'types:toml-parse-error
                                :message (format nil "Cannot extend key ~A which was already defined in inline table" prefix-str)))

                (let ((parent-table (get-or-create-table table parent-keys)))
                  (multiple-value-bind (existing key-exists-p)
                      (gethash final-key parent-table)
                    (declare (ignore existing))
                    (when key-exists-p
                      (error 'types:toml-parse-error
                             :message (format nil "Duplicate key in inline table: ~A" (format nil "~{~A~^.~}" key-path))))
                    (setf (gethash final-key parent-table) value)
                    (setf (gethash (format nil "~{~A~^.~}" key-path) defined-keys) t)))))))

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
           (advance-token state :context :key))

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
         (advance-token state :context :key)))

      ;; Boolean keywords (true/false) can be keys
      ((eq (lexer:token-type token) :boolean)
       (prog1 (if (lexer:token-value token) "true" "false")
         (advance-token state :context :key)))

      ;; Float keywords (inf/nan) can be keys
      ((and (eq (lexer:token-type token) :float)
            (let ((val (lexer:token-value token)))
              (or (float-utils:float-infinity-p val)
                  (float-utils:float-nan-p val))))
       (prog1 (cond
                ((and (float-utils:float-infinity-p (lexer:token-value token))
                      (plusp (lexer:token-value token)))
                 "inf")
                ((and (float-utils:float-infinity-p (lexer:token-value token))
                      (minusp (lexer:token-value token)))
                 "-inf")
                ((float-utils:float-nan-p (lexer:token-value token))
                 "nan")
                (t (error 'types:toml-parse-error
                         :message "Unexpected float value as key")))
         (advance-token state :context :key)))

      ;; Integer/float/datetime as bare key (will now be lexed as bare-key in key context)
      ((member (lexer:token-type token) '(:integer :float :datetime))
       (prog1 (format nil "~A" (lexer:token-value token))
         (advance-token state :context :key)))

      (t
       (error 'types:toml-parse-error
              :message (format nil "Expected key but got ~A" (lexer:token-type token))
              :line (lexer:token-line token)
              :column (lexer:token-column token))))))

(defun parse-dotted-key (state)
  "Parse a dotted key (a.b.c) and return a list of key parts"
  (cons (parse-key state)
        (loop while (and (current-token state)
                         (eq (lexer:token-type (current-token state)) :dot))
              do (advance-token state :context :key)  ; skip dot, next will be key
              collect (parse-key state))))

(defun get-or-create-table (table key-path)
  "Navigate/create nested tables for a dotted key path"
  (if (null key-path)
      table
      (let ((key (first key-path)))
        (multiple-value-bind (existing key-exists-p)
            (gethash key table)
          (cond
            ;; Key exists and is a hash table - continue navigation
            ((hash-table-p existing)
             (get-or-create-table existing (rest key-path)))

            ;; Key doesn't exist - create new table
            ((not key-exists-p)
             (let ((new-table (make-hash-table :test 'equal)))
               (setf (gethash key table) new-table)
               (get-or-create-table new-table (rest key-path))))

            ;; Key exists but isn't a table - error
            (t
             (error 'types:toml-parse-error
                    :message (format nil "Cannot redefine key ~A as table" key))))))))


(defun parse-key-value (state)
  "Parse a key = value line (supports dotted keys)"
  (let ((key-path (parse-dotted-key state)))
    (expect-token state :equals)
    (let ((value (parse-value state))
          (current-table (get-current-table state))
          (full-path (append (parser-state-current-table-path state) key-path)))
      ;; Enforce newline or EOF after value
      (let ((next-token (current-token state)))
        (when (and next-token
                   (not (eq (lexer:token-type next-token) :newline))
                   (not (eq (lexer:token-type next-token) :eof)))
          (error 'types:toml-parse-error
                 :message "Expected newline or EOF after key-value pair"
                 :line (lexer:token-line next-token)
                 :column (lexer:token-column next-token))))
      (if (= (length key-path) 1)
          ;; Simple key - assign to current table
          (let ((key (first key-path)))
            (multiple-value-bind (existing key-exists-p)
                (gethash key current-table)
              (declare (ignore existing))
              (when key-exists-p
                (error 'types:toml-parse-error
                       :message (format nil "Duplicate key: ~A" key)))
              (setf (gethash key current-table) value)
              ;; Mark inline tables
              (when (hash-table-p value)
                (mark-inline-table state full-path value))))
          ;; Dotted key - navigate from current table and set final key
          (let* ((parent-keys (butlast key-path))
                 (final-key (car (last key-path)))
                 (base-path (parser-state-current-table-path state)))
            ;; Check if any intermediate path was explicitly defined with [table] header
            (loop for keys-tail on parent-keys
                  for sub-path = (append base-path (ldiff parent-keys (rest keys-tail)))
                  for path-str = (format nil "~{~A~^.~}" sub-path)
                  when (gethash path-str (parser-state-explicit-table-paths state))
                  do (error 'types:toml-parse-error
                            :message (format nil "Cannot extend table [~A] via dotted keys" path-str)))
            ;; Check if any intermediate path is an inline table (immutable)
            (loop for keys-tail on parent-keys
                  for sub-path = (append base-path (ldiff parent-keys (rest keys-tail)))
                  for path-str = (format nil "~{~A~^.~}" sub-path)
                  when (gethash path-str (parser-state-inline-table-paths state))
                  do (error 'types:toml-parse-error
                            :message (format nil "Cannot extend inline table ~A" path-str)))
            (let ((parent-table (get-or-create-table current-table parent-keys)))
              ;; Mark all intermediate paths as created via dotted keys
              (loop for keys-tail on parent-keys
                    for sub-path = (append base-path (ldiff parent-keys (rest keys-tail)))
                    for path-str = (format nil "~{~A~^.~}" sub-path)
                    do (setf (gethash path-str (parser-state-dotted-key-paths state)) t))
              (multiple-value-bind (existing key-exists-p)
                  (gethash final-key parent-table)
                (declare (ignore existing))
                (when key-exists-p
                  (error 'types:toml-parse-error
                         :message (format nil "Duplicate key: ~A" (format nil "~{~A~^.~}" key-path))))
                (setf (gethash final-key parent-table) value)
                ;; Mark inline tables
                (when (hash-table-p value)
                  (mark-inline-table state full-path value)))))))))

;;; Table Section Parsing

(defun parse-table-header (state)
  "Parse a table header [section.name] or [[array.of.tables]]"
  (let ((first-bracket (current-token state)))
    (expect-token state :left-bracket :next-context :key)

    ;; Check if this is an array of tables [[...]]
    (let ((is-array-table nil))
      (when (and (current-token state)
                 (eq (lexer:token-type (current-token state)) :left-bracket))
        ;; Validate no whitespace between brackets
        (let ((second-bracket (current-token state)))
          (unless (and (= (lexer:token-line first-bracket) (lexer:token-line second-bracket))
                       (= (lexer:token-column second-bracket) (1+ (lexer:token-column first-bracket))))
            (error 'types:toml-parse-error
                   :message "Array-of-tables brackets [[...]] must not have whitespace between them"
                   :line (lexer:token-line second-bracket)
                   :column (lexer:token-column second-bracket))))
        (setf is-array-table t)
        (advance-token state :context :key))

      (let ((key-path (parse-dotted-key state)))
        (let ((first-close-bracket (current-token state)))
          (expect-token state :right-bracket)

          ;; If array table, expect another right bracket with no whitespace
          (when is-array-table
            (let ((second-close-bracket (current-token state)))
              (unless (and (= (lexer:token-line first-close-bracket) (lexer:token-line second-close-bracket))
                           (= (lexer:token-column second-close-bracket) (1+ (lexer:token-column first-close-bracket))))
                (error 'types:toml-parse-error
                       :message "Array-of-tables brackets [[...]] must not have whitespace between them"
                       :line (lexer:token-line second-close-bracket)
                       :column (lexer:token-column second-close-bracket))))
            (expect-token state :right-bracket))

          ;; Enforce newline or EOF after table header
          (let ((next-token (current-token state)))
            (when (and next-token
                       (not (eq (lexer:token-type next-token) :newline))
                       (not (eq (lexer:token-type next-token) :eof)))
              (error 'types:toml-parse-error
                     :message "Expected newline or EOF after table header"
                     :line (lexer:token-line next-token)
                     :column (lexer:token-column next-token))))

          (values key-path is-array-table))))))

(defun navigate-table-header (state key-path)
  "Navigate to table specified by header [key-path].
   Handles array-of-tables in the path by navigating to last elements.
   This is used for [table.header] sections, NOT for key-value pairs."
  (loop with table = (parser-state-result state)
        for keys-tail on key-path
        for key = (first keys-tail)
        for path-so-far = (ldiff key-path (rest keys-tail))
        for path-str = (format nil "~{~A~^.~}" path-so-far)
        for is-array-table = (gethash path-str (parser-state-array-table-paths state))
        for is-inline-table = (gethash path-str (parser-state-inline-table-paths state))
        for existing = (gethash key table)
        do (when is-inline-table
             (error 'types:toml-parse-error
                    :message (format nil "Cannot extend inline table ~A" path-str)))
           (cond
             ;; This path is an array-table that already exists
             ;; Navigate into the last element of the array
             (is-array-table
              (unless (vectorp existing)
                (error 'types:toml-parse-error
                       :message (format nil "Expected array at ~A" key)))
              (when (zerop (length existing))
                (error 'types:toml-parse-error
                       :message (format nil "Cannot navigate into empty array ~A" key)))
              (setf table (aref existing (1- (length existing)))))

             ;; Regular table exists - navigate into it
             ((hash-table-p existing)
              (setf table existing))

             ;; Need to create new table
             ((null existing)
              (let ((new-table (make-hash-table :test 'equal)))
                (setf (gethash key table) new-table)
                (setf table new-table)))

             ;; Error: key exists but is wrong type (not table or array)
             (t
              (error 'types:toml-parse-error
                     :message (format nil "Cannot redefine ~A as table" key))))
        finally (return table)))

(defun set-current-table (state key-path)
  "Set the current table context for subsequent key-value pairs"
  (let ((path-str (format nil "~{~A~^.~}" key-path)))
    ;; Check if this table was already created via dotted keys
    (when (gethash path-str (parser-state-dotted-key-paths state))
      (error 'types:toml-parse-error
             :message (format nil "Cannot define table [~A] after it was created via dotted keys" path-str)))

    ;; Check if this table is an array-of-tables
    (when (gethash path-str (parser-state-array-table-paths state))
      (error 'types:toml-parse-error
             :message (format nil "Cannot redefine array-of-tables [[~A]] as regular table" path-str)))

    ;; Check if this table was already explicitly defined with [table] header
    (when (gethash path-str (parser-state-explicit-table-paths state))
      (error 'types:toml-parse-error
             :message (format nil "Cannot redefine table [~A]" path-str)))

    ;; Mark this table as explicitly defined
    (setf (gethash path-str (parser-state-explicit-table-paths state)) t)

    (setf (parser-state-current-table-path state) key-path)
    ;; Use array-aware navigation for table headers
    (navigate-table-header state key-path)))

(defun set-current-array-table (state key-path)
  "Set current context to a new element in an array of tables"
  (setf (parser-state-current-table-path state) key-path)

  ;; Mark this path as an array table
  (let ((path-str (format nil "~{~A~^.~}" key-path)))
    (setf (gethash path-str (parser-state-array-table-paths state)) t)

    ;; Clear explicit-table-paths and dotted-key-paths for descendants of this array table
    ;; Each [[array]] element creates a fresh scope for sub-tables
    (let ((prefix-len (1+ (length path-str))))  ; +1 for the dot
      (loop for key being the hash-keys of (parser-state-explicit-table-paths state)
            when (and (> (length key) prefix-len)
                      (string= path-str key :end2 (length path-str))
                      (char= (char key (length path-str)) #\.))
            do (remhash key (parser-state-explicit-table-paths state)))
      (loop for key being the hash-keys of (parser-state-dotted-key-paths state)
            when (and (> (length key) prefix-len)
                      (string= path-str key :end2 (length path-str))
                      (char= (char key (length path-str)) #\.))
            do (remhash key (parser-state-dotted-key-paths state)))))

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
      (cond
        ((null array)
         ;; Create new array
         (setf array (make-array 0 :adjustable t :fill-pointer 0))
         (setf (gethash final-key parent-table) array))
        ((not (vectorp array))
         ;; Key exists but isn't an array
         (error 'types:toml-parse-error
                :message (format nil "Cannot redefine ~A as array of tables" final-key)))
        ((and (vectorp array) (not (adjustable-array-p array)))
         ;; Key exists as a regular (non-adjustable) array, not an array-of-tables
         (error 'types:toml-parse-error
                :message (format nil "Cannot redefine static array ~A as array of tables" final-key))))

      ;; Append a new table to the array
      (let ((new-table (make-hash-table :test 'equal)))
        (vector-push-extend new-table array)
        new-table))))

(defun path-has-array-table-prefix (path array-table-paths)
  "Check if any prefix of path is an array-table"
  (loop for path-tail on path
        for prefix = (ldiff path (rest path-tail))
        for prefix-str = (format nil "~{~A~^.~}" prefix)
        when (gethash prefix-str array-table-paths)
        return t))

(defun get-current-table (state)
  "Get the current table for key-value assignments"
  (if (null (parser-state-current-table-path state))
      (parser-state-result state)
      (let ((path (parser-state-current-table-path state)))
        ;; Check if this path or any of its prefixes is an array table
        (if (path-has-array-table-prefix path (parser-state-array-table-paths state))
            ;; Navigate through the path, handling array tables
            ;; For nested arrays like [[fruit.variety]], we need to navigate through parent arrays
            (loop with table = (parser-state-result state)
                  for path-tail on path
                  for key = (first path-tail)
                  for path-so-far = (ldiff path (rest path-tail))
                  do (if (gethash (format nil "~{~A~^.~}" path-so-far)
                                  (parser-state-array-table-paths state))
                         ;; This level is an array table - get last element
                         (let ((array (gethash key table)))
                           (when (and array (vectorp array) (> (length array) 0))
                             (setf table (aref array (1- (length array))))))
                         ;; Regular table - navigate normally
                         (setf table (gethash key table)))
                  finally (return table))
            ;; Regular table - no arrays in path
            (get-or-create-table (parser-state-result state) path)))))

;;; Main Parser Entry Points

(defun parse (string)
  "Parse a TOML v1.0.0 string and return a hash table."
  (check-type string string)
  ;; Note: UTF-8 validation happens at the file reading layer in SBCL.
  ;; Invalid UTF-8 byte sequences cause stream decoding errors before
  ;; the string reaches this function, so no additional validation is needed.
  (let* ((lex (lexer:make-lexer :input string))
         (state (make-parser-state :lexer lex)))
    ;; Get the first token - use :key context since top-level is either table header or key
    (advance-token state :context :key)
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
