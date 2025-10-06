(defpackage #:tomlet/tests
  (:use #:cl
        #:rove)
  (:local-nicknames
   (#:float-utils #:tomlet/float-utils)))
(in-package #:tomlet/tests)

;;; ===========================================================================
;;; Official TOML Test Suite Integration
;;; ===========================================================================

(defparameter *test-dir*
  (asdf:system-relative-pathname :tomlet "tests/toml-test/tests/"))

(defparameter *toml-1.0.0-files*
  (asdf:system-relative-pathname :tomlet "tests/toml-test/tests/files-toml-1.0.0"))

(defun read-test-file-list ()
  "Read the list of TOML 1.0.0 test files."
  (with-open-file (stream *toml-1.0.0-files*)
    (loop for line = (read-line stream nil)
          while line
          when (uiop:string-suffix-p line ".toml")
          collect line)))

(defun parse-toml-type (type-str)
  "Convert TOML type string to symbol."
  (cond
    ((string= type-str "string") :string)
    ((string= type-str "integer") :integer)
    ((string= type-str "float") :float)
    ((string= type-str "bool") :bool)
    ((string= type-str "datetime") :datetime)
    ((string= type-str "datetime-local") :datetime-local)
    ((string= type-str "date-local") :date-local)
    ((string= type-str "time-local") :time-local)
    (t (error "Unknown TOML type: ~A" type-str))))

(defun toml-value-equal (expected actual expected-type)
  "Compare TOML values according to their type."
  (ecase expected-type
    (:string
     (string= expected actual))
    (:integer
     (= (parse-integer expected) actual))
    (:float
     (let ((expected-float (let ((*read-default-float-format* 'double-float))
                              (read-from-string expected))))
       (cond
         ((string= expected "inf") (and (floatp actual) (float-utils:float-infinity-p actual) (plusp actual)))
         ((string= expected "-inf") (and (floatp actual) (float-utils:float-infinity-p actual) (minusp actual)))
         ((string= expected "nan") (and (floatp actual) (float-utils:float-nan-p actual)))
         ((string= expected "+inf") (and (floatp actual) (float-utils:float-infinity-p actual) (plusp actual)))
         ((string= expected "-nan") (and (floatp actual) (float-utils:float-nan-p actual)))
         ((string= expected "+nan") (and (floatp actual) (float-utils:float-nan-p actual)))
         (t (< (abs (- expected-float actual)) 0.00001)))))
    (:bool
     (eq (string= expected "true") actual))
    (:datetime
     (typep actual 'tomlet/types:offset-datetime))
    (:datetime-local
     (typep actual 'tomlet/types:local-datetime))
    (:date-local
     (typep actual 'tomlet/types:local-date))
    (:time-local
     (typep actual 'tomlet/types:local-time))))

(defun compare-toml-structures (expected actual)
  "Recursively compare expected JSON structure with actual parsed TOML."
  (cond
    ;; Expected is a typed value (from JSON {"type": "...", "value": "..."})
    ;; Check this FIRST before general hash table check
    ((and (hash-table-p expected)
          (= (hash-table-count expected) 2)
          (gethash "type" expected)
          (gethash "value" expected))
     (let ((expected-type (parse-toml-type (gethash "type" expected)))
           (expected-value (gethash "value" expected)))
       (toml-value-equal expected-value actual expected-type)))

    ;; Expected is a hash table (TOML table)
    ((hash-table-p expected)
     (unless (hash-table-p actual)
       (return-from compare-toml-structures nil))
     (when (/= (hash-table-count expected) (hash-table-count actual))
       (return-from compare-toml-structures nil))
     (loop for key being the hash-keys of expected
           using (hash-value expected-value)
           always (and (nth-value 1 (gethash key actual))
                      (compare-toml-structures expected-value (gethash key actual)))))

    ;; Expected is a vector (TOML array)
    ((vectorp expected)
     (unless (vectorp actual)
       (return-from compare-toml-structures nil))
     (when (/= (length expected) (length actual))
       (return-from compare-toml-structures nil))
     (loop for i from 0 below (length expected)
           always (compare-toml-structures (aref expected i) (aref actual i))))

    ;; Direct comparison
    (t (equal expected actual))))

(defun json-to-toml-expected (json-obj)
  "Convert JSON object to expected TOML structure."
  (cond
    ;; Hash table with type and value - this is a typed value
    ((and (hash-table-p json-obj)
          (= (hash-table-count json-obj) 2)
          (gethash "type" json-obj)
          (gethash "value" json-obj))
     json-obj)

    ;; Regular hash table - recursively convert values
    ((hash-table-p json-obj)
     (let ((result (make-hash-table :test 'equal)))
       (loop for key being the hash-keys of json-obj
             using (hash-value value)
             do (setf (gethash key result) (json-to-toml-expected value)))
       result))

    ;; Array - recursively convert elements
    ((vectorp json-obj)
     (map 'vector #'json-to-toml-expected json-obj))

    ;; Leave as is
    (t json-obj)))

(defun run-valid-test (test-path)
  "Run a single valid test case."
  #+abcl
  ;; ABCL: char-code-limit of 65536 (16-bit, BMP only)
  (when (or (search "quoted-unicode" test-path)
            (search "multibyte-escape" test-path))
    (skip (format nil "~A: skipped on ABCL (16-bit char-code-limit)" test-path))
    (return-from run-valid-test))
  #+ecl
  ;; ECL: STREAM-DECODING-ERROR when reading files with high codepoints
  (when (search "comment/nonascii" test-path)
    (skip (format nil "~A: skipped on ECL (cannot handle codepoints >U+FFFF)" test-path))
    (return-from run-valid-test))
  (let* ((toml-file (merge-pathnames test-path *test-dir*))
         (json-file (merge-pathnames (uiop:strcat (subseq test-path 0 (- (length test-path) 5)) ".json")
                                     *test-dir*))
         (toml-content (uiop:read-file-string toml-file))
         #+ecl
         (expected-json (let ((yason:*parse-json-arrays-as-vectors* t))
                          (yason:parse (uiop:read-file-string json-file))))
         #-ecl
         (expected-json (com.inuoe.jzon:parse (uiop:read-file-string json-file)))
         (expected (json-to-toml-expected expected-json)))
    (let ((actual (tomlet:parse toml-content)))
      (ok (compare-toml-structures expected actual)
          (format nil "~A: structures match" test-path)))))

(defun run-invalid-test (test-path)
  "Run a single invalid test case - parser should signal an error."
  #+abcl
  ;; ABCL doesn't properly reject invalid UTF-8 during file reading,
  ;; and also cannot handle high Unicode codepoints (char-code-limit 65536)
  (when (or (search "bad-utf8" test-path)
            (search "bad-codepoint" test-path))
    (skip (format nil "~A: skipped on ABCL (lenient UTF-8 or 16-bit char limit)" test-path))
    (return-from run-invalid-test))
  (let* ((toml-file (merge-pathnames test-path *test-dir*))
         (toml-content (handler-case
                           (uiop:read-file-string toml-file)
                         (error ()
                           ;; File read error (e.g., UTF-8 decode error) counts as invalid
                           (ok t (format nil "~A: correctly rejected (file read error)" test-path))
                           (return-from run-invalid-test)))))
    (ok (signals (tomlet:parse toml-content) 'tomlet:toml-error)
        (format nil "~A: signals toml-error" test-path))))

;;; ===========================================================================
;;; Main Test Entry
;;; ===========================================================================

(deftest official-toml-test-suite
  (testing "TOML 1.0.0 valid test cases"
    (let* ((all-tests (read-test-file-list))
           (valid-tests (remove-if-not (lambda (path) (uiop:string-prefix-p "valid/" path)) all-tests)))
      (dolist (test-path valid-tests)
        (run-valid-test test-path))))

  (testing "TOML 1.0.0 invalid test cases"
    (let* ((all-tests (read-test-file-list))
           (invalid-tests (remove-if-not (lambda (path) (uiop:string-prefix-p "invalid/" path)) all-tests)))
      (dolist (test-path invalid-tests)
        (run-invalid-test test-path)))))

;;; ===========================================================================
;;; Individual Unit Tests (kept for debugging specific features)
;;; ===========================================================================

(deftest test-boolean-values
  (testing "Boolean true"
    (let ((result (tomlet:parse "key = true")))
      (ok (hash-table-p result))
      (ok (eq (gethash "key" result) t))))

  (testing "Boolean false"
    (let ((result (tomlet:parse "key = false")))
      (ok (hash-table-p result))
      (ok (eq (gethash "key" result) nil)))))

(deftest test-integer-basic
  (testing "Simple integer"
    (let ((result (tomlet:parse "answer = 42")))
      (ok (= (gethash "answer" result) 42)))))

(deftest test-string-basic
  (testing "Simple string"
    (let ((result (tomlet:parse "key = \"value\"")))
      (ok (string= (gethash "key" result) "value")))))
