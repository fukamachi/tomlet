(uiop:define-package #:tomlex
  (:use #:cl)
  (:local-nicknames (#:types #:tomlex/types))
  (:use-reexport #:tomlex/types)
  (:export #:parse
           #:parse-file))
(in-package #:tomlex)

(defun parse (string)
  "Parse a TOML v1.0.0 string and return a hash table."
  (check-type string string)
  (error "Not implemented yet"))

(defun parse-file (filename)
  "Parse a TOML v1.0.0 file and return a hash table."
  (parse (uiop:read-file-string filename)))
