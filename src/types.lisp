(defpackage #:tomlex/types
  (:use #:cl)
  (:export #:toml-error
           #:toml-error-message
           #:toml-parse-error
           #:toml-parse-error-line
           #:toml-parse-error-column))
(in-package #:tomlex/types)

;; Error conditions
(define-condition toml-error (error)
  ((message :initarg :message
            :reader toml-error-message))
  (:report (lambda (condition stream)
             (format stream "TOML Error: ~A" (toml-error-message condition)))))

(define-condition toml-parse-error (toml-error)
  ((line :initarg :line
         :initform nil
         :reader toml-parse-error-line)
   (column :initarg :column
           :initform nil
           :reader toml-parse-error-column))
  (:report (lambda (condition stream)
             (format stream "TOML Parse Error~@[ at line ~D~]~@[, column ~D~]: ~A"
                     (toml-parse-error-line condition)
                     (toml-parse-error-column condition)
                     (toml-error-message condition)))))
