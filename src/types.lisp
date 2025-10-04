(defpackage #:tomlet/types
  (:use #:cl)
  (:export #:toml-error
           #:toml-error-message
           #:toml-parse-error
           #:toml-parse-error-line
           #:toml-parse-error-column
           #:local-date
           #:local-date-p
           #:make-local-date
           #:local-date-year
           #:local-date-month
           #:local-date-day
           #:local-time
           #:local-time-p
           #:make-local-time
           #:local-time-hour
           #:local-time-minute
           #:local-time-second
           #:local-time-nanosecond
           #:local-datetime
           #:local-datetime-p
           #:make-local-datetime
           #:local-datetime-year
           #:local-datetime-month
           #:local-datetime-day
           #:local-datetime-hour
           #:local-datetime-minute
           #:local-datetime-second
           #:local-datetime-nanosecond
           #:offset-datetime
           #:offset-datetime-p
           #:make-offset-datetime
           #:offset-datetime-year
           #:offset-datetime-month
           #:offset-datetime-day
           #:offset-datetime-hour
           #:offset-datetime-minute
           #:offset-datetime-second
           #:offset-datetime-nanosecond
           #:offset-datetime-offset))
(in-package #:tomlet/types)

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

;; Date/Time types for TOML v1.0.0

(defstruct local-date
  "Represents a date without time or timezone (e.g., 1979-05-27)"
  (year 0 :type integer)
  (month 1 :type (integer 1 12))
  (day 1 :type (integer 1 31)))

(defstruct local-time
  "Represents a time without date or timezone (e.g., 07:32:00)"
  (hour 0 :type (integer 0 23))
  (minute 0 :type (integer 0 59))
  (second 0 :type (integer 0 60))
  (nanosecond 0 :type (integer 0 999999999)))

(defstruct local-datetime
  "Represents a date-time without timezone (e.g., 1979-05-27T07:32:00)"
  (year 0 :type integer)
  (month 1 :type (integer 1 12))
  (day 1 :type (integer 1 31))
  (hour 0 :type (integer 0 23))
  (minute 0 :type (integer 0 59))
  (second 0 :type (integer 0 60))
  (nanosecond 0 :type (integer 0 999999999)))

(defstruct offset-datetime
  "Represents a date-time with timezone offset (e.g., 1979-05-27T07:32:00Z)"
  (year 0 :type integer)
  (month 1 :type (integer 1 12))
  (day 1 :type (integer 1 31))
  (hour 0 :type (integer 0 23))
  (minute 0 :type (integer 0 59))
  (second 0 :type (integer 0 60))
  (nanosecond 0 :type (integer 0 999999999))
  (offset 0 :type integer))
