(defpackage #:tomlet/float-utils
  (:use #:cl)
  (:export #:double-float-positive-infinity
           #:double-float-negative-infinity
           #:double-float-nan
           #:float-infinity-p
           #:float-nan-p))
(in-package #:tomlet/float-utils)

;;; Portable special float values for Common Lisp

(defconstant double-float-positive-infinity
  #+sbcl sb-ext:double-float-positive-infinity
  #+ccl 1D++0
  #+ecl ext:long-float-positive-infinity
  #+clasp ext:double-float-positive-infinity
  #+abcl ext:double-float-positive-infinity
  #+allegro excl:*infinity-double*
  #+lispworks (progn
                (let ((bits (byte 64 0)))
                  (sys:make-typed-aref-vector 8)))
  #-(or sbcl ccl ecl clasp abcl allegro lispworks)
  (error "Positive infinity not supported on this implementation")
  "Positive infinity as a double-float")

(defconstant double-float-negative-infinity
  #+sbcl sb-ext:double-float-negative-infinity
  #+ccl -1D++0
  #+ecl ext:long-float-negative-infinity
  #+clasp ext:double-float-negative-infinity
  #+abcl ext:double-float-negative-infinity
  #+allegro excl:*negative-infinity-double*
  #+lispworks (- double-float-positive-infinity)
  #-(or sbcl ccl ecl clasp abcl allegro lispworks)
  (error "Negative infinity not supported on this implementation")
  "Negative infinity as a double-float")

(defconstant double-float-nan
  #+sbcl
  (let ((bits #x7ff8000000000000))
    (sb-kernel:make-double-float (ldb (byte 32 32) bits)
                                  (ldb (byte 32 0) bits)))
  #+ccl
  (ccl::nan-double)
  #+ecl
  (si::nan)
  #+clasp
  ext:double-float-nan
  #+abcl
  (/ 0.0d0 0.0d0)
  #+allegro
  excl::*nan-double*
  #+lispworks
  (sys:make-typed-aref-vector 8)
  #-(or sbcl ccl ecl clasp abcl allegro lispworks)
  (error "NaN not supported on this implementation")
  "Not-a-Number as a double-float")

(defun float-infinity-p (number)
  "Check if NUMBER is infinity (positive or negative)"
  (and (floatp number)
       #+sbcl (sb-ext:float-infinity-p number)
       #+ccl (ccl::infinity-p number)
       #+ecl (ext:float-infinity-p number)
       #+clasp (ext:float-infinity-p number)
       #+abcl (system::float-infinity-p number)
       #+allegro (excl::infinityp number)
       #+lispworks (sys:float-infinity-p number)
       #-(or sbcl ccl ecl clasp abcl allegro lispworks)
       (or (eql number double-float-positive-infinity)
           (eql number double-float-negative-infinity))))

(defun float-nan-p (number)
  "Check if NUMBER is NaN (Not-a-Number)"
  (and (floatp number)
       #+sbcl (sb-ext:float-nan-p number)
       #+ccl (ccl::nan-or-infinity-p number)
       #+ecl (ext:float-nan-p number)
       #+clasp (ext:float-nan-p number)
       #+abcl (system::float-nan-p number)
       #+allegro (excl::nanp number)
       #+lispworks (sys:float-nan-p number)
       #-(or sbcl ccl ecl clasp abcl allegro lispworks)
       (/= number number))) ; NaN is the only float that doesn't equal itself
