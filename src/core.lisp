(defpackage rational.core
  (:use :cl)
  (:export :make-fraction
           :reduce-fraction))
(in-package :rational.core)


(defstruct fraction
  (num 1)
  (denom 1))

(defun reduce-fraction (fraction)
  (let ((gcd-val (gcd (fraction-num fraction)
                      (fraction-denom fraction))))
    (make-fraction :num (/ (fraction-num fraction) gcd-val)
                   :denom (/ (fraction-denom fraction) gcd-val))))
