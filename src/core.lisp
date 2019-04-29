(defpackage rational.core
  (:use :cl)
  (:export :make-fraction
           :fraction-num
           :fraction-denom
           :add
           :sub
           :mul
           :div
           :reduce-fraction))
(in-package :rational.core)


(defstruct fraction
  (num 1)
  (denom 1))

(defun reduce-fraction (frac)
  (let ((gcd-val (gcd (fraction-num frac)
                      (fraction-denom frac))))
    (make-fraction :num (/ (fraction-num frac) gcd-val)
                   :denom (/ (fraction-denom frac) gcd-val))))

(defun add (x y)
  (reduce-fraction
   (make-fraction :num (+ (* (fraction-num x) (fraction-denom y))
                          (* (fraction-num y) (fraction-denom x)))
                  :denom (* (fraction-denom x) (fraction-denom y)))))

(defun sub (x y)
  (add x (make-fraction :num (- (fraction-num y))
                        :denom (fraction-denom y))))

(defun mul (x y)
  (reduce-fraction
   (make-fraction :num (* (fraction-num x) (fraction-num y))
                  :denom (* (fraction-denom x) (fraction-denom y)))))

(defun div (x y)
  (mul x (make-fraction :num (fraction-denom y)
                        :denom (fraction-num y))))
