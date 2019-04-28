(defpackage rational
  (:use :cl :smug :rational.utils))
(in-package :rational)


(defstruct fraction
  (num 1)
  (denom 1))

(defstruct token
  id
  val)

(defmacro with-token (token-id action &body forms)
  (if (null action)
      `(.bind
        (progn ,@forms)
        (lambda (x) (.identity (make-token :id ,token-id :val x))))
      `(.bind
        (progn ,@forms)
        (lambda (x) (.identity (make-token :id ,token-id :val (funcall ,action x)))))))

(defun .ws ()
  (.first (.many (.is 'member '(#\space #\tab)))))

(defun .number ()
  (with-token :number 'parse-integer
    (.or (.string= "0")
         (.concs
          (.bind (.one-of "123456789")
                 (lambda (x) (.identity (string x))))
          (.many (.is #'digit-char-p))))))

(defun .fraction ()
  (with-token :frac nil
    (.let* ((num (.number))
            (_ (.char= #\/))
            (denom (.number)))
      (.identity (make-fraction :num (token-val num) :denom (token-val denom))))))

(defun .addition ()
  (with-token :add nil
    (.char= #\+)))

(defun .minus ()
  (with-token :minus nil
    (.char= #\-)))

(defun .multi ()
  (with-token :multi nil
    (.char= #\*)))

(defun .div ()
  (with-token :div nil
    (.char= #\:)))

(defun .operation ()
  (.let* ((_ (.ws))
          (op (.or (.addition)
                   (.minus)
                   (.multi)
                   (.div)))
          (_ (.ws)))
    (.identity op)))

(defun .expr ()
  (.let* ((head (.fraction))
          (tail (.many (.let* ((op (.operation))
                               (f (.fraction)))
                         (.identity (list op f)))
                       'list)))
        (.identity (cons head tail))))

(defun reduce-fraction (fraction)
  (let ((gcd-val (gcd (fraction-num fraction)
                      (fraction-denom fraction))))
    (make-fraction :num (/ (fraction-num fraction) gcd-val)
                   :denom (/ (fraction-denom fraction) gcd-val))))
