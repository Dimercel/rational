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
  (when (null action) (setf action #'identity))
  `(.bind
    (progn ,@forms)
    (lambda (x) (.identity (make-token :id ,token-id :val (funcall ,action x))))))

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
