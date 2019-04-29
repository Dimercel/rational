(defpackage rational.parser
  (:use :cl :smug :rational.utils)
  (:import-from :rational.core :make-fraction)
  (:export :parse-expr
           :operatorp))
(in-package :rational.parser)


(defstruct token
  id
  val)

(defun operatorp (token)
  (if (member (token-id token) (list :add :minus :multi :div))
      t
      nil))

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

(defun parse-expr (expr)
  (parse (.expr) expr))
