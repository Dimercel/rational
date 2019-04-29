(defpackage rational.utils
  (:use :cl :smug)
  (:export :.one-of
           :.none-of
           :.many
           :.many1
           :.concs
           :.take-while
           :.concl))

(in-package :rational.utils)

(defun .one-of (string)
  (.is (lambda (x) (find x string))))

(defun .none-of (string)
  (.is-not (lambda (x) (find x string))))

(defun .many1 (parser &optional (result-type 'string))
  (.first (.map result-type parser :at-least 1)))

(defun .many (parser &optional (result-type 'string))
  (.first (.map result-type parser :at-least 0)))

(defun .concs (&rest parsers)
  (apply #'.concatenate (cons 'string parsers)))

(defun .concl (&rest parsers)
  (apply #'.concatenate (cons 'list parsers)))

(defun take-while (pred list)
  (loop for item in list
        while (funcall pred item)
        collect item))
