(defpackage rational
  (:use #:cl #:rational.core)
  (:import-from #:rational.parser
                #:parse-expr
                #:make-token
                #:token-id
                #:token-val
                #:operatorp)
  (:import-from #:rational.utils
                #:take-while))
(in-package :rational)


(defun op-lvl (token)
  (cond
    ((eq (token-id token) :add) 0)
    ((eq (token-id token) :minus) 0)
    ((eq (token-id token) :multi) 1)
    ((eq (token-id token) :div) 1)))

(defun to-postfix (expr &optional (stack nil) (result nil))
  "Преобразует выражение из инфиксной в постфиксную запись"
  (let ((token (first expr)))
    (if token
        (cond
          ((eq (token-id token) :frac)
           (to-postfix (rest expr) stack (append result (list token))))
          ((operatorp token)
           (let ((pop-op (take-while (lambda (x)
                                       (>= (op-lvl x) (op-lvl token)))
                                     stack)))
             (to-postfix
              (rest expr)
              (cons token (subseq stack (length pop-op)))
              (append result pop-op)))))
        (append result stack))))

(defun execute-op (op x y)
  (cond
    ((eq (token-id op) :add) (add x y))
    ((eq (token-id op) :minus) (sub x y))
    ((eq (token-id op) :multi) (mul x y))
    ((eq (token-id op) :div) (div x y))))

(defun calculate (expr)
  "Вычисляет результат выражения,находящиеся в постфиксной записи"
  (let ((stack nil))
    (dolist (token expr)
      (if (eq (token-id token) :frac)
          (push token stack)
          (let ((op-2 (token-val (pop stack)))
                (op-1 (token-val (pop stack))))
            (push (make-token :id :frac
                              :val (execute-op token op-1 op-2))
                  stack))))
    stack))
