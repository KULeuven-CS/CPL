#lang eopl
(provide (all-defined-out))

;;; Syntax
(define-datatype program program?
  (a-program (exp1 expression?)))

(define-datatype expression expression?
  (const-exp (num number?))
  (diff-exp (exp1 expression?) (exp2 expression?))
  (minus-exp (exp1 expression?)) ; 3.6 addition
  (zero?-exp (exp1 expression?))
  (equal?-exp (exp1 expression?) (exp2 expression?)) ; 3.8 addition
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp (var symbol?))
  (let-exp
   (var symbol?)
   (exp1 expression?)
   (body expression?))
  (emptylist-exp)
  (cons-exp ; 3.9 addition
   (exp1 expression?)
   (exp2 expression?))
  (car-exp
   (body expression?))
  (cdr-exp
   (body expression?))
  (null?-exp
   (body expression?)))

(define (program->string pgm)
     (cases program pgm
      (a-program (exp1)
                 (exp->string exp1 ))))

(define (exp->string exp)
    (cases expression exp
      (const-exp (num) (number->string num))
      (var-exp (var) (symbol->string var))
      (diff-exp (exp1 exp2)
                (let ((val1 (exp->string exp1))
                      (val2 (exp->string exp2)))
                  (string-append "(" val1 " - " val2 ")")))
      (minus-exp (exp1) ;3.6 addition
                 (let ((val1 (exp->string exp1))) (string-append "(- " val1 ")")))
      (zero?-exp (exp1)
                 (let ((val1 (exp->string exp1 )))
                   (string-append "zero? " val1)))
      (equal?-exp (exp1 exp2) ;3.8 addition
                  (let ((val1 (exp->string exp1))
                        (val2 (exp->string exp2)))
                    (string-append "equal? " val1 " " val2)))
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (exp->string exp1 ))
                    (val2 (exp->string exp2 ))
                    (val3 (exp->string exp3 )))
                (string-append "if " val1 " then " val2 " else " val3 )))
              
      (let-exp (var exp1 body)       
               (let ((val1 (exp->string exp1))
                     (val2 (exp->string body)))
                 (string-append "let " (symbol->string var) " = " val1 " in " val2) ))
      (emptylist-exp () ;3.9 addition
               (string-append "emptylist"))
      (cons-exp (exp1 exp2)
                (let ((val1 (exp->string exp1))
                      (val2 (exp->string exp2)))
		  (string-append "cons " val1 " " val2)))
      (car-exp (body)
               (let ((val1 (exp->string body)))
                     (string-append "car " val1)))
      (cdr-exp (body)
               (let ((val1 (exp->string body)))
                     (string-append "cdr " val1)))
      (null?-exp (exp)
	       (let ((val1 (exp->string exp)))
                     (string-append "null? " val1)))
	   ))