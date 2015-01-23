#lang eopl
(require racket)
(provide (all-defined-out))

;;; Syntax
(define-datatype program program?
  (a-program (exp1 expression?)))

(define-datatype expression expression?
  (const-exp (num number?))
  (diff-exp (exp1 expression?) (exp2 expression?))
  (unop-exp (oper unary-op?) (exp1 expression?)) ; Unary operators
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp (var symbol?))
  (let-exp
   (var symbol?)
   (exp1 expression?)
   (body expression?))
  (proc-exp
   (var symbol?)
   (body expression?))
  (call-exp
   (rator expression?)
   (rand expression?))
  (letrec-exp 
   (p-name symbol?)
   (b-var symbol?)
   (p-body expression?)
   (letrec-body expression?))
  (const-list-exp (nums (list-of number?)))   
  (try-exp
   (body expression?)
   (var symbol?)
   (handler expression?))
  (raise-exp (exp1 expression?)))
  
(define-datatype
   unary-op
   unary-op?
   (null?-unop)
   (car-unop)
   (cdr-unop)
   (zero?-unop))

(define (oper->string oper)
  (cases unary-op oper
    (null?-unop () "null?")
   (car-unop () "car")
   (cdr-unop () "cdr")
   (zero?-unop () "zero?")))

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
    (unop-exp (oper exp1)
               (let ((val1 (exp->string exp1 )))
                 (string-append (oper->string oper) "(" val1 ")")))
    (if-exp (exp1 exp2 exp3)
            (let ((val1 (exp->string exp1 ))
                  (val2 (exp->string exp2 ))
                  (val3 (exp->string exp3 )))
              (string-append "if " val1 " then " val2 " else " val3 )))
    
    (let-exp (var exp1 body)       
             (let ((val1 (exp->string exp1))
                   (val2 (exp->string body)))
               (string-append "let " (symbol->string var) " = " val1 " in " val2) ))
    (proc-exp (var body)
              (let ((val (exp->string body)))
                (string-append "proc(" (symbol->string var) ")" val)))
    (call-exp (rator rand)
              (let ((val1 (exp->string rator))
                    (val2 (exp->string rand)))
                (string-append "(" val1 " " val2 ")")))
    (letrec-exp (p-name b-var p-body letrec-body)
                (let ((val1 (exp->string p-body))
                      (val2 (exp->string letrec-body)))
                  (string-append "letrec " (symbol->string p-name) "(" (symbol->string b-var) ") = " val1 " in " val2)))
    (const-list-exp (nums) (string-append "(" (string-join (map number->string nums) ", ") ")"))
    (try-exp (body var handler)
             (let ((val1 (exp->string body))
                   (val2 (exp->string handler)))
               (string-append "try " val1 " catch(" (symbol->string var) ") " val2 )))
    (raise-exp (exp1)
               (let ((val1 (exp->string exp1)))                    
                (string-append "raise " val1 )))
   ))

(define (stringify p-name b-var p-body)
  (string-append (symbol->string p-name) "(" (symbol->string b-var) ") = " (exp->string p-body) "\n"))

