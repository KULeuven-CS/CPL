#lang eopl
(provide (all-defined-out))

;;; Syntax
(define-datatype program program?
  (a-program (exp1 expression?)))

(define-datatype expression expression?
  (const-exp (num number?))
  (diff-exp (exp1 expression?) (exp2 expression?))
  (zero?-exp (exp1 expression?))
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
  (nameless-var-exp (num number?))
  (nameless-let-exp (exp1 expression?) (body expression?))
  (nameless-proc-exp (body expression?))
)

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
    (zero?-exp (exp1)
               (let ((val1 (exp->string exp1 )))
                 (string-append "zero? " val1)))
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
    (nameless-var-exp (num) (string-append "%lexref " (number->string num)) )
    (nameless-let-exp (exp1 body)
                        (let ((val1 (exp->string exp1))
                     (val2 (exp->string body)))
                 (string-append "%let " val1 " in " val2) ))
    (nameless-proc-exp (body)
                        (let ((val (exp->string body)))
                (string-append "%lexproc " val)))
    ))


