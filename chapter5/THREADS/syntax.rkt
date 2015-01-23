#lang eopl
(require racket)
(provide (all-defined-out))

;;; Syntax
(define-datatype program program?
  (a-program (exp1 expression?)))

(define-datatype expression expression?
  (const-exp (num number?))
  (const-list-exp (nums (list-of number?)))
  (unop-exp (oper unop?) (exp1 expression?))
  (diff-exp (exp1 expression?) (exp2 expression?))
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
   (p-name (list-of symbol?))
   (b-var (list-of symbol?))
   (p-body (list-of expression?))
   (letrec-body expression?))
  (begin-exp (exp1 expression?) (exps (list-of expression?)))
  (set-exp
   (var symbol?)
   (exp expression?))
  (spawn-exp (exp1 expression?))
  (yield-exp)
  (mutex-exp)
  (wait-exp (exp1 expression?))
  (signal-exp (exp2 expression?))  
  )

(define-datatype unop unop?
  (car-unop)
  (cdr-unop)
  (null?-unop)
  (zero?-unop)
  (print-unop))

(define (oper->string oper)
  (cases unop oper
    (null?-unop () "null?")
   (car-unop () "car")
   (cdr-unop () "cdr")
   (zero?-unop () "zero?")
    (print-unop () "print")))

(define (program->string pgm)
  (cases program pgm
    (a-program (exp1)
               (exp->string exp1 ))))

(define (exp->string exp)
  (cases expression exp
    (const-exp (num) (number->string num))
    (const-list-exp (nums) (string-append "(" (string-join (map number->string nums) ", ") ")"))
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
    (letrec-exp (p-names b-vars p-bodys letrec-body)
                (string-append "letrec\n" 
                               (string-join (map stringify p-names b-vars p-bodys)) 
                               "in\n" 
                               (exp->string letrec-body)))
    (set-exp (var exp)
                (let ((val1 (symbol->string var ))
                      (val2 (exp->string exp )))
                  (string-append val1 " := " val2  )))
    (begin-exp (exp exps)
               (string-append "begin " (exp->string exp) ";" (string-join  (map exp->string exps) ";") " end"))
  (spawn-exp (exp1)
             (let ((val1 (exp->string exp1 )))
                 (string-append "spawn(" val1 ")")))
  (yield-exp () "yield() ")
  (mutex-exp () "mutex() ")
  (wait-exp (exp1)
            (let ((val1 (exp->string exp1 )))
                 (string-append "wait(" val1 ")")))
  (signal-exp (exp1)
              (let ((val1 (exp->string exp1 )))
                 (string-append "signal(" val1 ")")))     
    
    ))

(define (stringify p-name b-var p-body)
  (string-append (symbol->string p-name) "(" (symbol->string b-var) ") = " (exp->string p-body) "\n"))


