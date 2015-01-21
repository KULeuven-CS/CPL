#lang eopl
(require racket)
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
  (letrec-exp ; multi-declaration letrec
   (p-name (list-of symbol?))
   (b-var (list-of symbol?))
   (p-body (list-of expression?))
   (letrec-body expression?))
  (begin-exp (exp1 expression?) (exps (list-of expression?)))
  (newref-exp
   (exp1 expression?))
  (deref-exp
   (exp1 expression?))  
  (setref-exp
   (exp1 expression?)
   (exp2 expression?))
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
    (letrec-exp (p-names b-vars p-bodys letrec-body)
                (string-append "letrec\n" 
                               (string-join (map stringify p-names b-vars p-bodys)) 
                               "in\n" 
                               (exp->string letrec-body)))
    (newref-exp (exp1)
                (let ((val1 (exp->string exp1 )))
                  (string-append "newref(" val1 ")")))
    (deref-exp (exp1)
               (let ((val1 (exp->string exp1 )))
                 (string-append "deref(" val1 ")")))
    (setref-exp (exp1 exp2)
                (let ((val1 (exp->string exp1 ))
                      (val2 (exp->string exp2 )))
                  (string-append "setref(" val1 "," val2 ")" )))
    (begin-exp (exp exps)
               (string-append "begin " (exp->string exp) ";" (string-join  (map exp->string exps) ";") " end"))
    
    ))

(define (stringify p-name b-var p-body)
  (string-append (symbol->string p-name) "(" (symbol->string b-var) ") = " (exp->string p-body) "\n"))


