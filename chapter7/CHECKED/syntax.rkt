#lang eopl
(provide (all-defined-out))

;;; Syntax
(define-datatype type type?
   (int-type)
   (bool-type)
   (proc-type (arg-type type?) (result-type type?)))

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
   (ty type?)
   (body expression?))
  (call-exp
   (rator expression?)
   (rand expression?))
  (letrec-exp
   (p-result type?)
   (p-name symbol?)
   (b-var symbol?)
   (b-var-type type?)
   (p-body expression?)
   (letrec-body expression?))
  )

(define (type->string ty)
  (cases type ty
    (int-type () "int")
    (bool-type () "bool")
    (proc-type (arg-type result-type ) (string-append "(" (type->string arg-type) " -> " (type->string result-type) ")"))
    ))

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
    (proc-exp (var ty body)
              (let ((val (exp->string body)))
                (string-append "proc(" (symbol->string var) ":" (type->string ty) ")" val)))
    (call-exp (rator rand)
              (let ((val1 (exp->string rator))
                    (val2 (exp->string rand)))
                (string-append "(" val1 " " val2 ")")))
    (letrec-exp (p-result-type p-name b-var b-var-type p-body letrec-body)
                (let ((ty1 (type->string p-result-type))
                      (val1 (exp->string p-body))
                      (ty2 (type->string b-var-type))
                      (val2 (exp->string letrec-body)))
                  (string-append "letrec " ty1 " " (symbol->string p-name) "(" (symbol->string b-var) ":" ty2 ") = " val1 " in " val2)))
    ))


