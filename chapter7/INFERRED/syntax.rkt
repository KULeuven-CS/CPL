#lang eopl
(provide (all-defined-out))

;;; Syntax

; TYPES
(define-datatype type type?
  (int-type)
  (bool-type)
  (proc-type (arg-type type?) (result-type type?))
  (tvar-type (n number?)))

(define-datatype optional-type optional-type?
  (no-type)
  (a-type (ty type?)))

; predicates on types
(define atomic-type?
  (lambda (ty)
    (cases type ty
      (proc-type (ty1 ty2) #f)
      (tvar-type (sn) #f)
      (else #t))))

(define proc-type?
  (lambda (ty)
    (cases type ty
      (proc-type (t1 t2) #t)
      (else #f))))

(define tvar-type?
  (lambda (ty)
    (cases type ty
      (tvar-type (serial-number) #t)
      (else #f))))

;extractors
(define proc-type->arg-type
  (lambda (ty)
    (cases type ty
      (proc-type (arg-type result-type) arg-type)
      (else (eopl:error 'proc-type->arg-type
                        "Not a proc type: ~s" ty)))))

(define proc-type->result-type
  (lambda (ty)
    (cases type ty
      (proc-type (arg-type result-type) result-type)
      (else (eopl:error 'proc-type->result-types
                        "Not a proc type: ~s" ty)))))

; PROGRAMS and EXPRESSIONS
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
   (ty optional-type?)
   (body expression?))
  (call-exp
   (rator expression?)
   (rand expression?))
  (letrec-exp
   (p-result optional-type?)
   (p-name symbol?)
   (b-var symbol?)
   (b-var-type optional-type?)
   (p-body expression?)
   (letrec-body expression?))
  )

; PRETTY PRINTING

(define (type->string ty)
  (cases type ty
    (int-type () "int")
    (bool-type () "bool")
    (proc-type (arg-type result-type ) (string-append "(" (type->string arg-type) " -> " (type->string result-type) ")"))
    (tvar-type (n) (string-append "%" (number->string n)))
    ))

(define (otype->string ty)
  (cases optional-type ty
    (no-type () "?")
    (a-type (t) (type->string t))))

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
                (string-append "proc(" (symbol->string var) ":" (otype->string ty) ")" val)))
    (call-exp (rator rand)
              (let ((val1 (exp->string rator))
                    (val2 (exp->string rand)))
                (string-append "(" val1 " " val2 ")")))
    (letrec-exp (p-result-type p-name b-var b-var-type p-body letrec-body)
                (let ((ty1 (otype->string p-result-type))
                      (val1 (exp->string p-body))
                      (ty2 (otype->string b-var-type))
                      (val2 (exp->string letrec-body)))
                  (string-append "letrec " ty1 " " (symbol->string p-name) "(" (symbol->string b-var) ":" ty2 ") = " val1 " in " val2)))
    ))


