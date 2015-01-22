#lang eopl
(require "syntax.rkt")
(provide (all-defined-out))

;; Semantics
;;; an expressed value is either a number, or a boolean.

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val
   (proc proc?)))

(define expval->string
  (lambda (v)
    (cases expval v
      (num-val (num) (string-append "Number: " (number->string num)))
      (bool-val (bool) (string-append "Boolean: " (if bool "#t" "#f")))
      (proc-val (proc) "Procedure: {proc} "))))

;; proc? : SchemeVal -> Bool
;; procedure : Var * Exp * Env -> Proc
(define-datatype proc proc?
  (procedure
   (var symbol?)
   (body expression?)
   (env environment?)))

;; expval->num : ExpVal -> Int
;; Page: 70
(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

;; expval->bool : ExpVal -> Bool
;; Page: 70
(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

;; expval->proc : ExpVal -> Proc
(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (p) p)
      (else (expval-extractor-error 'proc v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;; Environments

(define-datatype environment environment?
  (empty-env)
  (extend-env 
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?))
  (extend-env-rec
   (id symbol?)
   (bvar symbol?)
   (body expression?)
   (saved-env environment?)))

(define apply-env
  (lambda (env search-sym)
    (cases environment env
      (empty-env ()
                 (eopl:error 'apply-env "No binding for ~s" search-sym))
      (extend-env (bvar bval saved-env)
                  (if (eqv? search-sym bvar)
                      bval
                      (apply-env saved-env search-sym)))
      (extend-env-rec (p-name b-var p-body saved-env)
                      (if (eqv? search-sym p-name)
                          (proc-val (procedure b-var p-body env))          
                          (apply-env saved-env search-sym))))))

;; init-env : () -> Env
;; usage: (init-env) = [i=1, v=5, x=10]
;; (init-env) builds an environment in which i is bound to the
;; expressed value 1, v is bound to the expressed value 5, and x is
;; bound to the expressed value 10.
;; Page: 69

(define init-env 
  (lambda ()
    (extend-env 
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

;; Continuations

;; Page: 148
(define identifier? symbol?)

(define-datatype continuation continuation?
  (end-cont)                   ; [ ]
  (zero1-cont
   (saved-cont continuation?)) ; E[zero? [] ]
  (let-exp-cont
   (var identifier?)
   (body expression?)
   (saved-env environment?)
   (saved-cont continuation?)) ; E[let var = [] in body]
  (if-test-cont 
   (exp2 expression?)
   (exp3 expression?)
   (saved-env environment?)
   (saved-cont continuation?)) ; E[if [] then exp1 else exp2]
  (diff1-cont                
   (exp2 expression?)
   (saved-env environment?)
   (saved-cont continuation?)) ; E[ [] - exp2 ]
  (diff2-cont                
   (val1 expval?)
   (saved-cont continuation?)) ; E[ val1 - [] ]
  (rator-cont            
   (rand expression?)
   (saved-env environment?)
   (saved-cont continuation?)) ; E[([] rand)]
  (rand-cont             
   (val1 expval?)
   (saved-cont continuation?))); E[(val1 [])]

; cont->string: Cont -> String
; prints the continuation as a program with a hole [ ]
(define (cont->string k) (cont->string-inner k "[ ]"))

; cont->string-inner: (k:Cont) -> (h:String) -> String
; prints the continuation k as a program with h put in the hole
(define (cont->string-inner k hole)
  (cases continuation k
    (end-cont () hole)
    (zero1-cont 
     (k2) 
     (cont->string-inner 
      k2 (string-append "zero?(" hole ")" )))
    (let-exp-cont 
     (var body saved-env saved-cont) 
     (cont->string-inner 
      saved-cont 
      (string-append "let " (symbol->string var) " = " hole " in <" (exp->string body) ">" )))
    (if-test-cont 
     (exp2 exp3 saved-env saved-cont) 
     (cont->string-inner 
      saved-cont 
      (string-append "if " hole " then <" (exp->string exp2) "> else <" (exp->string exp3) ">" )))    
    (diff1-cont 
     (exp env k2) 
     (cont->string-inner 
      k2 
      (string-append "(" hole " - <" (exp->string exp) ">)" )))
    (diff2-cont 
     (v1 k2) 
     (cont->string-inner 
      k2 
      (string-append "(" (number->string (expval->num v1)) " - " hole ")")))
    (rator-cont 
     (rand env k2) 
     (cont->string-inner 
      k2 
      (string-append "(" hole " <" (exp->string rand) ">)" )))
    (rand-cont 
     (v1 k2) 
     (cont->string-inner 
      k2 
      (string-append "({proc} " hole ) ))
    ))

;; Interpreter
;; apply-cont : Cont * ExpVal -> FinalAnswer
;; Page: 148
(define apply-cont
  (lambda (cont val)
    (display (string-append "Sending " (expval->string val) " to cc " (cont->string cont) "\n"))
    (cases continuation cont
      (end-cont () 
                (begin
                  (eopl:printf
                   "End of computation.~%")
                  val))
      (zero1-cont (saved-cont)
                  (apply-cont saved-cont
                              (bool-val
                               (zero? (expval->num val)))))
      (let-exp-cont (var body saved-env saved-cont)
                    (value-of/k body
                                (extend-env var val saved-env) saved-cont))
      (if-test-cont (exp2 exp3 saved-env saved-cont)
                    (if (expval->bool val)
                        (value-of/k exp2 saved-env saved-cont)
                        (value-of/k exp3 saved-env saved-cont)))
      (diff1-cont (exp2 saved-env saved-cont)
                  (value-of/k exp2
                              saved-env (diff2-cont val saved-cont)))
      (diff2-cont (val1 saved-cont)
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val)))
                    (apply-cont saved-cont
                                (num-val (- num1 num2)))))
      (rator-cont (rand saved-env saved-cont)
                  (value-of/k rand saved-env
                              (rand-cont val saved-cont)))
      (rand-cont (val1 saved-cont)
                 (let ((proc (expval->proc val1)))
                   (apply-procedure/k proc val saved-cont)))
      )))



;; value-of-program : Program -> FinalAnswer
;; Page: 143 and 154
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of/k exp1 (init-env) (end-cont))))))  

;; value-of/k : Exp * Env * Cont -> FinalAnswer
;; Page: 143--146, and 154
(define value-of/k
  (lambda (exp env cont)
    (display (string-append "Evaluating " (exp->string exp) " in cc " (cont->string cont)))
    (display "\n")
    (cases expression exp
      (const-exp (num) (apply-cont cont (num-val num)))
      (var-exp (var) (apply-cont cont (apply-env env var)))
      (proc-exp (var body)
                (apply-cont cont 
                            (proc-val (procedure var body env))))
      (letrec-exp (p-name b-var p-body letrec-body)
                  (value-of/k letrec-body
                              (extend-env-rec p-name b-var p-body env)
                              cont))
      (zero?-exp (exp1)
                 (value-of/k exp1 env
                             (zero1-cont cont)))
      (let-exp (var exp1 body)
               (value-of/k exp1 env
                           (let-exp-cont var body env cont)))
      (if-exp (exp1 exp2 exp3)
              (value-of/k exp1 env
                          (if-test-cont exp2 exp3 env cont)))
      (diff-exp (exp1 exp2)
                (value-of/k exp1 env
                            (diff1-cont exp2 env cont)))        
      (call-exp (rator rand) 
                (value-of/k rator env
                            (rator-cont rand env cont)))
      )))


;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
;; Page 152 and 155
(define apply-procedure/k
  (lambda (proc1 arg cont)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of/k body
                             (extend-env var arg saved-env)
                             cont)))))
