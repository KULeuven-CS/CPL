#lang eopl
(require "syntax.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean, a procval, or a
;;; list of expvals.

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val 
   (proc proc?))
  (list-val
   (lst (list-of expval?))))

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

(define expval->list
  (lambda (v)
    (cases expval v
      (list-val (lst) lst)
      (else (expval-extractor-error 'list v)))))

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

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;


(define-datatype continuation continuation?
  (end-cont)                          ; []
  (diff1-cont                       
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (diff2-cont                         
   (val1 expval?)
   (cont continuation?))
  (unop-arg-cont
   (unop unary-op?)
   (cont continuation?))
  (let-exp-cont
   (var symbol?)
   (body expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (if-test-cont
   (exp2 expression?)
   (exp3 expression?)
   (env environment?)
   (cont continuation?))
  (rator-cont            
   (rand expression?)
   (env environment?)
   (cont continuation?))
  (rand-cont                          
   (val1 expval?)
   (cont continuation?))
  (try-cont
   (var symbol?)
   (handler-exp expression?)
   (env environment?)
   (cont continuation?))
  (raise1-cont
   (saved-cont continuation?))
  )

(define (cont->string k) (cont->string-inner k "[ ]"))
(define (cont->string-inner k hole)
  (cases continuation k
    (end-cont () hole)
    (unop-arg-cont (oper k2) 
                   (cont->string-inner k2 
                                       (string-append (oper->string oper) "(" hole ")" )))
    (let-exp-cont (var body saved-env saved-cont) 
                  (cont->string-inner saved-cont 
                                      (string-append "let " (symbol->string var) " = " hole " in <" (exp->string body) ">" )))
    (if-test-cont (exp2 exp3 saved-env saved-cont) 
                  (cont->string-inner saved-cont 
                                      (string-append "if " hole " then <" (exp->string exp2) "> else <" (exp->string exp3) ">" )))    
    (diff1-cont (exp env k2) 
                (cont->string-inner k2 
                                    (string-append "(" hole " - <" (exp->string exp) ">)" )))
    (diff2-cont (v1 k2) 
                (cont->string-inner k2 
                                    (string-append "(" (number->string (expval->num v1)) " - " hole ")")))
    (rator-cont (rand env k2) 
                (cont->string-inner k2 
                                    (string-append "(" hole " <" (exp->string rand) ">)" )))
    (rand-cont (v1 k2) (cont->string-inner k2 
                                           (string-append "({proc-val} " hole ) ))
    (try-cont (var handler-exp env cont ) (cont->string-inner cont
                                                              (string-append "try " hole " catch(" (symbol->string var) ")" (exp->string handler-exp))
                                                              ))
    (raise1-cont (saved-cont) (cont->string-inner saved-cont (string-append "raise " hole))) 
    ))  

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (value-of/k body (init-env) (end-cont))))))

;; value-of/k : Exp * Env * Cont -> FinalAnswer
;; Page: 173
(define value-of/k
  (lambda (exp env cont)
    (display (string-append "Evaluating " (exp->string exp) " in cc " (cont->string cont)))
    (display "\n")
    
    (cases expression exp
      
      (const-exp (num) (apply-cont cont (num-val num)))
      
      (const-list-exp (nums)
                      (apply-cont cont
                                  (list-val (map num-val nums))))
      
      (var-exp (var) (apply-cont cont (apply-env env var)))
      
      (diff-exp (exp1 exp2)
                (value-of/k exp1 env
                            (diff1-cont exp2 env cont)))
      
      (unop-exp (unop exp1)
                (value-of/k exp1 env
                            (unop-arg-cont unop cont)))
      
      (if-exp (exp1 exp2 exp3)
              (value-of/k exp1 env
                          (if-test-cont exp2 exp3 env cont)))
      
      (proc-exp (var body)
                (apply-cont cont
                            (proc-val
                             (procedure var body env))))
      
      (call-exp (rator rand)
                (value-of/k rator env
                            (rator-cont rand env cont)))
      

      (let-exp (var exp1 body)
               (value-of/k exp1 env
                           (let-exp-cont var body env cont)))
      
      (letrec-exp (p-name b-var p-body letrec-body)
                  (value-of/k
                   letrec-body
                   (extend-env-rec p-name b-var p-body env)
                   cont))
      
      (try-exp (exp1 var handler-exp)
               (value-of/k exp1 env
                           (try-cont var handler-exp env cont)))
      
      (raise-exp (exp1)
                 (value-of/k exp1 env
                             (raise1-cont cont))))))

;; apply-cont : continuation * expval -> final-expval

(define apply-cont
  (lambda (cont val)
    (display "Sending ")
    (display val)
    (display (string-append " to cc " (cont->string cont) "\n"))
    (cases continuation cont
      (end-cont () val)
      (diff1-cont (exp2 saved-env saved-cont)
                  (value-of/k exp2 saved-env (diff2-cont val saved-cont)))
      (diff2-cont (val1 saved-cont)
                  (let ((n1 (expval->num val1))
                        (n2 (expval->num val)))
                    (apply-cont saved-cont
                                (num-val (- n1 n2)))))
      (unop-arg-cont (unop cont)
                     (apply-cont cont
                                 (apply-unop unop val)))
      (let-exp-cont (var body saved-env saved-cont)
                    (value-of/k body
                                (extend-env var val saved-env) saved-cont))
      (if-test-cont (exp2 exp3 env cont)
                    (if (expval->bool val)
                        (value-of/k exp2 env cont)
                        (value-of/k exp3 env cont)))
      (rator-cont (rand saved-env saved-cont)
                  (value-of/k rand saved-env
                              (rand-cont val saved-cont)))
      (rand-cont (val1 saved-cont)
                 (let ((proc (expval->proc val1)))
                   (apply-procedure proc val saved-cont)))
      ;; the body of the try finished normally-- don't evaluate the handler
      (try-cont (var handler-exp saved-env saved-cont)
                (apply-cont saved-cont val))
      ;; val is the value of the argument to raise
      (raise1-cont (saved-cont)
                   (apply-handler val saved-cont))
      )))

;; apply-handler : ExpVal * Cont -> FinalAnswer
(define apply-handler
  (lambda (val cont)
    (display (string-append "Looking for handler in: " (cont->string cont) "\n"))    
    (cases continuation cont
      ;; interesting cases
      (try-cont (var handler-exp saved-env saved-cont)
                (value-of/k handler-exp
                            (extend-env var val saved-env)
                            saved-cont))
      
      (end-cont () (eopl:error 'apply-handler "uncaught exception!"))
      
      ;; otherwise, just look for the handler...
      (diff1-cont (exp2 saved-env saved-cont)
                  (apply-handler val saved-cont))
      (diff2-cont (val1 saved-cont)
                  (apply-handler val saved-cont))
      (if-test-cont (exp2 exp3 env saved-cont)
                    (apply-handler val saved-cont))
      (unop-arg-cont (unop saved-cont)
                     (apply-handler val saved-cont))
      (rator-cont (rand saved-env saved-cont)
                  (apply-handler val saved-cont))
      (rand-cont (val1 saved-cont)
                 (apply-handler val saved-cont))
      (raise1-cont (cont)
                   (apply-handler val cont))
      (let-exp-cont (var body saved-env saved-cont)
                    (apply-handler val saved-cont))
      )))


;; apply-procedure : procedure * expval * cont -> final-expval

(define apply-procedure
  (lambda (proc1 arg cont)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of/k body
                             (extend-env var arg saved-env)
                             cont)))))


(define apply-unop
  (lambda (unop val)
    (cases unary-op unop
      (null?-unop ()
                  (bool-val
                   (null? (expval->list val))))
      (car-unop ()
                (car (expval->list val)))
      (cdr-unop ()
                (list-val (cdr (expval->list val))))
      (zero?-unop ()
                  (bool-val
                   (zero? (expval->num val)))))))
