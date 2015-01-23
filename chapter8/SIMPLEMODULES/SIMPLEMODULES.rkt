#lang eopl
(require "syntax.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val 
   (proc proc?)))

;;; extractors:

(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (proc) proc)
      (else (expval-extractor-error 'proc v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype proc proc?
  (procedure
   (bvar symbol?)
   (body expression?)
   (env environment?)))

;;;;;;;;;;;;;;;; module values ;;;;;;;;;;;;;;;;

;; Page: 282, 319
(define-datatype typed-module typed-module?
  (simple-module
   (bindings environment?))
  (proc-module
   (bvar symbol?)
   (body module-body?)
   (saved-env environment?))
  )

;;;;;;;;;;;;;;;; environments ;;;;;;;;;;;;;;;;

;; Page: 282
(define-datatype environment environment?
  (empty-env)
  (extend-env 
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?))
  (extend-env-recursively
   (id symbol?)
   (bvar symbol?)
   (body expression?)
   (saved-env environment?))
  (extend-env-with-module
   (m-name symbol?)
   (m-val typed-module?)
   (saved-env environment?)   
   ))

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

;; initial-value-env : module-env -> environment

;; (init-env m-env) builds an environment in which i is bound to the
;; expressed value 1, v is bound to the expressed value 5, and x is
;; bound to the expressed value 10, and in which m-env is the module
;; environment.

(define inital-value-env 
  (lambda (m-env)
    (extend-env 
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env m-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

;; for variables bound by extend-env or extend-env-recursively

(define apply-env
  (lambda (env search-sym)
    (cases environment env
      (empty-env ()
                 (eopl:error 'apply-env "No value binding for ~s" search-sym))
      (extend-env (bvar bval saved-env)
                  (if (eqv? search-sym bvar)
                      bval
                      (apply-env saved-env search-sym)))
      (extend-env-recursively
       (id bvar body saved-env)
       (if (eqv? search-sym id)
           (proc-val (procedure bvar body env))          
           (apply-env saved-env search-sym)))
      (extend-env-with-module
       (m-name m-val saved-env)
       (apply-env saved-env search-sym)) )))

;; for names bound by extend-env-with-module

;; lookup-module-name-in-env : Sym * Env -> Typed-Module
(define lookup-module-name-in-env
  (lambda (m-name env)            
    (cases environment env
      (empty-env ()
                 (eopl:error 'lookup-module-name-in-env
                             "No module binding for ~s" m-name))
      (extend-env (bvar bval saved-env)
                  (lookup-module-name-in-env m-name saved-env))
      (extend-env-recursively  (id bvar body saved-env)
                               (lookup-module-name-in-env m-name saved-env))
      (extend-env-with-module
       (m-name1 m-val saved-env)
       (if (eqv? m-name1 m-name)
           m-val
           (lookup-module-name-in-env m-name saved-env))))))

;; lookup-qualified-var-in-env : Sym * Sym * Env -> ExpVal
;; Page: 283
(define lookup-qualified-var-in-env
  (lambda (m-name var-name env)
    (let ((m-val (lookup-module-name-in-env m-name env)))
      ; (pretty-print m-val)
      (cases typed-module m-val
        (simple-module (bindings)
                       (apply-env bindings var-name))
        (proc-module (bvar body saved-env)
                     (eopl:error 'lookup-qualified-var
                                 "can't retrieve variable from ~s take ~s from proc module"
                                 m-name var-name))))))
;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> Expval
;; Page: 284
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (module-defs body)
                 (let ((env 
                        (add-module-defns-to-env module-defs (empty-env))))
                   ;; (eopl:pretty-print env)
                   (value-of body env))))))

;; add-module-defns-to-env : Listof(Defn) * Env -> Env
;; Page: 284
(define add-module-defns-to-env
  (lambda (defs env)
    (if (null? defs)
        env
        (cases module-definition (car defs)
          (a-module-definition (m-name iface m-body)
                               (add-module-defns-to-env
                                (cdr defs)
                                (extend-env-with-module
                                 m-name 
                                 (value-of-module-body m-body env)
                                 env)))))))

;; We will have let* scoping inside a module body.
;; We put all the values in the environment, not just the ones
;; that are in the interface.  But the typechecker will prevent
;; anybody from using the extras.

;; value-of-module-body : ModuleBody * Env -> TypedModule
;; Page: 285
(define value-of-module-body
  (lambda (m-body env)    
    (cases module-body m-body
      (defns-module-body (defns)
        (simple-module
         (defns-to-env defns env))) )))


(define raise-cant-apply-non-proc-module!
  (lambda (rator-val)
    (eopl:error 'value-of-module-body 
                "can't apply non-proc-module-value ~s" rator-val)))

;; defns-to-env : Listof(Defn) * Env -> Env
;; Page: 285
(define defns-to-env
  (lambda (defns env)
    (if (null? defns)
        (empty-env)                ; we're making a little environment
        (cases definition (car defns)
          (val-defn (var exp)
                    (let ((val (value-of exp env)))
                      ;; new environment for subsequent definitions
                      (let ((new-env (extend-env var val env)))
                        (extend-env var val
                                    (defns-to-env
                                      (cdr defns) new-env)))))
          ))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    
    (cases expression exp
      
      (const-exp (num) (num-val num))
      
      (var-exp (var) (apply-env env var))
      
      (qualified-var-exp (m-name var-name)
                         (lookup-qualified-var-in-env m-name var-name env))
      
      (diff-exp (exp1 exp2)
                (let ((val1
                       (expval->num
                        (value-of exp1 env)))
                      (val2
                       (expval->num
                        (value-of exp2 env))))
                  (num-val
                   (- val1 val2))))
      
      (zero?-exp (exp1)
                 (let ((val1 (expval->num (value-of exp1 env))))
                   (if (zero? val1)
                       (bool-val #t)
                       (bool-val #f))))
      
      (if-exp (exp0 exp1 exp2) 
              (if (expval->bool (value-of exp0 env))
                  (value-of exp1 env)
                  (value-of exp2 env)))
      
      (let-exp (var exp1 body)       
               (let ((val (value-of exp1 env)))
                 (let ((new-env (extend-env var val env)))
                   ;; (eopl:pretty-print new-env)
                   (value-of body new-env))))
      
      (proc-exp (bvar ty body)
                (proc-val
                 (procedure bvar body env)))
      
      (call-exp (rator rand)          
                (let ((proc (expval->proc (value-of rator env)))
                      (arg  (value-of rand env)))
                  (apply-procedure proc arg)))
      
      (letrec-exp (ty1 proc-name bvar ty2 proc-body letrec-body)
                  (value-of letrec-body
                            (extend-env-recursively proc-name bvar proc-body env)))
      
      )))

;; apply-procedure : Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 arg)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of body (extend-env var arg saved-env))))))
