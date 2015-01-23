#lang eopl
(require "syntax.rkt")
;(require "SIMPLEMODULES.rkt")
(provide (all-defined-out))

(define-datatype type-environment type-environment?
  (empty-tenv)
  (extend-tenv 
   (bvar symbol?)
   (bval type?)
   (saved-tenv type-environment?))
  (extend-tenv-with-module
   (name symbol?)
   (interface interface?)
   (saved-tenv type-environment?))
  )

;;;;;;;;;;;;;;;; procedures for looking things up tenvs ;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; lookup or die

;; lookup-qualified-var-in-tenv : Sym * Sym * Tenv -> Type
;; Page: 285
(define lookup-qualified-var-in-tenv
  (lambda (m-name var-name tenv)
    (let ((iface (lookup-module-name-in-tenv tenv m-name)))
      (cases interface iface
        (simple-iface (decls)
                      (lookup-variable-name-in-decls var-name decls)) ))))

(define lookup-variable-name-in-tenv
  (lambda (tenv search-sym)
    (let ((maybe-answer
           (variable-name->maybe-binding-in-tenv tenv search-sym)))
      (if maybe-answer maybe-answer
          (raise-tenv-lookup-failure-error 'variable search-sym tenv)))))

(define lookup-module-name-in-tenv
  (lambda (tenv search-sym)
    (let ((maybe-answer
           (module-name->maybe-binding-in-tenv tenv search-sym)))
      (if maybe-answer maybe-answer
          (raise-tenv-lookup-failure-error 'module search-sym tenv)))))

(define apply-tenv lookup-variable-name-in-tenv)

(define raise-tenv-lookup-failure-error
  (lambda (kind var tenv)
    (eopl:pretty-print
     (list 'tenv-lookup-failure: (list 'missing: kind var) 'in:
           tenv))
    (eopl:error 'lookup-variable-name-in-tenv)))

(define lookup-variable-name-in-decls
  (lambda (var-name decls0)
    (let loop ((decls decls0))
      (cond
        ((null? decls)
         (raise-lookup-variable-in-decls-error! var-name decls0))
        ((eqv? var-name (decl->name (car decls)))
         (decl->type (car decls)))
        (else (loop (cdr decls)))))))

(define raise-lookup-variable-in-decls-error!
  (lambda (var-name decls)
    (eopl:pretty-print
     (list 'lookup-variable-decls-failure:
           (list 'missing-variable var-name)
           'in:
           decls))))

;;;;;;;;;;;;;;;; lookup or return #f.

;; variable-name->maybe-binding-in-tenv : Tenv * Sym -> Maybe(Type)
(define variable-name->maybe-binding-in-tenv
  (lambda (tenv search-sym)
    (let recur ((tenv tenv)) 
      (cases type-environment tenv
        (empty-tenv () #f)
        (extend-tenv (name ty saved-tenv)
                     (if (eqv? name search-sym) 
                         ty
                         (recur saved-tenv)))
        (else (recur (tenv->saved-tenv tenv)))))))

;; module-name->maybe-binding-in-tenv : Tenv * Sym -> Maybe(Iface)
(define module-name->maybe-binding-in-tenv
  (lambda (tenv search-sym)
    (let recur ((tenv tenv)) 
      (cases type-environment tenv
        (empty-tenv () #f)
        (extend-tenv-with-module (name m-type saved-tenv)
                                 (if (eqv? name search-sym) 
                                     m-type
                                     (recur saved-tenv)))
        (else (recur (tenv->saved-tenv tenv)))))))

;; assumes tenv is non-empty.  
(define tenv->saved-tenv
  (lambda (tenv)
    (cases type-environment tenv
      (empty-tenv () 
                  (eopl:error 'tenv->saved-tenv
                              "tenv->saved-tenv called on empty tenv"))
      (extend-tenv (name ty saved-tenv) saved-tenv)
      (extend-tenv-with-module (name m-type saved-tenv) saved-tenv)
      )))

;; check-equal-type! : Type * Type * Exp -> Unspecified
;; Page: 242
(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (cond [(not (equal? ty1 ty2))
           (report-unequal-types ty1 ty2 exp)])))

;; report-unequal-types : Type * Type * Exp -> Unspecified
;; Page: 243
(define report-unequal-types
  (lambda (ty1 ty2 exp)
    (eopl:error 'check-equal-type!  
                "Types didn't match: ~s != ~a in~%~a"
                (type->string ty1)
                (type->string ty2)
                exp)))

;;;;;;;;;;;;;;;; The Type Checker ;;;;;;;;;;;;;;;;

;; moved to check-modules.scm
;; type-of-program : Program -> Type
;; Page: 244 
;;   (define type-of-program
;;     (lambda (pgm)
;;       (cases program pgm
;;         (a-program (exp1) 
;;           (type-of exp1 (init-tenv))))))


;; type-of : Exp * Tenv -> Type
;; Page 244--246.  See also page 285.
(define type-of
  (lambda (exp tenv)
    (cases expression exp
      (const-exp (num) (int-type))
      
      (diff-exp (exp1 exp2)
                (let ((type1 (type-of exp1 tenv))
                      (type2 (type-of exp2 tenv)))
                  (check-equal-type! type1 (int-type) exp1)
                  (check-equal-type! type2 (int-type) exp2)
                  (int-type)))
      
      (zero?-exp (exp1)
                 (let ((type1 (type-of exp1 tenv)))
                   (check-equal-type! type1 (int-type) exp1)
                   (bool-type)))
      
      (if-exp (exp1 exp2 exp3)
              (let ((ty1 (type-of exp1 tenv))
                    (ty2 (type-of exp2 tenv))
                    (ty3 (type-of exp3 tenv)))
                (check-equal-type! ty1 (bool-type) exp1)
                (check-equal-type! ty2 ty3 exp)
                ty2))
      
      (var-exp (var) (apply-tenv tenv var))
      
      ;; lookup-qualified-var-in-tenv defined on page 285.
      (qualified-var-exp (m-name var-name) 
                         (lookup-qualified-var-in-tenv m-name var-name tenv))
      
      (let-exp (var exp1 body)
               (let ((rhs-type (type-of exp1 tenv)))
                 (type-of body (extend-tenv var rhs-type tenv))))
      
      (proc-exp (bvar bvar-type body)
                
                (let ((result-type
                       (type-of body
                                (extend-tenv
                                 bvar
                                 bvar-type
                                 tenv))))
                  (proc-type bvar-type result-type)))
      
      (call-exp (rator rand) 
                (let ((rator-type (type-of rator tenv))
                      (rand-type  (type-of rand tenv)))
                  (cases type rator-type
                    (proc-type (arg-type result-type)
                               (begin
                                 (check-equal-type! arg-type rand-type rand)
                                 result-type))
                    (else
                     (eopl:error 'type-of
                                 "Rator not a proc type:~%~s~%had rator type ~s"   
                                 rator (type->string rator-type))))))
      
      (letrec-exp (proc-result-type proc-name 
                                    bvar bvar-type 
                                    proc-body
                                    letrec-body)
                  (let ((tenv-for-letrec-body
                         (extend-tenv 
                          proc-name
                          
                          (proc-type bvar-type proc-result-type)
                          
                          tenv)))
                    (let ((proc-body-type
                           (type-of proc-body
                                    (extend-tenv
                                     bvar
                                     bvar-type
                                     tenv-for-letrec-body))))
                      (check-equal-type!
                       proc-body-type proc-result-type proc-body)
                      (type-of letrec-body tenv-for-letrec-body))))
      
      )))
;; <:-iface : Iface * Iface * Tenv -> Bool
;; Page: 289
(define <:-iface
  (lambda (iface1 iface2 tenv)
    (cases interface iface1
      (simple-iface (decls1)
                    (cases interface iface2
                      (simple-iface (decls2)
                                    (<:-decls decls1 decls2 tenv)))))))

;; <:-decls : Listof(Decl) * Listof(Decl) * Tenv -> Bool
;; Page: 289
;;
;; s1 <: s2 iff s1 has at least as much stuff as s2, and in the same
;; order.  We walk down s1 until we find a declaration that declares
;; the same name as the first component of s2.  If we run off the
;; end of s1, then we fail.  As we walk down s1, we record any type
;; bindings in the tenv
;;
(define <:-decls
  (lambda (decls1 decls2 tenv)
    (cond
      ((null? decls2) #t)
      ((null? decls1) #f)
      (else
       (let ((name1 (decl->name (car decls1)))
             (name2 (decl->name (car decls2))))
         (if (eqv? name1 name2)
             (and
              (equal?
               (decl->type (car decls1))
               (decl->type (car decls2)))
              (<:-decls (cdr decls1) (cdr decls2) tenv))
             (<:-decls (cdr decls1) decls2 tenv)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
  ;; type-of-program : Program -> Type
  ;; Page: 286
  (define type-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (module-defs body)
          (type-of body
            (add-module-defns-to-tenv module-defs (empty-tenv)))))))

  ;; add-module-defns-to-tenv : Listof(ModuleDefn) * Tenv -> Tenv
  ;; Page: 286
  (define add-module-defns-to-tenv
    (lambda (defns tenv)
      (if (null? defns)
        tenv
        (cases module-definition (car defns)
          (a-module-definition (m-name expected-iface m-body)
            (let ((actual-iface (interface-of m-body tenv)))
              (if (<:-iface actual-iface expected-iface tenv)
                (let ((new-tenv
                        (extend-tenv-with-module
                          m-name
                          expected-iface
                          tenv)))
                  (add-module-defns-to-tenv
                    (cdr defns) new-tenv))
                (report-module-doesnt-satisfy-iface
                  m-name expected-iface actual-iface))))))))

  ;; interface-of : ModuleBody * Tenv -> Iface
  ;; Page: 288
  (define interface-of
    (lambda (m-body tenv)
      (cases module-body m-body
        (defns-module-body (defns)
          (simple-iface
            (defns-to-decls defns tenv))) )))

  ;; defns-to-decls : Listof(Defn) * Tenv -> Listof(Decl)
  ;; Page: 288
  ;; 
  ;; Convert defns to a set of declarations for just the names defined
  ;; in defns.  Do this in the context of tenv.  The tenv is extended
  ;; at every step, so we get the correct let* scoping
  ;;
  (define defns-to-decls
    (lambda (defns tenv)
      (if (null? defns)
        '()
        (cases definition (car defns)
          (val-defn (var-name exp)
            (let ((ty (type-of exp tenv)))
              (let ((new-env (extend-tenv var-name ty tenv)))
                (cons
                  (val-decl var-name ty)
                  (defns-to-decls (cdr defns) new-env)))))))))

  (define report-module-doesnt-satisfy-iface
    (lambda (m-name expected-type actual-type)
      (display 
        (list 'error-in-defn-of-module: m-name
          'expected-type: expected-type
          'actual-type: actual-type))
      (eopl:error 'type-of-module-defn)))