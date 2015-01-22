#lang eopl
(require "syntax.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;; Unit substitution ;;;;;;;;;;;;;;;;

;; apply-one-subst: type * tvar * type -> type
;; (apply-one-subst ty0 var ty1) returns the type obtained by
;; substituting ty1 for every occurrence of tvar in ty0.  This is
;; sometimes written ty0[tvar=ty1]

;; apply-one-subst : Type * Tvar * Type -> Type
;; Page: 260
(define apply-one-subst
  (lambda (ty0 tvar ty1)
    (cases type ty0
      (int-type () (int-type))
      (bool-type () (bool-type))
      (proc-type (arg-type result-type)
                 (proc-type
                  (apply-one-subst arg-type tvar ty1)
                  (apply-one-subst result-type tvar ty1)))
      (tvar-type (sn)
                 (if (equal? ty0 tvar) ty1 ty0)))))

;;;;;;;;;;;;;;;; Substitutions ;;;;;;;;;;;;;;;;

;; a substitution is a map from unknown types to types.
;; we'll represent this as an association list.

(define pair-of
  (lambda (pred1 pred2)
    (lambda (val)
      (and (pair? val) (pred1 (car val)) (pred2 (cdr val))))))

(define substitution? 
  (list-of (pair-of tvar-type? type?)))

;; basic observer: apply-subst-to-type
;; this is sometimes written ty1.subst 

;; apply-subst-to-type : Type * Subst -> Type
;; Page: 261
(define apply-subst-to-type
  (lambda (ty subst)
    (cases type ty
      (int-type () (int-type))
      (bool-type () (bool-type))
      (proc-type (t1 t2)
                 (proc-type
                  (apply-subst-to-type t1 subst)
                  (apply-subst-to-type t2 subst)))
      (tvar-type (sn)
                 (let ((tmp (assoc ty subst)))
                   (if tmp
                       (cdr tmp)
                       ty))))))

;; empty-subst : () -> Subst
;; produces a representation of the empty substitution.

;; extend-subst : Subst * Tvar * Type -> Subst

;; (extend-subst s tv t) produces a substitution with the property
;; that for all t0,

;;   (apply-subst t0 (extend-subst s tv t))
;;   = (apply-one-subst (apply-subst t0 s) tv t)

;; i.e.,  t0.(s[tv=t]) = (t0.s)[tv=t]

;; this means that for any type variable tv0 in the domain of s,

;;   (apply-subst tv0 (extend-subst s tv t))
;;   = (apply-one-subst (apply-subst tv0 s) tv t)

;; so we extend the substitution with a new element, and apply [t/v] to every
;; element already in the substitution. 


;; empty-subst : () -> Subst
;; Page 262
(define empty-subst (lambda () '()))

;; extend-subst : Subst * Tvar * Type -> Subst
;; usage: tvar not already bound in subst.
;; Page: 262
(define extend-subst
  (lambda (subst tvar ty)
    (cons
     (cons tvar ty)
     (map 
      (lambda (p)
        (let ((oldlhs (car p))
              (oldrhs (cdr p)))
          (cons
           oldlhs
           (apply-one-subst oldrhs tvar ty))))
      subst))))

; UNIFICATION

;; we'll maintain the invariant that no variable bound in the
;; substitution occurs in any of the right-hand sides of the
;; substitution. 


;;;;;;;;;;;;;;;; the unifier ;;;;;;;;;;;;;;;;

;; unifier : Type * Type * Subst * Exp -> Subst OR Fails
;; Page: 264
(define unifier
  (lambda (ty1 ty2 subst exp)
    (let ((ty1 (apply-subst-to-type ty1 subst))
          (ty2 (apply-subst-to-type ty2 subst)))
      (cond
        ((equal? ty1 ty2) subst)            
        ((tvar-type? ty1)
         (if (no-occurrence? ty1 ty2)
             (extend-subst subst ty1 ty2)
             (report-no-occurrence-violation ty1 ty2 exp)))
        ((tvar-type? ty2)
         (if (no-occurrence? ty2 ty1)
             (extend-subst subst ty2 ty1)
             (report-no-occurrence-violation ty2 ty1 exp)))
        ((and (proc-type? ty1) (proc-type? ty2))
         (let ((subst (unifier
                       (proc-type->arg-type ty1)
                       (proc-type->arg-type ty2)
                       subst exp)))
           (let ((subst (unifier
                         (proc-type->result-type ty1)
                         (proc-type->result-type ty2)
                         subst exp)))
             subst)))
        (else (report-unification-failure ty1 ty2 exp))))))

(define report-unification-failure
  (lambda (ty1 ty2 exp) 
    (eopl:error 'unification-failure
                "Type mismatch: ~s doesn't match ~s in ~s~%"
                (type->string ty1)
                (type->string ty2)
                exp)))

(define report-no-occurrence-violation
  (lambda (ty1 ty2 exp)
    (eopl:error 'check-no-occurence!
                "Can't unify: type variable ~s occurs in type ~s in expression ~s~%" 
                (type->string ty1)
                (type->string ty2)
                exp)))

;; no-occurrence? : Tvar * Type -> Bool
;; usage: Is there an occurrence of tvar in ty?
;; Page: 265
(define no-occurrence?
  (lambda (tvar ty)
    (cases type ty
      (int-type () #t)
      (bool-type () #t)
      (proc-type (arg-type result-type)
                 (and
                  (no-occurrence? tvar arg-type)
                  (no-occurrence? tvar result-type)))
      (tvar-type (serial-number) (not (equal? tvar ty))))))

;;;;;;;;;;;;;;;; The Type Checker ;;;;;;;;;;;;;;;;

;; we'll be thinking of the type of an expression as pair consisting
;; of a type (possibly with some type variables in it) and a
;; substitution that tells us how to interpret those type variables.

;; Answer = Type * Subst
;; type-of: Exp * Tenv * Subst  -> Answer

(define-datatype answer answer?
  (an-answer                       
   (type type?)
   (subst substitution?)))

;; type-of-program : Program -> Type
;; Page: 267
(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (cases answer (type-of exp1 (init-tenv) (empty-subst))
                   (an-answer (ty subst)
                              (apply-subst-to-type ty subst)))))))


;; type-of : Exp * Tenv * Subst -> Answer
;; Page: 267--270
(define type-of
  (lambda (exp tenv subst)
    (cases expression exp
      
      (const-exp (num) (an-answer (int-type) subst))
      
      (zero?-exp (exp1)
                 (cases answer (type-of exp1 tenv subst)
                   (an-answer (type1 subst1)
                              (let ((subst2 (unifier type1 (int-type) subst1 exp)))
                                (an-answer (bool-type) subst2)))))
      
      (diff-exp (exp1 exp2)
                (cases answer (type-of exp1 tenv subst)
                  (an-answer (type1 subst1)
                             (let ((subst1 (unifier type1 (int-type) subst1 exp1)))
                               (cases answer (type-of exp2 tenv subst1)
                                 (an-answer (type2 subst2)
                                            (let ((subst2
                                                   (unifier type2 (int-type) subst2 exp2)))
                                              (an-answer (int-type) subst2))))))))
      
      (if-exp (exp1 exp2 exp3)
              (cases answer (type-of exp1 tenv subst)
                (an-answer (ty1 subst)
                           (let ((subst (unifier ty1 (bool-type) subst
                                                 exp1)))
                             (cases answer (type-of exp2 tenv subst)
                               (an-answer (ty2 subst)
                                          (cases answer (type-of exp3 tenv subst)
                                            (an-answer (ty3 subst)
                                                       (let ((subst (unifier ty2 ty3 subst exp)))
                                                         (an-answer ty2 subst))))))))))
      
      (var-exp (var) (an-answer (apply-tenv tenv var) subst))
      
      (let-exp (var exp1 body)
               (cases answer (type-of exp1 tenv subst)
                 (an-answer (rhs-type subst)
                            (type-of body
                                     (extend-tenv var rhs-type tenv)
                                     subst))))
      
      (proc-exp (var otype body)
                (let ((arg-type (otype->type otype)))
                  (cases answer (type-of body
                                         (extend-tenv var arg-type tenv)
                                         subst)
                    (an-answer (result-type subst)
                               (an-answer
                                (proc-type arg-type result-type)
                                subst)))))
      
      (call-exp (rator rand)
                (let ((result-type (fresh-tvar-type)))
                  (cases answer (type-of rator tenv subst)
                    (an-answer (rator-type subst)
                               (cases answer (type-of rand tenv subst)
                                 (an-answer (rand-type subst)
                                            (let ((subst
                                                   (unifier rator-type
                                                            (proc-type rand-type result-type)
                                                            subst
                                                            exp)))
                                              (an-answer result-type subst))))))))
      
      (letrec-exp (proc-result-otype proc-name 
                                     bvar proc-arg-otype 
                                     proc-body
                                     letrec-body)
                  (let ((proc-result-type
                         (otype->type proc-result-otype)) 
                        (proc-arg-type
                         (otype->type proc-arg-otype)))
                    (let ((tenv-for-letrec-body
                           (extend-tenv 
                            proc-name
                            (proc-type proc-arg-type proc-result-type)
                            tenv)))
                      (cases answer (type-of proc-body
                                             (extend-tenv
                                              bvar proc-arg-type tenv-for-letrec-body)
                                             subst)
                        (an-answer (proc-body-type subst)
                                   (let ((subst 
                                          (unifier proc-body-type proc-result-type subst
                                                   proc-body))) 
                                     (type-of letrec-body
                                              tenv-for-letrec-body
                                              subst)))))))
      
      )))

;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;;

(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extended-tenv-record
   (sym symbol?)
   (type type?)
   (tenv type-environment?)))

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)

(define apply-tenv 
  (lambda (tenv sym)
    (cases type-environment tenv
      (empty-tenv-record ()
                         (eopl:error 'apply-tenv "Unbound variable ~s" sym))
      (extended-tenv-record (sym1 val1 old-env)
                            (if (eqv? sym sym1) 
                                val1
                                (apply-tenv old-env sym))))))

(define init-tenv
  (lambda ()
    (extend-tenv 'x (int-type) 
                 (extend-tenv 'v (int-type)
                              (extend-tenv 'i (int-type)
                                           (empty-tenv))))))

;; fresh-tvar-type : () -> Type
;; Page: 265  
(define fresh-tvar-type
  (let ((sn 0))
    (lambda ()
      (set! sn (+ sn 1))
      (tvar-type sn))))

;; otype->type : OptionalType -> Type
;; Page: 265
(define otype->type
  (lambda (otype)
    (cases optional-type otype
      (no-type () (fresh-tvar-type))
      (a-type (ty) ty))))