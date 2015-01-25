#lang eopl
(require "syntax.rkt")
(provide (all-defined-out))

;;; an expressed value is either a number, a boolean or a procval.

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val
   (proc proc?)))

;; expval->num : ExpVal -> Int
(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

;; expval->bool : ExpVal -> Bool
(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

;; expval->proc : ExpVal -> Proc
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


;; proc? : SchemeVal -> Bool
;; procedure : Exp * Nameless-env -> Proc
(define-datatype proc proc?
  (procedure
   ;; in LEXADDR, bound variables are replaced by %nameless-vars, so
   ;; there is no need to declare bound variables.
   ;; (bvar symbol?)
   (body expression?)
   ;; and the closure contains a nameless environment
   (env nameless-environment?)))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

;; nameless-environment? : SchemeVal -> Bool
;; Page: 99
(define nameless-environment?
  (lambda (x)
    ((list-of expval?) x)))

;; empty-nameless-env : () -> Nameless-env
;; Page: 99
(define empty-nameless-env
  (lambda ()
    '()))

;; empty-nameless-env? : Nameless-env -> Bool
(define empty-nameless-env?
  (lambda (x)
    (null? x)))

;; extend-nameless-env : ExpVal * Nameless-env -> Nameless-env
;; Page: 99
(define extend-nameless-env
  (lambda (val nameless-env)
    (cons val nameless-env)))

;; apply-nameless-env : Nameless-env * Lexaddr -> ExpVal
;; Page: 99
(define apply-nameless-env
  (lambda (nameless-env n)
    (list-ref nameless-env n)))

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

;; init-nameless-env : () -> Nameless-env

(define init-nameless-env
  (lambda ()
    (extend-nameless-env
     (num-val 1)            ; was i
     (extend-nameless-env
      (num-val 5)           ; was v
      (extend-nameless-env
       (num-val 10)         ; was x
       (empty-nameless-env))))))

;;;;;;;;;;;;;;;; lexical address calculator ;;;;;;;;;;;;;;;;

;; translation-of-program : Program -> Nameless-program
;; Page: 96
(define translation-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (a-program
                  (translation-of exp1 (init-senv)))))))

;; translation-of : Exp * Senv -> Nameless-exp
;; Page 97
(define translation-of
  (lambda (exp senv)
    (cases expression exp
      (const-exp (num) (const-exp num))
      (diff-exp (exp1 exp2)
                (diff-exp
                 (translation-of exp1 senv)
                 (translation-of exp2 senv)))
      (zero?-exp (exp1)
                 (zero?-exp
                  (translation-of exp1 senv)))
      (if-exp (exp1 exp2 exp3)
              (if-exp
               (translation-of exp1 senv)
               (translation-of exp2 senv)
               (translation-of exp3 senv)))
      (var-exp (var)
               (nameless-var-exp
                (apply-senv senv var)))
      (let-exp (var exp1 body)
               (nameless-let-exp
                (translation-of exp1 senv)
                (translation-of body
                                (extend-senv var senv))))
      (proc-exp (var body)
                (nameless-proc-exp
                 (translation-of body
                                 (extend-senv var senv))))
      (call-exp (rator rand)
                (call-exp
                 (translation-of rator senv)
                 (translation-of rand senv)))
      (else (report-invalid-source-expression exp))
      )))

(define report-invalid-source-expression
  (lambda (exp)
    (eopl:error 'value-of
                "Illegal expression in source code: ~s" exp)))

;;;;;;;;;;;;;;;; static environments ;;;;;;;;;;;;;;;;

;;; Senv = Listof(Sym)
;;; Lexaddr = N

;; empty-senv : () -> Senv
;; Page: 95
(define empty-senv
  (lambda ()
    '()))

;; extend-senv : Var * Senv -> Senv
;; Page: 95
(define extend-senv
  (lambda (var senv)
    (cons var senv)))

;; apply-senv : Senv * Var -> Lexaddr
;; Page: 95
(define apply-senv
  (lambda (senv var)
    (cond
      ((null? senv) (report-unbound-var var))
      ((eqv? var (car senv))
       0)
      (else
       (+ 1 (apply-senv (cdr senv) var))))))

(define report-unbound-var
  (lambda (var)
    (eopl:error 'translation-of "unbound variable in code: ~s" var)))

;; init-senv : () -> Senv
;; Page: 96
(define init-senv
  (lambda ()
    (extend-senv 'i
                 (extend-senv 'v
                              (extend-senv 'x
                                           (empty-senv))))))



;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-translation : Nameless-program -> ExpVal

  ;(define value-of-translation
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-nameless-env)))))

  ;; value-of-translation : Nameless-program -> ExpVal
  ;; Page: 100
  (define value-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-nameless-env))))))

  ;; value-of : Nameless-exp * Nameless-env -> ExpVal
  (define value-of
    (lambda (exp nameless-env)
      (cases expression exp
                (const-exp (num) (num-val num))

        (diff-exp (exp1 exp2)
          (let ((val1
          (expval->num
            (value-of exp1 nameless-env)))
                (val2
          (expval->num
            (value-of exp2 nameless-env))))
            (num-val
          (- val1 val2))))

        (zero?-exp (exp1)
      (let ((val1 (expval->num (value-of exp1 nameless-env))))
        (if (zero? val1)
          (bool-val #t)
          (bool-val #f))))

        (if-exp (exp0 exp1 exp2)
          (if (expval->bool (value-of exp0 nameless-env))
            (value-of exp1 nameless-env)
            (value-of exp2 nameless-env)))

        (call-exp (rator rand)
          (let ((proc (expval->proc (value-of rator nameless-env)))
                (arg (value-of rand nameless-env)))
        (apply-procedure proc arg)))

        (nameless-var-exp (n)
          (apply-nameless-env nameless-env n))

        (nameless-let-exp (exp1 body)
          (let ((val (value-of exp1 nameless-env)))
            (value-of body
              (extend-nameless-env val nameless-env))))

        (nameless-proc-exp (body)
          (proc-val
            (procedure body nameless-env)))

        (else
         (eopl:error 'value-of
        "Illegal expression in translated code: ~s" exp))

        )))


  ;; apply-procedure : Proc * ExpVal -> ExpVal

  (define apply-procedure
    (lambda (proc1 arg)
      (cases proc proc1
        (procedure (body saved-env)
          (value-of body (extend-nameless-env arg saved-env))))))
