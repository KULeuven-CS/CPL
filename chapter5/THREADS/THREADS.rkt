#lang eopl
(require "syntax.rkt")
; (require "store.rkt")
(require "../../chapter4/EXPLICIT-REFS/store.rkt")
(provide (all-defined-out))

;; Semantics
;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val 
   (proc proc?))
  (list-val
   (lst (list-of expval?)))
  (mutex-val
   (mutex mutex?))
  )

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

(define expval->list
  (lambda (v)
    (cases expval v
      (list-val (lst) lst)
      (else (expval-extractor-error 'list v)))))

(define expval->mutex
  (lambda (v)
    (cases expval v
      (mutex-val (l) l)
      (else (expval-extractor-error 'mutex v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;;;;;;;;;;;;;;;; mutexes ;;;;;;;;;;;;;;;;

(define-datatype mutex mutex?
  (a-mutex
   (ref-to-closed?    reference?)    ; ref to bool
   (ref-to-wait-queue reference?)))  ; ref to (listof thread)

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype proc proc?
  (procedure
   (bvar symbol?)
   (body expression?)
   (env environment?)))

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;

(define-datatype continuation continuation?
  
  (end-main-thread-cont)           
  (end-subthread-cont)
  
  (diff1-cont                       
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (diff2-cont                         
   (val1 expval?)
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
  (set-rhs-cont
   (loc reference?)
   (cont continuation?))
  (spawn-cont 
   (saved-cont continuation?))
  (wait-cont 
   (saved-cont continuation?))
  (signal-cont 
   (saved-cont continuation?))
  (unop-arg-cont
   (unop1 unop?)
   (cont continuation?))
  )

;; Environments
(define-datatype environment environment?
  (empty-env)
  (extend-env 
   (bvar symbol?)
   (bval reference?)                 
   (saved-env environment?))
  (extend-env-rec*
   (proc-names (list-of symbol?))
   (b-vars (list-of symbol?))
   (proc-bodies (list-of expression?))
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
      (extend-env-rec* (p-names b-vars p-bodies saved-env)
                       (let ((n (location search-sym p-names)))
                         ;; n : (maybe int)
                         (if n
                             (newref
                              (proc-val
                               (procedure 
                                (list-ref b-vars n)
                                (list-ref p-bodies n)
                                env)))
                             (apply-env saved-env search-sym))))
      )))

;; location : Sym * Listof(Sym) -> Maybe(Int)
;; (location sym syms) returns the location of sym in syms or #f is
;; sym is not in syms.  We can specify this as follows:
;; if (memv sym syms)
;;   then (list-ref syms (location sym syms)) = sym
;;   else (location sym syms) = #f
(define location
  (lambda (sym syms)
    (cond
      ((null? syms) #f)
      ((eqv? sym (car syms)) 0)
      ((location sym (cdr syms))
       => (lambda (n) 
            (+ n 1)))
      (else #f))))


;; init-env : () -> Env

(define init-env 
  (lambda ()
    (extend-env 
     'i (newref (num-val 1))
     (extend-env
      'v (newref (num-val 5))
      (extend-env
       'x (newref (num-val 10))
       (empty-env))))))

;; queues

;; We maintain the queue by adding to the end and dequeuing from the
;; front. 

;; exercise: enqueue is expensive, since it uses append.  Do
;; something better than this.

(define empty-queue
  (lambda ()
    '()))

(define empty? null?)

(define enqueue
  (lambda (q val)
    (append q (list val))))

(define dequeue
  (lambda (q f)
    (f (car q) (cdr q))))

;;;;;;;;;;;;;;;; the scheduler ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; the state ;;;;;;;;;;;;;;;;

;; components of the scheduler state:

(define the-ready-queue   'uninitialized)         
(define the-final-answer  'uninitialized)

(define the-max-time-slice    'uninitialized)
(define the-time-remaining    'uninitialized)

;; initialize-scheduler! : Int -> Unspecified
(define initialize-scheduler!
  (lambda (ticks)
    (set! the-ready-queue (empty-queue))
    (set! the-final-answer 'uninitialized)
    (set! the-max-time-slice ticks)
    (set! the-time-remaining the-max-time-slice) 
    ))

;;;;;;;;;;;;;;;; the final answer ;;;;;;;;;;;;;;;;

;; place-on-ready-queue! : Thread -> Unspecified
;; Page: 184  
(define place-on-ready-queue!
  (lambda (th)
    (set! the-ready-queue
          (enqueue the-ready-queue th))))

;; run-next-thread : () -> FinalAnswer
;; Page: 184    
(define run-next-thread
  (lambda ()
    (if (empty? the-ready-queue)
        the-final-answer
        (dequeue the-ready-queue
                 (lambda (first-ready-thread other-ready-threads)
                   (set! the-ready-queue other-ready-threads)            
                   (set! the-time-remaining the-max-time-slice) 
                   (first-ready-thread)
                   )))))

;; set-final-answer! : ExpVal -> Unspecified
;; Page: 184    
(define set-final-answer!
  (lambda (val)
    (set! the-final-answer val)))

;; time-expired? : () -> Bool
;; Page: 184    
(define time-expired?
  (lambda ()
    (zero? the-time-remaining)))

;; decrement-timer! : () -> Unspecified
;; Page: 184    
(define decrement-timer!
  (lambda ()
    (set! the-time-remaining (- the-time-remaining 1))))


;; new-mutex () -> Mutex
;; Page: 188
(define new-mutex
  (lambda ()
    (a-mutex
     (newref #f)                     
     (newref '()))))                 

; wait queue, initially empty

;; wait-for-mutex : Mutex * Thread -> FinalAnswer
;; waits for mutex to be open, then closes it.
;; Page: 190
(define wait-for-mutex
  (lambda (m th)
    (cases mutex m
      (a-mutex (ref-to-closed? ref-to-wait-queue)
               (cond
                 ((deref ref-to-closed?)                  
                  (setref! ref-to-wait-queue
                           (enqueue (deref ref-to-wait-queue) th))
                  (run-next-thread))
                 (else
                  (setref! ref-to-closed? #t)
                  (th)))))))

;; signal-mutex : Mutex * Thread -> FinalAnswer
;; Page 190
(define signal-mutex
  (lambda (m th)
    (cases mutex m
      (a-mutex (ref-to-closed? ref-to-wait-queue)
               (let ((closed? (deref ref-to-closed?))
                     (wait-queue (deref ref-to-wait-queue)))
                 (cond (closed? 
                     (if (empty? wait-queue)
                         (setref! ref-to-closed? #f)
                         (dequeue wait-queue
                                  (lambda (first-waiting-th other-waiting-ths)
                                    (place-on-ready-queue!
                                     first-waiting-th)
                                    (setref!
                                     ref-to-wait-queue
                                     other-waiting-ths)))))
                     )
                 (th))))))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program * Int -> ExpVal
;; Page: 185    
(define value-of-program
  (lambda (timeslice pgm)
    (initialize-store!)
    (initialize-scheduler! timeslice)
    (cases program pgm
      (a-program (exp1)
                 (value-of/k
                  exp1
                  (init-env)
                  (end-main-thread-cont))))))

;; value-of/k : Exp * Env * Cont -> FinalAnswer
;; Page 182
(define value-of/k                    
  (lambda (exp env cont)
    
    (cases expression exp
      
      (const-exp (num) (apply-cont cont (num-val num)))
      
      (const-list-exp (nums)
                      (apply-cont cont
                                  (list-val (map num-val nums))))
      
      (var-exp (var) (apply-cont cont (deref (apply-env env var))))
      
      (diff-exp (exp1 exp2)
                (value-of/k exp1 env
                            (diff1-cont exp2 env cont)))
      
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
      
      (begin-exp (exp exps)           ; desugar to let expression
                 (if (null? exps)
                     (value-of/k exp env cont)
                     (value-of/k
                      (let-exp 'void  ; void should not be used as variable
                               exp
                               (begin-exp (car exps) (cdr exps)))
                      env
                      cont)))
      
      (letrec-exp (p-names b-vars p-bodies letrec-body)
                  (value-of/k
                   letrec-body
                   (extend-env-rec* p-names b-vars p-bodies env)
                   cont))
      
      (set-exp (id exp)
               (value-of/k exp env
                           (set-rhs-cont (apply-env env id) cont)))
      
      (spawn-exp (exp)
                 (value-of/k exp env
                             (spawn-cont cont)))
      
      (yield-exp ()
                 (place-on-ready-queue!
                  (lambda () (apply-cont cont (num-val -1))))
                 (run-next-thread))
      
      (mutex-exp ()
                 (apply-cont cont (mutex-val (new-mutex))))  
      
      (wait-exp (exp)
                (value-of/k exp env
                            (wait-cont cont)))
      
      (signal-exp (exp)
                  (value-of/k exp env
                              (signal-cont cont)))
      
      (unop-exp (unop1 exp)
                (value-of/k exp env
                            (unop-arg-cont unop1 cont)))
      
      )))

;; apply-cont : Cont * Exp -> FinalAnswer
;; Page: 182 and 186
(define apply-cont                    
  (lambda (cont val)
    (if (time-expired?)
        (begin
          (place-on-ready-queue!
           (lambda () (apply-cont cont val)))
          (run-next-thread))
        (begin
          
          (decrement-timer!)
          
          (cases continuation cont
            
            (end-main-thread-cont ()
                                  (set-final-answer! val)
                                  (run-next-thread))
            
            (end-subthread-cont ()
                                (run-next-thread))
            
            (diff1-cont (exp2 saved-env saved-cont)
                        (value-of/k exp2 saved-env (diff2-cont val saved-cont)))
            (diff2-cont (val1 saved-cont)
                        (let ((n1 (expval->num val1))
                              (n2 (expval->num val)))
                          (apply-cont saved-cont
                                      (num-val (- n1 n2)))))
            (if-test-cont (exp2 exp3 env cont)
                          (if (expval->bool val)
                              (value-of/k exp2 env cont)
                              (value-of/k exp3 env cont)))
            (let-exp-cont (var body saved-env saved-cont)
                          (value-of/k body
                                      (extend-env var (newref val) saved-env) saved-cont))
            (rator-cont (rand saved-env saved-cont)
                        (value-of/k rand saved-env
                                    (rand-cont val saved-cont)))
            (rand-cont (val1 saved-cont)
                       (let ((proc (expval->proc val1)))
                         (apply-procedure proc val saved-cont)))
            (set-rhs-cont (loc cont)
                          (begin
                            (setref! loc val)
                            (apply-cont cont (num-val 26))))
            
            (spawn-cont (saved-cont)
                        (let ((proc1 (expval->proc val)))
                          (place-on-ready-queue!
                           (lambda ()
                             (apply-procedure proc1
                                              (num-val 28)
                                              (end-subthread-cont))))
                          (apply-cont saved-cont (num-val 73))))
            
            (wait-cont (saved-cont)
                       (wait-for-mutex
                        (expval->mutex val)
                        (lambda () (apply-cont saved-cont (num-val 52)))))
            
            (signal-cont (saved-cont)
                         (signal-mutex
                          (expval->mutex val)
                          (lambda () (apply-cont saved-cont (num-val 53)))))
            
            (unop-arg-cont (unop1 cont)
                           (apply-unop unop1 val cont))
            
            )))))

(define apply-procedure
  (lambda (proc1 arg cont)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of/k body
                             (extend-env var (newref arg) saved-env)
                             cont)))))

(define apply-unop
  (lambda (unop1 arg cont)
    (cases unop unop1
      
      (zero?-unop ()
                  (apply-cont cont
                              (bool-val
                               (zero? (expval->num arg)))))
      
      (car-unop ()
                (let ((lst (expval->list arg)))
                  (apply-cont cont (car lst))))
      (cdr-unop ()
                (let ((lst (expval->list arg)))
                  (apply-cont cont (list-val (cdr lst)))))
      
      (null?-unop ()
                  (apply-cont cont 
                              (bool-val (null? (expval->list arg)))))
      
      (print-unop ()
                  (begin
                    (eopl:printf "~a~%" (expval->num arg))
                    (apply-cont cont (num-val 1))))
      
      )))  
