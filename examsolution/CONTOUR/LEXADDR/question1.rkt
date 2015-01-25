#lang eopl
(require "syntax.rkt")
(require "LEXADDR.rkt")
(require rackunit)

;; Original in PROC
(define p
  (a-program 
   (let-exp 'g 
     (proc-exp 'x 
       (let-exp 'y 
         (var-exp 'x) 
         (proc-exp 'z (diff-exp (var-exp 'x) (diff-exp (var-exp 'y) (var-exp 'z))))))
     (call-exp (call-exp (var-exp 'g) (const-exp 1)) (const-exp 1)))))



;; Translation to LEXADDR
(define nameless-p
  (a-program 
   (nameless-let-exp 
     (nameless-proc-exp 
       (nameless-let-exp
         (nameless-var-exp 1) 
         (nameless-proc-exp (diff-exp (nameless-var-exp 2) (diff-exp (nameless-var-exp 1) (nameless-var-exp 0))))))
     (call-exp (call-exp (nameless-var-exp 0) (const-exp 1)) (const-exp 1)))))

;; RackUnit Solutiobn Check
(check-equal? (value-of-program nameless-p) (value-of-program (translation-of-program p)))


