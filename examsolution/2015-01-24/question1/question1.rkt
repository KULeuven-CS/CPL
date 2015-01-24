#lang eopl
(require "syntax.rkt")
(require "PROC.rkt")

(define p
  (a-program 
   (let-exp 'g 
            (proc-exp 'x 
                       (let-exp 'y 
                                (var-exp 'x) 
                                (proc-exp 'z (diff-exp (var-exp 'x) (diff-exp (var-exp 'y) (var-exp 'z))))))
            (call-exp (call-exp (var-exp 'g) (const-exp 1)) (const-exp 1)))))

