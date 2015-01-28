#lang eopl
(require "syntax.rkt")
(require "CHECKED.rkt")
(require "checker.rkt")

; unpair(x y pair(1, 2) x)
(define pair1
  (a-program
   (unpair-exp 'x 'y (pair-exp (const-exp 1) (const-exp 2)) (var-exp 'x))))

(define (test p)
  (begin
    (display "PROGRAM:\n")
    (display (program->string p))
    (display "\n")
    (display "TYPE:\n")
    (display (type->string (type-of-program p)))
    (display "\n")
    
    (value-of-program p)
    )
  )



