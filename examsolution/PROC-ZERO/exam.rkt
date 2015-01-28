#lang eopl
(require "syntax.rkt")
(require "PROC.rkt")
(require rackunit)

(define ex0
  (a-program (call-exp (var-exp 'zero?) (const-exp 0))))


(define ex1
  (a-program (call-exp (var-exp 'zero?) (const-exp 1))))

(check-true (value-of-program ex0))
(check-false (value-of-program ex1))
