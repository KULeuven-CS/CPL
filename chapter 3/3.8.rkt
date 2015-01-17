#lang eopl
(require "LET/syntax.rkt")
(require "LET/LET.rkt")

;;; Exercise 3.8
; additions to the syntax and interpreter can be found in the required files
(define ex308
  (a-program (let-exp 'a (minus-exp (const-exp 10)) (let-exp 'b (diff-exp (const-exp 0) (const-exp 10)) (equal?-exp (var-exp 'a) (var-exp 'b))))))
