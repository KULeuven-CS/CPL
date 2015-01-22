#lang eopl
(require "LET/syntax.rkt")
(require "LET/LET.rkt")

;;; Exercise 3.6
; additions to the syntax and interpreter can be found in the required files
(define ex306
  (a-program (let-exp 'y (minus-exp (const-exp 10)) (var-exp 'y))))

