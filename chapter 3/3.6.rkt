#lang eopl
(require "LET/syntax.rkt")
(require "LET/LET.rkt")

;;; Exercise 3.6
(define ex306
  (a-program (let-exp 'y (minus-exp (const-exp 10)) (var-exp 'y))))

