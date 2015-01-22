#lang eopl
(require rackunit)

(define empty-env
  (lambda () '()))

(define extend-env
  (lambda(var val env)
    (cons (cons var val) env)))

;;; Exercise 2.8
(define empty-env?
  (lambda (env)
    (null? env)))

(check-equal? (empty-env? (empty-env)) #t)
(check-equal? (empty-env? (extend-env 'a 'b (empty-env))) #f)

