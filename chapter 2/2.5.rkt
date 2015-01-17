#lang eopl
(require rackunit)

(define empty-env
  (lambda () '()))

(define extend-env
  (lambda(var val env)
    (cons (cons var val) env)))

;;; Exercise 2.5
(define apply-env
  (lambda (env search-var)
    (cond 
           ((null? env) (report-no-binding-found search-var))
           ((equal? (caar env) search-var) (cdar env))
           (else (apply-env (cdr env) search-var)))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding found for ~s" search-var)))

(check-equal? (apply-env (extend-env 'a 'b (empty-env)) 'a) 'b)
(check-equal? (apply-env (extend-env 'c 'd (extend-env 'a 'b empty-env)) 'a) 'b)

