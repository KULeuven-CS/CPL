#lang eopl
(require rackunit)

(define empty-env
  (lambda () '()))

(define extend-env
  (lambda(var val env)
    (cons (cons var val) env)))

;;; Exercise 2.9
(define has-binding?
  (lambda (env search-var)
    (if (empty-env? env) 
        #f
        (if (equal? (caar env) search-var) 
            #t
            (has-binding? (cdr env) search-var)))))

(check-equal? (has-binding? (extend-env 'a 'b (empty-env)) 'a) #t)
(check-equal? (has-binding? (extend-env 'c 'd (extend-env 'a 'b (empty-env))) 'a) #t)
(check-equal? (has-binding? (extend-env 'a 'b (empty-env)) 'x) #f)
(check-equal? (has-binding? (extend-env 'c 'd (extend-env 'a 'b (empty-env))) 'x) #f)
