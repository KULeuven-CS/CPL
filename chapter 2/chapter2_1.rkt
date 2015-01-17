#lang eopl
(require rackunit)

;;; Exercise 2.5
(define empty-env
  (lambda () '()))

(define extend-env
  (lambda(var val env)
    (cons (cons var val) env)))

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

;;; Exercise 2.8
(define empty-env?
  (lambda (env)
    (null? env)))

(check-equal? (empty-env? (empty-env)) #t)
(check-equal? (empty-env? (extend-env 'a 'b (empty-env))) #f)

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


