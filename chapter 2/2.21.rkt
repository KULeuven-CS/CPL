#lang eopl
(require rackunit)

(define-datatype environment environment?
  (empty-env)
  (non-emty-env
   (var symbol?)
   (val symbol?)
   (env environment?)))

(define extend-env
  (lambda (var val env)
    (non-emty-env var val env)))

;;; Exercise 2.21
(define apply-env
  (lambda (search-env search-var)
    (cases environment search-env
      (empty-env () (report-no-binding-found search-var))
      (non-emty-env (var val env) 
       (if(equal? var search-var)
          val
          (apply-env env search-var)))
      (else report-invalid-env search-env))))
      
(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding found for ~s" search-var)))

(define report-invalid-env
  (lambda (env) 
    (eopl:error 'apply-env "Bad environment: ~s" env))) 
  
(check-equal? (apply-env (extend-env 'a 'b (empty-env)) 'a) 'b)
(check-equal? (apply-env (extend-env 'c 'd (extend-env 'a 'b (empty-env))) 'a) 'b)

(define has-binding?
  (lambda (search-env search-var)
    (cases environment search-env
      (empty-env () #f)
      (non-emty-env (var val env) 
       (if(equal? var search-var)
          #t
          (has-binding? env search-var)))
      (else report-invalid-env search-env))))
   
(check-equal? (has-binding? (extend-env 'a 'b (empty-env)) 'a) #t)
(check-equal? (has-binding? (extend-env 'c 'd (extend-env 'a 'b (empty-env))) 'a) #t)
(check-equal? (has-binding? (extend-env 'a 'b (empty-env)) 'x) #f)
(check-equal? (has-binding? (extend-env 'c 'd (extend-env 'a 'b (empty-env))) 'x) #f)
