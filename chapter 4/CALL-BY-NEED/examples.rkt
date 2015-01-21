#lang eopl
(require "syntax.rkt")
(require "CALL-BY-NEED.rkt")

;(define ep136
;	letrec infinite-loop (x) = infinite-loop(- (x,-1)))
;	in let f = proc (z) 11
;		in (f (infinite-loop 0))


(define ep136
  (a-program (letrec-exp 
              (list 'infinite-loop) 
              (list 'x) 
              (list (call-exp (var-exp 'infinite-loop) 
                              (diff-exp((var-exp 'x) 
										(const-exp -1)))))
              (let-exp 
				(var-exp 'f) 
				(proc-exp ('z) (const-exp 11))
				(call-exp 
				  (var-exp 'f) 
				  (call-exp 
					(var-exp 'infinite-loop)
					(const-exp 0)))))))

(define test
  (a-program (const-exp 0)))



(define run
  (lambda (prgm)
	(value-of-program prgm)))
