#lang eopl
(require "syntax.rkt")
(require "CALL-BY-NEED-LETLAZ.rkt")
(require "store.rkt")

;All IMPLICIT-REFS examples should also work.

; Example page 136
;	letrec infinite-loop (x) = infinite-loop(- (x,-1)))
;	in let f = proc (z) 11
;		in (f (infinite-loop 0))
; Syntax repesentation of example page 136
(define ep136
  (a-program (letrec-exp 
              (list 'infinite-loop)
              (list 'x)
              (list (call-exp 
               (var-exp 'infinite-loop) 
               (diff-exp 
                (var-exp 'x)	
                (const-exp -1))))
              (letlaz-exp 
				'f
				(proc-exp 'z (const-exp 11))
				(call-exp 
				  (var-exp 'f) 
				  (call-exp 
                                   (var-exp 'infinite-loop)
                                   (const-exp 0)))))))

(define letfails 
   (a-program (letrec-exp 
              (list 'infinite-loop)
              (list 'x)
              (list (call-exp 
               (var-exp 'infinite-loop) 
               (diff-exp 
                (var-exp 'x)	
                (const-exp -1))))
              (let-exp 
				'f ;Evaluating f first would give an infinite loop.
				(call-exp (var-exp 'infinite-loop) (const-exp 99))
				(const-exp 0))))) ; which it does with let-exp.

(define letlazworks 
   (a-program (letrec-exp 
              (list 'infinite-loop)
              (list 'x)
              (list (call-exp 
               (var-exp 'infinite-loop) 
               (diff-exp 
                (var-exp 'x)	
                (const-exp -1))))
              (letlaz-exp 
				'f ;Evaluating f first would give an infinite loop.
				(call-exp (var-exp 'infinite-loop) (const-exp 99))
				(const-exp 0))))) ; but letlaz so returns 0.
(define run
  (lambda (prgm)
	(value-of-program prgm)))
