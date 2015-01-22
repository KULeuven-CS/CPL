#lang eopl
(require racket/base)
(require rackunit)
(require "../chapter7/INFERRED/syntax.rkt")
(require "../chapter7/INFERRED/INFERRED.rkt")
(require "../chapter7/INFERRED/infer.rkt")

;; Given the following types below. 
;;	Write for each a program that would have the given type according to the INFERRED language
;; Notice: %x is a var-type
;a) int
(define a
	(a-program 
	  (diff-exp
		(const-exp 3)
		(const-exp 5))))
(check-equal? (type-of-program a) (int-type))

;b) (int -> int) -> (int ->bool)
(define b
	(a-program 
	  (proc-exp 'arg1 (no-type)
		(proc-exp 'arg2 (no-type)
		 (zero?-exp 
		   (diff-exp 
			 (call-exp 
			   (var-exp 'arg1)
			   (const-exp 0))
			 (var-exp 'arg2)))))))

(check-equal? 
  (type-of-program b) 
  (proc-type 
	(proc-type (int-type) (int-type))
	(proc-type (int-type) (bool-type))))
;c) %1

;Option 1
;Type depends on type of var 'f
(define c1
	(a-program 
	  (if-exp 
		(zero?-exp (var-exp 'f))
		(diff-exp 
		  (const-exp 100)
		  (var-exp 'f))
		(zero?-exp (const-exp 1)))))

;Option 2 actually a %1 -> %1 program
;but it's result is a variable type
(define c2
  (a-program
	(proc-exp 'arg (no-type)
			  (var-exp 'arg))))
;Option 3 just a var-exp on which the program type depends.
(define c3
  (a-program (var-exp 'arg)))

;Yet neither seems completley right.

;d) (%1 -> %2) -> (%2 -> int) -> (%1 -> bool)
;(a-program )
;e) %1 -> %2
;(a-program )
;f) (%1 -> %2) -> %1
;(a-program )
