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
; With letrec we can give an unspecified 
; return type of the function we are defining
(define c 
  (a-program 
   (letrec-exp (no-type) ; Proc return type
			   'f ; Proc name
			   'x ; Proc arg name
			   (no-type) ; Proc arg type
			   (call-exp (var-exp 'f) (var-exp 'x)) ; Proc arg body
			   (call-exp (var-exp 'f) (const-exp 3))))) ; Letrec body, type of f.

(check-true (tvar-type? (type-of-program c)))

;d) (%1 -> %2) -> (%2 -> int) -> (%1 -> bool)
;

;e) %1 -> %2
(define e 
  (a-program 
   (letrec-exp (no-type) ; Proc return type
			   'f ; Proc name
			   'x ; Proc arg name
			   (no-type) ; Proc arg type
			   (call-exp (var-exp 'f) (var-exp 'x)) ; Proc arg body
			   (var-exp 'f)))) ; Letrec body, a function

(check-true
  (cases type (type-of-program e)
		 (proc-type (t1 t2) (not (equal? t1 t2)))
		 (else #f)))

(define 'f8
  (a-program
   (proc-exp 'g (no-type)
             (letrec-exp
              (no-type)
              'f
              'a
              (no-type)
              (var-exp 'a)
              (let-exp 'o 
				(call-exp (var-exp 'g) (var-exp 'f))
				(var-exp 'g))))))

;(a-program )
;f) (%1 -> %2) -> %1
; candidates for conversion p17 and p24 from examples
; couldn't figure it out so far.
; (define f2
;   (a-program 
; 	(proc-exp 'f (no-type) ; return function f with unspecified type.
; 			  (letrec-exp 
; 				(no-type) ; return type of some function that should match 2
; 				'n; name of some function
; 				'arg1 ; name argument that should match 1
; 				(no-type)
; 				(let-exp
; 				  'x
; 				  (call-exp (var-exp 'f) (var-exp 'arg1))
; 				  (var-exp 'arg1))
; 				(call-exp 
; 				  (var-exp 'n)
; 				  (var-exp 'f))))))
; (define f3
;  (let-exp 'r
; 		(proc-exp 'f 
; 				  (no-type)
; 				  (proc-exp 'x (no-type) (call-exp (var-exp 'f) (call-exp (var-exp 'f) (var-exp 'x)))))
; 		(var-exp 'r)))
