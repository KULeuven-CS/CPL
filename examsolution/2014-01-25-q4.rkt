#lang eopl
(require racket/base)
(require rackunit)
(require "../chapter7/INFERRED/syntax.rkt")
(require "../chapter7/INFERRED/INFERRED.rkt")
(require "../chapter7/INFERRED/infer.rkt")

;; Given the following types below. 
;;	Write for each a program that would have the given type according to the INFERRED language
;a) int
(define a
	(a-program 
	  (diff-exp
		(const-exp 3)
		(const-exp 5))))
(check-equal? )

;b) (int -> int) -> (int ->bool)
(a-program let)
;c) %1
(a-program )
;d) (%1 -> %2) -> (%2 -> int) -> (%1 -> bool)
(a-program )
;e) %1 -> %2
(a-program )
;f) (%1 -> %2) -> %1
(a-program )
