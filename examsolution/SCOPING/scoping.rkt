#lang eopl
(require "../../chapter3/LETREC/syntax.rkt")

(require "../../chapter3/LETREC/LETREC.rkt") ;; VOOR STATIC
;; (require "LETREC-dynamic.rkt") ;; VOOR DYNAMIC

;;let d = 6 
;;in let f = proc(x)
;;	let z = 1 in proc(w)
;;		if zero? z 
;;			then 0 
;;			else letrec double(x) = if zero? x 
;;										then 0 
;;										else ((double (x - 1)) - -2) 
;;				 in (double x) 
;;  in let z = 0 in ((f d) 0)

;; The variable 'd is the number that should be doubled. 
;; Should return d*2 for normal (static scoping) LETREC
;; Should return 0 for dynamics scoping LETREC 

(define scoping
  (a-program
   (let-exp 'd (const-exp 6) 
	(let-exp 'f 
	  (proc-exp 'x 
		(let-exp 'z (const-exp 1) 
			   (proc-exp 'w  
				 (if-exp (zero?-exp (var-exp 'z)) ;Body of proc(w)
					   (const-exp 0)
					   (letrec-exp 
						 'double 
						 'x 
						 (if-exp (zero?-exp (var-exp 'x)) ;Body of double(x)
						   (const-exp 0) ; x is zero
						   (diff-exp     ; x is nonzero
							 (call-exp (var-exp 'double) 
									   (diff-exp (var-exp 'x) (const-exp 1)))
							 (const-exp -2)))
						 (call-exp (var-exp 'double) (var-exp 'x))))))) ; call double(x) 
	  (let-exp 'z (const-exp 0) (call-exp (call-exp (var-exp 'f) (var-exp 'd)) (const-exp 0)))))))

;;STATIC scoping result
;; Result value = #(struct:num-val 12) ( = 2*6)

;;DYNAMIC Scoping result
;; Result value = #(struct:num-val 0)

;; You can run this with "(test scoping)"
(define (test p)
  (begin
    (display (program->string p))
    (display "\n")
    (value-of-program p)))

