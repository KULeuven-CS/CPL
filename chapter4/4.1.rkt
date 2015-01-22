#lang racket

(require "EXPLICIT-REFS/syntax.rkt")
(require "EXPLICIT-REFS/EXPLICIT-REFS.rkt")
(require "EXPLICIT-REFS/store.rkt")

; let g =  proc (dummy)
;   let counter = newref(0)
;   in
;       begin
;         setref(counter, -(deref(counter), -1));
;         deref(counter)
;       end
;  in let a = (g 11) in let b = (g 11) in -(a,b)

(define p 
  (a-program (let-exp 'g
                      (proc-exp 'dummy 
                               (let-exp 'counter
                               (newref-exp (const-exp 0))
                                         (begin-exp 
                                           (setref-exp (var-exp 'counter) (diff-exp (deref-exp (var-exp 'counter)) (const-exp -1)))
                                           (list (deref-exp (var-exp 'counter))))))
                      (let-exp 'a
                               (call-exp (var-exp 'g) (const-exp 11))
                               (let-exp 'b
                                        (call-exp (var-exp 'g) (const-exp 11))
                                        (diff-exp (var-exp 'a) (var-exp 'b)))))))

(define (test p)
  (begin
    (display "SOURCE:\n")
    (display (program->string p))
    (display "\n\n")
    (display "RESULT:\n")
    (display (value-of-program p))
    (display "\n\n")
    (display "STORE:\n")
    (display (get-store-as-list))
    (display "\n----------------------\n")))
