#lang eopl
(require "syntax.rkt")
(require "IMPLICITREFS-CONT.rkt")

;;; Example programs

; 2
(define p1 (a-program (const-exp 2)))

; 2 - 4
(define p2 (a-program (diff-exp (const-exp 2) (const-exp 4))))

; 10 - (2 - 4)
(define p3 
  (a-program (diff-exp (const-exp 10) 
                       (diff-exp (const-exp 2) (const-exp 4)))))
; (8 - 6) - (2 - 4)
(define p3b 
  (a-program (diff-exp (diff-exp (const-exp 8) (const-exp 6)) 
                       (diff-exp (const-exp 2) (const-exp 4)))))
; x
(define p4
  (a-program (var-exp 'x)))

; x - 4
(define p5
  (a-program (diff-exp (var-exp 'x) (const-exp 4))))

; let x = 0 in x - 4
(define p6
  (a-program (let-exp 'x 
                                   (const-exp 0) 
                                   (diff-exp (var-exp 'x) (const-exp 4)))))
; let x = 0 in let x = 2 in x
(define p7
  (a-program (let-exp 'x (const-exp 0)
                      (let-exp 'x (const-exp 2)
                               (var-exp 'x)))))

; let f = proc(x) (x - 11) in (f (f 7))
(define p8
  (a-program (let-exp 'f (proc-exp 'x (diff-exp (var-exp 'x) (const-exp 11)))
                      (call-exp (var-exp 'f) (call-exp (var-exp 'f) (const-exp 7))))))

;( proc (f) (f (f 77)) 
;  proc (x) (x - 11) )
(define p9
  (a-program (call-exp (proc-exp 'f (call-exp (var-exp 'f) (call-exp (var-exp 'f) (const-exp 77))))
                       (proc-exp 'x (diff-exp (var-exp 'x) (const-exp 11))))))

; let x = 200
; in let f = proc(z) (z - x)
;    in let x = 100
;       in let g = proc (z) (z - x)
;          in (f 1) - (g 1)
(define p10
  (a-program (let-exp 'x (const-exp 200)
                      (let-exp 'f (proc-exp 'z (diff-exp (var-exp 'z) (var-exp 'x)))
                               (let-exp 'x (const-exp 100)
                                        (let-exp 'g (proc-exp 'z (diff-exp (var-exp 'z) (var-exp 'x)))
                                                 (diff-exp (call-exp (var-exp 'f) (const-exp 1))
                                                           (call-exp (var-exp 'g) (const-exp 1)))))))))

; letrec double(x) = if zero?(x) then 0 else (double (x - 1)) - (-2)
; in (double 6)

(define p11
  (a-program (letrec-exp 'double 'x 
                         (if-exp (zero?-exp (var-exp 'x)) 
                                 (const-exp 0)
                                 (diff-exp (call-exp (var-exp 'double) (diff-exp (var-exp 'x) (const-exp 1)))
                                           (const-exp -2)))
                         (call-exp (var-exp 'double) (const-exp 6)))))

; letrec f(n) = if zero? n then 0 else (f(n-1) - 1) in f(10)
(define p12
  (a-program (letrec-exp 'f 'n 
                         (if-exp (zero?-exp (var-exp 'n)) 
                                 (const-exp 0)
                                 (diff-exp (const-exp 1)
                                           (call-exp (var-exp 'f) (diff-exp (var-exp 'n) (const-exp 1)))
                                           ))
                         (call-exp (var-exp 'f) (const-exp 10)))))

; (if zero? 0 then 1 else 2) - (let x= 5 in x)
(define p13
  (a-program (diff-exp (if-exp (zero?-exp (const-exp 0)) 
                               (const-exp 1)
                               (const-exp 2))
                       (let-exp 'x 
                                (const-exp 5) 
                                (var-exp 'x))
                       )))

; let f = proc(x) 7 in f(2) - f(3)
(define p14
  (a-program (let-exp 'f (proc-exp 'x (const-exp 7))
                      (diff-exp (call-exp (var-exp 'f) (const-exp 2))
                                (call-exp (var-exp 'f) (const-exp 3))))))

(define (test p)
  (begin
    (display (program->string p))
    (display "\n")
    (value-of-program p)))



