#lang eopl
(require "syntax.rkt")
(require "CHECKED.rkt")
(require "checker.rkt")

;;; Example programs

; 2
(define p1 (a-program (const-exp 2)))

; 2 - 4
(define p2 (a-program (diff-exp (const-exp 2) (const-exp 4))))

; 10 - (2 - 4)
(define p3 
  (a-program (diff-exp (const-exp 10) 
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

; let f = proc(x:int) (x - 11) in (f (f 7))
(define p8
  (a-program (let-exp 'f (proc-exp 'x (int-type) (diff-exp (var-exp 'x) (const-exp 11)))
                      (call-exp (var-exp 'f) (call-exp (var-exp 'f) (const-exp 7))))))

;( proc (f:int -> int) (f (f 77)) 
;  proc (x:int) (x - 11) )
(define p9
  (a-program (call-exp (proc-exp 'f (proc-type (int-type) (int-type)) (call-exp (var-exp 'f) (call-exp (var-exp 'f) (const-exp 77))))
                       (proc-exp 'x (int-type) (diff-exp (var-exp 'x) (const-exp 11))))))

; let x = 200
; in let f = proc(z:int) (z - x)
;    in let x = 100
;       in let g = proc (z:int) (z - x)
;          in (f 1) - (g 1)
(define p10
  (a-program (let-exp 'x (const-exp 200)
                      (let-exp 'f (proc-exp 'z (int-type) (diff-exp (var-exp 'z) (var-exp 'x)))
                               (let-exp 'x (const-exp 100)
                                        (let-exp 'g (proc-exp 'z (int-type) (diff-exp (var-exp 'z) (var-exp 'x)))
                                                 (diff-exp (call-exp (var-exp 'f) (const-exp 1))
                                                           (call-exp (var-exp 'g) (const-exp 1)))))))))

; letrec int double(x:int) = 
;   if zero?(x) then 0 else (double (x - 1)) - (-2)
; in (double 6)

(define p11
  (a-program (letrec-exp (int-type) 'double 'x (int-type)
                         (if-exp (zero?-exp (var-exp 'x)) 
                                 (const-exp 0)
                                 (diff-exp (call-exp (var-exp 'double) (diff-exp (var-exp 'x) (const-exp 1)))
                                           (const-exp -2)))
                         (call-exp (var-exp 'double) (const-exp 6)))))

;let f = proc(x:int) 10 in
;  letrec int f(x:int) = if zero? x then 1 else (f (x - 1))
;     in (f 3)
(define p12
  (a-program 
   (let-exp 'f 
            (proc-exp 'x (int-type)
                      (const-exp 10))
            (letrec-exp (int-type) 'f 
                        'x (int-type)
                      (if-exp (zero?-exp (var-exp 'x))
                              (const-exp 1)
                              (call-exp (var-exp 'f) (diff-exp (var-exp 'x) (const-exp 1))))
                     (call-exp (var-exp 'f) (const-exp 3))
                     ))))

; TYPE ERROR
; let f = proc(x:int) (x - 11) in (f zero?(7)))
(define p13
  (a-program (let-exp 'f (proc-exp 'x (int-type) (diff-exp (var-exp 'x) (const-exp 11)))
                      (call-exp (var-exp 'f) (zero?-exp  (const-exp 7))))))

;let twice = proc(f:int->int) proc(x:int) f(f(x)) in twice
(define p14
  (a-program 
   (let-exp 'twice 
            (proc-exp 'f 
                      (proc-type (int-type) (int-type))
                      (proc-exp 'x (int-type) (call-exp (var-exp 'f) (call-exp (var-exp 'f) (var-exp 'x)))))
            (var-exp 'twice))))

;let twice = proc(f:int->int) proc(x:int) f(f(x)) in (twice proc(x:int) x)
(define p15
  (a-program 
   (let-exp 'twice 
            (proc-exp 'f 
                      (proc-type (int-type) (int-type))
                      (proc-exp 'x (int-type) (call-exp (var-exp 'f) (call-exp (var-exp 'f) (var-exp 'x)))))
            (call-exp (var-exp 'twice) (proc-exp 'x (int-type) (var-exp 'x))))))

; proc (x:int) zero?x
(define p16
  (a-program 
   (proc-exp 'x (int-type) (zero?-exp  (var-exp 'x)) )))

(define (test p)
  (begin
    (display "PROGRAM:\n")
    (display (program->string p))
    (display "\n")
    (display "TYPE:\n")
    (display (type->string (type-of-program p)))
    (display "\n")
    
    (value-of-program p)
    )
  )



