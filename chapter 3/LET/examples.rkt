#lang eopl
(require "syntax.rkt")
(require "LET.rkt")

;;; Example programs
;;; Initial bindings: i=1, v=5, x=10

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

; xx
; Interpreter will give an error!
(define p8
  (a-program (var-exp 'xx)))

; let y = 0 in let y = y - 1 in y
(define p9
  (a-program (let-exp 'y (const-exp 0)
                      (let-exp 'y (diff-exp (var-exp 'y) (const-exp 1))
                               (var-exp 'y)))))

; zero? 10
(define p10
  (a-program (zero?-exp (const-exp 10))))

(define (test p)
  (begin
    (display (program->string p))
    (display "\n")
    (display (expval->string (value-of-program p)))
    (display "\n----------------\n")))

(define examples (list p1 p2 p3 p4 p5 p6 p7 p9 p10)) ; not p8 because it fails

;;; Exercise 3.6

(define ex306
  (a-program (let-exp 'y (minus-exp (const-exp 10)) (var-exp 'y))))

;;; Exercise 3.8
(define ex308
  (a-program (let-exp 'a (minus-exp (const-exp 10)) (let-exp 'b (diff-exp (const-exp 0) (const-exp 10)) (equal?-exp (var-exp 'a) (var-exp 'b))))))