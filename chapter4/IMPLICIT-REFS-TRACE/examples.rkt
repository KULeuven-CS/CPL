#lang eopl
(require "syntax.rkt")
(require "IMPLICIT-REFS.rkt")
(require "store.rkt")

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

; let x = 1 in begin x:=2; x end;
(define p11
  (a-program (let-exp 'x (const-exp 1)
                      (begin-exp (assign-exp 'x (const-exp 2))
                                 (list (var-exp 'x))))))

; 
; let p = let y = 1 in proc(dummy) begin y := y - 1; y end
; in begin; p(0);p(0);p(0);p(0) end 
(define p12
  (a-program (let-exp 'p (let-exp 'y (const-exp 1)
                                  (proc-exp 'dummy
                                  (begin-exp (assign-exp 'y (diff-exp (var-exp 'y) (const-exp 1)))
                                             (list (var-exp 'y)))))
                      (begin-exp (call-exp (var-exp 'p) (const-exp 0))
                                 (list (call-exp (var-exp 'p) (const-exp 0)) (call-exp (var-exp 'p) (const-exp 0)) (call-exp (var-exp 'p) (const-exp 0)))))))

; let x = 0
; in letrec even(dummy) = if zero? x then 1 else begin x:= x-1; (odd 888) end
;           odd(dummy) = if zero? x then 0 else begin x:= x-1; (even 888) end
;    in begin x := 13; (odd 888) end
(define p15
  (a-program (let-exp 'x
                      (const-exp 0)
                      (letrec-exp '(even odd) '(dummy dummy) 
                                  (list (if-exp (zero?-exp (var-exp 'x))
                                                (const-exp 1)
                                                (begin-exp (assign-exp 'x (diff-exp (var-exp 'x) (const-exp 1)))
                                                           (list (call-exp (var-exp 'odd) (const-exp 888)))))
                                        (if-exp (zero?-exp (var-exp 'x))
                                                (const-exp 0)
                                                (begin-exp (assign-exp 'x (diff-exp (var-exp 'x) (const-exp 1)))
                                                           (list (call-exp (var-exp 'even) (const-exp 888))))))
                                  (begin-exp (assign-exp 'x (const-exp 13)) 
                                             (list (call-exp (var-exp 'odd) (const-exp 888))))))))

; let g = 
;   let counter = 0
;     in proc (dummy)
;         begin
;         counter := counter - -1;
;         counter
;         end
;  in let a = (g 11) in let b = (g 11) in a - b
(define p16 
  (a-program (let-exp 'g
                      (let-exp 'counter
                               (const-exp 0)
                               (proc-exp 'dummy 
                                         (begin-exp 
                                           (assign-exp 'counter (diff-exp (var-exp 'counter) (const-exp -1)))
                                           (list (var-exp 'counter)))))
                      (let-exp 'a
                               (call-exp (var-exp 'g) (const-exp 11))
                               (let-exp 'b
                                        (call-exp (var-exp 'g) (const-exp 11))
                                        (diff-exp (var-exp 'a) (var-exp 'b)))))))

; illustrate lack of referential transparency:
; let g = 
;   let counter = 0
;     in proc (dummy)
;         begin
;         counter := counter - -1;
;         counter
;         end
;  in (g 11) - (g 11)
(define p17 
  (a-program (let-exp 'g
                      (let-exp 'counter
                               (const-exp 0)
                               (proc-exp 'dummy 
                                         (begin-exp 
                                           (assign-exp 'counter (diff-exp (var-exp 'counter) (const-exp -1)))
                                           (list (var-exp 'counter)))))
                      (diff-exp (call-exp (var-exp 'g) (const-exp 11)) (call-exp (var-exp 'g) (const-exp 11))))))

; letrec uses more memory than needed
; letrec f(x) = 1
; in begin f;f;f end
(define p19
  (a-program (letrec-exp '(f) '(x) 
                                  (list (const-exp 1)
                                        )
                                  (begin-exp (var-exp 'f) 
                                             (list (var-exp 'f) (var-exp 'f) )))))
(define ex
  (a-program
   (let-exp 'd
            (let-exp 'f
                     (proc-exp 'x (var-exp 'x))
                     (proc-exp 'g (begin-exp
                                    (assign-exp 'f (var-exp 'g))
                                    (list (call-exp (var-exp 'f) (const-exp 1))))))
            (call-exp (var-exp 'd) (proc-exp 'x (const-exp 2))))))

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



