#lang eopl
(require "syntax.rkt")
(require "EXCEPTIONS.rkt")

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

; letrec double(x) = 
;   if zero?(x) then 0 else (double (x - 1)) - (-2)
; in (double 6)

(define p11
  (a-program (letrec-exp 'double 'x 
                         (if-exp (unop-exp (zero?-unop) (var-exp 'x)) 
                                 (const-exp 0)
                                 (diff-exp (call-exp (var-exp 'double) (diff-exp (var-exp 'x) (const-exp 1)))
                                           (const-exp -2)))
                         (call-exp (var-exp 'double) (const-exp 6)))))

;let f = proc(x) 10 in
;  letrec f(x) = if zero? x then 1 else (f (x - 1))
;     in (f 3)
(define p12
  (a-program 
   (let-exp 'f 
            (proc-exp 'x 
                      (const-exp 10))
            (letrec-exp 'f 
                        'x 
                      (if-exp (unop-exp (zero?-unop) (var-exp 'x))
                              (const-exp 1)
                              (call-exp (var-exp 'f) (diff-exp (var-exp 'x) (const-exp 1))))
                     (call-exp (var-exp 'f) (const-exp 3))
                     ))))

; (1 2 3)
(define p13
  (a-program 
   (const-list-exp '(1 2 3))))

; car (1 2 3)
(define p14
  (a-program 
   (unop-exp (car-unop) (const-list-exp '(1 2 3)))))

; cdr (1 2 3)
(define p15
  (a-program 
   (unop-exp (cdr-unop) (const-list-exp '(1 2 3)))))

; cdr (1)
(define p16
  (a-program 
   (unop-exp (cdr-unop) (const-list-exp '(1)))))

; null?( cdr (1))
(define p17
  (a-program 
   (unop-exp (null?-unop) (unop-exp (cdr-unop) (const-list-exp '(1))))))

; let safecdr = proc (l) if null? l then raise 0 else cdr(l)
; in try safecdr(()) handle(x) x
(define p18
  (a-program 
   (let-exp 'safecdr 
            (proc-exp 'l 
                      (if-exp (unop-exp (null?-unop) (var-exp 'l))
                              (raise-exp (const-exp 0))
                              (unop-exp (cdr-unop) (var-exp 'l))))
            (try-exp (call-exp (var-exp 'safecdr) (const-list-exp '()))
                     'x
                     (var-exp 'x))
            )))

; let safecdr = proc (l) if null? l then raise 0 else cdr(l)
; in try safecdr((1 2)) handle(x) x
(define p19
  (a-program 
   (let-exp 'safecdr 
            (proc-exp 'l 
                      (if-exp (unop-exp (null?-unop) (var-exp 'l))
                              (raise-exp (const-exp 0))
                              (unop-exp (cdr-unop) (var-exp 'l))))
            (try-exp (call-exp (var-exp 'safecdr) (const-list-exp '(1 2)))
                     'x
                     (var-exp 'x))
            )))

; let safecdr = proc (l) if null? l then raise 0 else cdr(l)
; in try safecdr(()) - 1 handle(x) x
(define p20
  (a-program 
   (let-exp 'safecdr 
            (proc-exp 'l 
                      (if-exp (unop-exp (null?-unop) (var-exp 'l))
                              (raise-exp (const-exp 0))
                              (unop-exp (cdr-unop) (var-exp 'l))))
            (try-exp (diff-exp (call-exp (var-exp 'safecdr) (const-list-exp '()))
                               (const-exp 1))
                     'x
                     (var-exp 'x))
            )))

; uncaught exception:
; letrec double(x) = 
;   if zero?(x) then raise 0 else (double (x - 1)) - (-2)
; in (double 6)
(define p21
  (a-program (letrec-exp 'double 'x 
                         (if-exp (unop-exp (zero?-unop) (var-exp 'x)) 
                                 (raise-exp (const-exp 0))
                                 (diff-exp (call-exp (var-exp 'double) (diff-exp (var-exp 'x) (const-exp 1)))
                                           (const-exp -2)))
                         (call-exp (var-exp 'double) (const-exp 6)))))

; catching a deep exception:
; letrec double(x) = 
;   if zero?(x) then raise 0 else (double (x - 1)) - (-2)
; in try (double 6) catch(x) x
(define p22
  (a-program (letrec-exp 'double 'x 
                         (if-exp (unop-exp (zero?-unop) (var-exp 'x)) 
                                 (raise-exp (const-exp 0))
                                 (diff-exp (call-exp (var-exp 'double) (diff-exp (var-exp 'x) (const-exp 1)))
                                           (const-exp -2)))
                         (try-exp (call-exp (var-exp 'double) (const-exp 6))
                                  'x
                                  (var-exp 'x))
                         )))

(define (test p)
  (begin
    (display (program->string p))
    (display "\n")
    (value-of-program p)
    ))



