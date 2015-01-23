#lang eopl
(require "syntax.rkt")
(require "SIMPLEMODULES.rkt")
(require "checker.rkt")

;;; Example programs

; module m1
;  interface 
;    [a : int
;    b : int]
;  body
;    [a = 33
;     c = -(a,1)
;     b = -(c,a)]
; let a = 10
;  in -(-(from m1 take a, from m1 take b), a)

(define p1
  (a-program
   (list (a-module-definition 
          'm1
          (simple-iface (list (val-decl 'a (int-type)) (val-decl 'b (int-type))))
          (defns-module-body (list (val-defn 'a (const-exp 33)) 
                                   (val-defn 'c (diff-exp (var-exp 'a) (const-exp 1)))
                                   (val-defn 'b (diff-exp (var-exp 'c) (var-exp 'a)))))))
   (let-exp 'a
            (const-exp 10)
            (diff-exp (diff-exp (qualified-var-exp 'm1 'a) (qualified-var-exp 'm1 'b)) (var-exp 'a)))))

; NOTE: TYPE ERROR!
; module m1 
;  interface 
;   [u : bool]
;  body 
;   [u = 33]
; 44

(define p2
  (a-program
   (list (a-module-definition 
          'm1
          (simple-iface (list (val-decl 'u (bool-type))))
          (defns-module-body (list (val-defn 'u (const-exp 33)) ))))
   (const-exp 44)))

; NOTE: TYPE ERROR: undefined v
; module m1 
;  interface 
;   [u : int
;    v : int]
;  body 
;   [u = 33]
; 44

(define p3
  (a-program
   (list (a-module-definition 
          'm1
          (simple-iface (list (val-decl 'u (int-type)) (val-decl 'v (int-type))))
          (defns-module-body (list (val-defn 'u (const-exp 33)) ))))
   (const-exp 44)))

; NOTE: TYPE ERROR: order of defns
; module m1 
;  interface 
;   [u : int
;    v : int]
;  body 
;   [v = 33
;    u = 44]
; from m1 take u
(define p4
  (a-program
   (list (a-module-definition 
          'm1
          (simple-iface (list (val-decl 'u (int-type)) (val-decl 'v (int-type))))
          (defns-module-body (list (val-defn 'v (const-exp 33)) (val-defn 'u (const-exp 44)) ))))
   (qualified-var-exp 'm1 'u)))

; module m1 
;  interface 
;   [u : int] 
;  body 
;   [u = 44]
; module m2 
;  interface
;   [v : int] 
;  body 
;   [v = -(from m1 take u,11)]
;-(from m1 take u, from m2 take v)
(define p5
  (a-program
   (list (a-module-definition 
          'm1
          (simple-iface (list (val-decl 'u (int-type)) ))
          (defns-module-body (list (val-defn 'u (const-exp 44)) )))
         (a-module-definition 
          'm2
          (simple-iface (list (val-decl 'v (int-type)) ))
          (defns-module-body (list (val-defn 'v (diff-exp (qualified-var-exp 'm1 'u) (const-exp 11))) ))))
   (diff-exp (qualified-var-exp 'm1 'u) (qualified-var-exp 'm2 'v))))

; ERROR: order of modules
; module m2 
;  interface
;   [v : int] 
;  body 
;   [v = -(from m1 take u,11)]
; module m1 
;  interface 
;   [u : int] 
;  body 
;   [u = 44]
;-(from m1 take u, from m2 take v)

(define p6
  (a-program
   (list (a-module-definition 
          'm2
          (simple-iface (list (val-decl 'v (int-type)) ))
          (defns-module-body (list (val-defn 'v (diff-exp (qualified-var-exp 'm1 'u) (const-exp 11))) )))
         (a-module-definition 
          'm1
          (simple-iface (list (val-decl 'u (int-type)) ))
          (defns-module-body (list (val-defn 'u (const-exp 44)) )))
         )
   (diff-exp (qualified-var-exp 'm1 'u) (qualified-var-exp 'm2 'v))))

; module m1
;  interface 
;    [a : int
;    f : int -> int]
;  body
;    [a = 33
;     f = proc (x:int) x]
;  in ((from m1 take f) (from m1 take a))


(define p7
  (a-program
   (list (a-module-definition 
          'm1
          (simple-iface (list (val-decl 'a (int-type)) (val-decl 'f (proc-type (int-type) (int-type)))))
          (defns-module-body (list (val-defn 'a (const-exp 33))
                                   (val-defn 'f (proc-exp 'x (int-type) (var-exp 'x)))))))
   (call-exp (qualified-var-exp 'm1 'f) (qualified-var-exp 'm1 'a))))

(define (test p)
  (begin
    (display (program->string p)) 
    (display "\n")
    (value-of-program p)
    (type-of-program p)
    ))



