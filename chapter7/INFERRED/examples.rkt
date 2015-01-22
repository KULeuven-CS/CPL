#lang eopl
(require "syntax.rkt")
(require "INFERRED.rkt")
(require "infer.rkt")

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

; let f = proc(x:?) (x - 11) in (f (f 7))
(define p8
  (a-program (let-exp 'f (proc-exp 'x (no-type) (diff-exp (var-exp 'x) (const-exp 11)))
                      (call-exp (var-exp 'f) (call-exp (var-exp 'f) (const-exp 7))))))

;( proc (f:?) (f (f 77)) 
;  proc (x:?) (x - 11) )
(define p9
  (a-program (call-exp (proc-exp 'f (no-type) (call-exp (var-exp 'f) (call-exp (var-exp 'f) (const-exp 77))))
                       (proc-exp 'x (no-type) (diff-exp (var-exp 'x) (const-exp 11))))))

; let x = 200
; in let f = proc(z:?) (z - x)
;    in let x = 100
;       in let g = proc (z:?) (z - x)
;          in (f 1) - (g 1)
(define p10
  (a-program (let-exp 'x (const-exp 200)
                      (let-exp 'f (proc-exp 'z (no-type) (diff-exp (var-exp 'z) (var-exp 'x)))
                               (let-exp 'x (const-exp 100)
                                        (let-exp 'g (proc-exp 'z (no-type) (diff-exp (var-exp 'z) (var-exp 'x)))
                                                 (diff-exp (call-exp (var-exp 'f) (const-exp 1))
                                                           (call-exp (var-exp 'g) (const-exp 1)))))))))

; letrec ? double(x:?) = 
;   if zero?(x) then 0 else (double (x - 1)) - (-2)
; in (double 6)

(define p11
  (a-program (letrec-exp (no-type) 'double 'x (no-type)
                         (if-exp (zero?-exp (var-exp 'x)) 
                                 (const-exp 0)
                                 (diff-exp (call-exp (var-exp 'double) (diff-exp (var-exp 'x) (const-exp 1)))
                                           (const-exp -2)))
                         (call-exp (var-exp 'double) (const-exp 6)))))

;let f = proc(x:?) 10 in
;  letrec ? f(x:?) = if zero? x then 1 else (f (x - 1))
;     in (f 3)
(define p12
  (a-program 
   (let-exp 'f 
            (proc-exp 'x (no-type)
                      (const-exp 10))
            (letrec-exp (no-type) 'f 
                        'x (no-type)
                      (if-exp (zero?-exp (var-exp 'x))
                              (const-exp 1)
                              (call-exp (var-exp 'f) (diff-exp (var-exp 'x) (const-exp 1))))
                     (call-exp (var-exp 'f) (const-exp 3))
                     ))))

; TYPE ERROR
; let f = proc(x:?) (x - 11) in (f zero?(7)))
(define p13
  (a-program (let-exp 'f (proc-exp 'x (no-type) (diff-exp (var-exp 'x) (const-exp 11)))
                      (call-exp (var-exp 'f) (zero?-exp  (const-exp 7))))))

;let twice = proc(f:?) proc(x:?) f(f(x)) in twice
(define p14
  (a-program 
   (let-exp 'twice 
            (proc-exp 'f 
                      (no-type)
                      (proc-exp 'x (no-type) (call-exp (var-exp 'f) (call-exp (var-exp 'f) (var-exp 'x)))))
            (var-exp 'twice))))

;let twice = proc(f:?) proc(x:?) f(f(x)) in (twice proc(x:?) x)
(define p15
  (a-program 
   (let-exp 'twice 
            (proc-exp 'f 
                      (no-type)
                      (proc-exp 'x (no-type) (call-exp (var-exp 'f) (call-exp (var-exp 'f) (var-exp 'x)))))
            (call-exp (var-exp 'twice) (proc-exp 'x (no-type) (var-exp 'x))))))

; proc (x:int) zero?x
(define p16
  (a-program 
   (proc-exp 'x (no-type) (zero?-exp  (var-exp 'x)) )))

; proc(f) proc (x) f(x)
(define p17
  (a-program 
   (proc-exp 'f (no-type) (proc-exp 'x (no-type) (call-exp (var-exp 'f) (var-exp 'x)) ) )))

; letrec f(x) = f(x) in f
(define p18
  (a-program 
   (letrec-exp (no-type) 'f 
                        'x (no-type)
                      (call-exp (var-exp 'f) (var-exp 'x))
                     (var-exp 'f)
                     )))

; WARNING: infinite loop!
; letrec f(x) = f(x) in (f 3)
(define p19
  (a-program 
   (letrec-exp (no-type) 'f 
                        'x (no-type)
                      (call-exp (var-exp 'f) (var-exp 'x))
                     (call-exp (var-exp 'f) (const-exp 3))
                     )))

; letrec f(x) = f(x) in zero?(f 3)
(define p20
  (a-program 
   (letrec-exp (no-type) 'f 
                        'x (no-type)
                      (call-exp (var-exp 'f) (var-exp 'x))
                     (zero?-exp (call-exp (var-exp 'f) (const-exp 3)))
                     )))
; let compose = proc (f) proc (g) proc (x) (f (g x)) in compose
(define p21
  (a-program 
   (let-exp 'compose 
            (proc-exp 'f 
                      (no-type)
                      (proc-exp 'g 
                      (no-type)
                      (proc-exp 'x (no-type) (call-exp (var-exp 'f) (call-exp (var-exp 'g) (var-exp 'x))))))
            (var-exp 'compose))))

; let compose = proc (f) proc (g) proc (x) (f (g x)) in (compose (proc (x) x-1))
(define p22
  (a-program 
   (let-exp 'compose 
            (proc-exp 'f 
                      (no-type)
                      (proc-exp 'g 
                      (no-type)
                      (proc-exp 'x (no-type) (call-exp (var-exp 'f) (call-exp (var-exp 'g) (var-exp 'x))))))
            (call-exp (var-exp 'compose) (proc-exp 'x (no-type) (diff-exp (var-exp 'x) (const-exp 1)))))))

; let compose = proc (f) proc (g) proc (x) (f (g x)) in ((compose (proc (x) x-1))(proc (x) x-1))
(define p23
  (a-program 
   (let-exp 'compose 
            (proc-exp 'f 
                      (no-type)
                      (proc-exp 'g 
                      (no-type)
                      (proc-exp 'x (no-type) (call-exp (var-exp 'f) (call-exp (var-exp 'g) (var-exp 'x))))))
            (call-exp 
             (call-exp (var-exp 'compose) 
                       (proc-exp 'x (no-type) (diff-exp (var-exp 'x) (const-exp 1)))) 
             (proc-exp 'x (no-type) (diff-exp (var-exp 'x) (const-exp 1)))))))

; proc(b) proc (a) (b a)
(define p24
  (a-program 
   (proc-exp 'c (no-type) (proc-exp 'b (no-type) (proc-exp 'a (no-type) (call-exp (call-exp (var-exp 'c) (var-exp 'b)) (call-exp (var-exp 'b) (var-exp 'a))) ) ) )))

;let pair = proc (x) proc (f) ((f x) x)
(define p25
  (a-program 
   (let-exp 'pair 
            (proc-exp 'x 
                      (no-type)
                      (proc-exp 'f 
                      (no-type)
                      (call-exp (call-exp (var-exp 'f) (var-exp 'x)) (var-exp 'x))))
            (var-exp 'pair))))

; let id = proc(x:?)x in if zero? 0 then 1 else (id 1)
(define p26
  (a-program 
   (let-exp 'id 
            (proc-exp 'x 
                      (no-type)
                      (var-exp 'x))
            (if-exp (zero?-exp (const-exp 0))
                              (const-exp 1)
                              (call-exp (var-exp 'id) (const-exp 1))))))

; TYPE ERROR
; let id = proc(x:?)x in if (id zero? 0) then 1 else (id 1)
(define p27
  (a-program 
   (let-exp 'id 
            (proc-exp 'x 
                      (no-type)
                      (var-exp 'x))
            (if-exp (call-exp (var-exp 'id) (zero?-exp (const-exp 0)))
                              (const-exp 1)
                              (call-exp (var-exp 'id) (const-exp 1))))))

; but if we substitute id in the body ...
; let id = proc(x:?)x in if (proc(x:?)x zero? 0) then 1 else (proc(x:?)x 1)
(define p28
  (a-program 
   (let-exp 'id 
            (proc-exp 'x 
                      (no-type)
                      (var-exp 'x))
            (if-exp (call-exp (proc-exp 'x 
                      (no-type)
                      (var-exp 'x)) (zero?-exp (const-exp 0)))
                              (const-exp 1)
                              (call-exp (proc-exp 'x 
                      (no-type)
                      (var-exp 'x)) (const-exp 1))))))

(define (test p)
  (begin
    (display "ORIGINAL PROGRAM:\n")
    (display (program->string p))
    (display "\n")
    ;(display "TYPE:\n")
    ;(display (type->string (type-of-program p)))
    (display "\n")
    (print-results p)
    
    ;(value-of-program p)
    )
  )


;; EXPERIMENTAL
;; define a print-results function that prints:
;;   - the program with no-type annotation substituted with type-vars
;;   - the program with all inferred types filled in
;;   - the type of that program

;; apply f to all types in exp
(define (map-exp f exp)
  (cases expression exp     
      (const-exp (num) (const-exp num))   
      (zero?-exp (exp1) (zero?-exp (map-exp f exp1)))   
      (diff-exp (exp1 exp2) (diff-exp (map-exp f exp1) (map-exp f exp2)))
      (if-exp (exp1 exp2 exp3)
              (if-exp (map-exp f exp1) (map-exp f exp2) (map-exp f exp3)))
      (var-exp (var) (var-exp var))
      (let-exp (var exp1 body)
               (let-exp var (map-exp f exp1) (map-exp f body)))
      (proc-exp (var otype body)
                (proc-exp var (f otype) (map-exp f body)))
      (call-exp (rator rand)
                (call-exp (map-exp f rator) (map-exp f rand)))
      (letrec-exp (proc-result-otype proc-name 
                                     bvar proc-arg-otype 
                                     proc-body
                                     letrec-body)
                  (letrec-exp (f proc-result-otype)
                              proc-name
                              bvar (f proc-arg-otype)
                              (map-exp f proc-body)
                              (map-exp f letrec-body)))
      ))

; replace no-types with typevars
(define notype->typevar
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (a-program (map-exp otype->otype exp1))))))
(define otype->otype
  (lambda (otype)
    (cases optional-type otype
      (no-type () (a-type (fresh-tvar-type)))
      (a-type (ty) (a-type ty)))))


; top: Pgm -> String
(define print-results
  (lambda (p)
    (let ((pgm (notype->typevar p)))
      (display (string-append "PROGRAM WITH TYPEVARS:\n" 
                              (program->string pgm)
                              "\n"))
      (cases program pgm
        (a-program (exp1)
                   (cases answer (type-of exp1 (init-tenv) (empty-subst))
                     (an-answer (ty subst)
                                (display (string-append
                                 "PROGRAM WITH INFERRED TYPES:\n"
                                 (program->string (a-program 
                                                   (map-exp (lambda (t) (a-type (apply-subst-to-type (rem-a t) subst))) exp1)))
                                 "\nTYPE:\n"
                                 (type->string (apply-subst-to-type ty subst))))
                                )))))))

(define (rem-a  t)
  (cases optional-type t
      (no-type () '())
      (a-type (ty) ty)))







