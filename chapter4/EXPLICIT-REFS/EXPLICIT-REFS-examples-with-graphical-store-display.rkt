#lang eopl
(require "syntax.rkt")
(require "EXPLICIT-REFS.rkt")
(require "store.rkt")
(require slideshow)
(require racket/draw)

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

; letrec double(x) = if zero?(x) then 0 else (double (x - 1)) - (-2)
; in (double 6)

(define p11
  (a-program (letrec-exp '(double) '(x) 
                         (list (if-exp (zero?-exp (var-exp 'x)) 
                                 (const-exp 0)
                                 (diff-exp (call-exp (var-exp 'double) (diff-exp (var-exp 'x) (const-exp 1)))
                                           (const-exp -2))))
                         (call-exp (var-exp 'double) (const-exp 6)))))

; let x = newref(0) in begin setref(x,1); x end
(define p12
  (a-program (let-exp 'x (newref-exp (const-exp 0))
                      (begin-exp (setref-exp (var-exp 'x) (const-exp 1)) (list (var-exp 'x) )))))

; let x = newref(0) in begin setref(x,1); deref(x) end
(define p13
  (a-program (let-exp 'x (newref-exp (const-exp 0))
                      (begin-exp (setref-exp (var-exp 'x) (const-exp 1)) (list (deref-exp (var-exp 'x)) )))))

; let x = newref(0) in begin setref(x,1); setref(x, deref(x) - 1); deref(x) end
(define p14
  (a-program (let-exp 'x (newref-exp (const-exp 0))
                      (begin-exp (setref-exp (var-exp 'x) (const-exp 1)) 
                                 (list (setref-exp (var-exp 'x) (diff-exp (deref-exp (var-exp 'x)) (const-exp 1))) 
                                       (deref-exp (var-exp 'x)) )))))


; let x = newref(0)
; in letrec even(dummy) = if zero?(deref(x)) then 1 else begin setref(x, deref(x) - 1); (odd 888) end
;           odd(dummy) = if zero?(deref(x)) then 0 else begin setref(x, deref(x) - 1); (even 888) end
;    in begin setref(x,13); (odd 888) end
(define p15
  (a-program (let-exp 'x
                      (newref-exp (const-exp 0))
                      (letrec-exp '(even odd) '(dummy dummy) 
                                  (list (if-exp (zero?-exp (deref-exp (var-exp 'x)))
                                                (const-exp 1)
                                                (begin-exp (setref-exp (var-exp 'x) (diff-exp (deref-exp (var-exp 'x)) (const-exp 1)))
                                                           (list (call-exp (var-exp 'odd) (const-exp 888)))))
                                        (if-exp (zero?-exp (deref-exp (var-exp 'x)))
                                                (const-exp 0)
                                                (begin-exp (setref-exp (var-exp 'x) (diff-exp (deref-exp (var-exp 'x)) (const-exp 1)))
                                                           (list (call-exp (var-exp 'even) (const-exp 888))))))
                                  (begin-exp (setref-exp (var-exp 'x) (const-exp 13)) 
                                             (list (call-exp (var-exp 'odd) (const-exp 888))))))))

; let g = 
;   let counter = newref(0)
;     in proc (dummy)
;         begin
;         setref(counter, -(deref(counter), -1));
;         deref(counter)
;         end
;  in let a = (g 11) in let b = (g 11) in -(a,b)
(define p16 
  (a-program (let-exp 'g
                      (let-exp 'counter
                               (newref-exp (const-exp 0))
                               (proc-exp 'dummy 
                                         (begin-exp 
                                           (setref-exp (var-exp 'counter) (diff-exp (deref-exp (var-exp 'counter)) (const-exp -1)))
                                           (list (deref-exp (var-exp 'counter))))))
                      (let-exp 'a
                               (call-exp (var-exp 'g) (const-exp 11))
                               (let-exp 'b
                                        (call-exp (var-exp 'g) (const-exp 11))
                                        (diff-exp (var-exp 'a) (var-exp 'b)))))))

; store a proc-val
; newref(proc(dummy) 10)
(define p17
  (a-program (newref-exp (proc-exp 'dummy 
                                         (const-exp 10)))))

; create a cyclic memory
; let l1 = newref(0) in let l2 = newref(l1) in setref(l1,l2)
(define p18
  (a-program (let-exp 'l1
                      (newref-exp (const-exp 0))
                      (let-exp 'l2
                               (newref-exp (var-exp 'l1))
                               (setref-exp (var-exp 'l1) (var-exp 'l2))))))
                       

; create aliasing
; let l1 = newref(0) 
;   in let l2 = l1 
;      in begin setref(l1,10);deref(l2) end
(define p19
  (a-program (let-exp 'l1
                      (newref-exp (const-exp 0))
                      (let-exp 'l2
                               (var-exp 'l1)
                               (begin-exp
                                 (setref-exp (var-exp 'l1) (const-exp 10))
                                 (list (deref-exp (var-exp 'l2))))))))
; create a chain of 2 references
; newref (newref (newref(0)))
(define p20
  (a-program                      
   (newref-exp (newref-exp (newref-exp (const-exp 0))))))

; create a cycle of memory refs of length 10
; let b = newref(0) in
; let e = newref(b) in
; letrec chain(n) = 
;   if iszero? n 
;     then 0 
;     else 
;      let x = newref(0) in begin; 
;                           setref(deref(e),x); 
;                           setref(e,x); 
;                           chain(n-1) 
;                           end
; in begin chain(10); setref(deref(e),b) end
(define p21
  (a-program 
   (let-exp 'b
            (newref-exp (const-exp 0))
            (let-exp 'e
                     (newref-exp (var-exp 'b))
                     (letrec-exp '(chain) '(n) 
                         (list (if-exp (zero?-exp (var-exp 'n)) 
                                 (const-exp 0)
                                 (let-exp 'x
                                          (newref-exp (const-exp 0))
                                          (begin-exp
                                            (setref-exp (deref-exp (var-exp 'e)) (var-exp 'x))
                                            (list (setref-exp (var-exp 'e) (var-exp 'x))
                                                  (call-exp (var-exp 'chain) (diff-exp (var-exp 'n) (const-exp 1))))))
                                 ))
                         (begin-exp
                                            (call-exp (var-exp 'chain) (const-exp 10))
                                            (list (setref-exp (deref-exp (var-exp 'e)) (var-exp 'b))
                                                  ))
                         )))))


; Draw a graphical version of a store, given as a list.
(define print-store
  (lambda (the-store)
    (let ((cells (build-list 
                  (length the-store) 
                  (lambda (n)
                    (hc-append -1
                     (cc-superimpose (rectangle 20 20)  (text (~a n))) 
                     (cc-superimpose (rectangle 300 20) (text (expval->hrstring (list-ref the-store n)))))))))
         (let ((combined (hc-append -1 
                                    (foldr (lambda (n1 n2) 
                                             (vl-append -1 n1 n2))
                                           (blank 0) 
                                           cells)
                                    (blank 200 (- (* 20 (length the-store)) (length the-store))))))
              (foldr (lambda (c s cp) 
                       (cases expval s
                         (ref-val (r) (pin-arrow-line 4 cp
                                                      c rc-find
                                                      (list-ref cells r) rc-find
                                                      #:color (make-color (random 255)
                                                                          (random 255)
                                                                          (random 255))
                                                      #:start-angle 0
                                                      #:end-angle pi
                                                      #:start-pull 2/4
                                                      #:end-pull 2/4
                                                      ))
                         (else cp)))
                     combined
                     cells
                     the-store)))))

; Convert an ExpVal to a human readable string.
; Note that:
;   * proc-vals, representing processes, will only have
;     their body printed.
(define expval->hrstring
  (lambda (v)
    (cases expval v
      (proc-val (p) 
        (cases proc p
          (procedure (sym body env) (string-append "procedure: " (exp->string body)))
          (else (~a "Unknown proc."))))
      (num-val (n) (string-append "number: " (~a n)))
      (bool-val (b) (string-append "boolean: " (~a b)))
      (ref-val (r) (string-append "reference: " (~a r)))
      (else (~a "Unknown expval.")))))
                              
(define (test p)
  (begin
    (display "SOURCE:\n")
    (display (program->string p))
    (display "\n\n")
    (display "RESULT:\n")
    (display (value-of-program p))
    (display "\n\n")
    (display "STORE (TEXT):\n")
    (display (get-store-as-list))
    (display "\n\nSTORE (GRAPHICAL):\n")
    (print-store (get-store))))



