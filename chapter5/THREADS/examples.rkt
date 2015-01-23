#lang eopl
(require "syntax.rkt")
; (require "store.rkt")
(require "../../chapter4/EXPLICIT-REFS/store.rkt")
(require "THREADS.rkt")

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
                      (begin-exp (set-exp 'x (const-exp 2))
                                 (list (var-exp 'x))))))

; 
; let p = let y = 1 in proc(dummy) begin y := y - 1; y end
; in begin; p(0);p(0);p(0);p(0) end 
(define p12
  (a-program (let-exp 'p (let-exp 'y (const-exp 1)
                                  (proc-exp 'dummy
                                  (begin-exp (set-exp 'y (diff-exp (var-exp 'y) (const-exp 1)))
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
                                  (list (if-exp (unop-exp (zero?-unop) (var-exp 'x))
                                                (const-exp 1)
                                                (begin-exp (set-exp 'x (diff-exp (var-exp 'x) (const-exp 1)))
                                                           (list (call-exp (var-exp 'odd) (const-exp 888)))))
                                        (if-exp (unop-exp (zero?-unop) (var-exp 'x))
                                                (const-exp 0)
                                                (begin-exp (set-exp 'x (diff-exp (var-exp 'x) (const-exp 1)))
                                                           (list (call-exp (var-exp 'even) (const-exp 888))))))
                                  (begin-exp (set-exp 'x (const-exp 13)) 
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
                                           (set-exp 'counter (diff-exp (var-exp 'counter) (const-exp -1)))
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
                                           (set-exp 'counter (diff-exp (var-exp 'counter) (const-exp -1)))
                                           (list (var-exp 'counter)))))
                      (diff-exp (call-exp (var-exp 'g) (const-exp 11)) (call-exp (var-exp 'g) (const-exp 11))))))

; test: two-non-cooperating-threads
; Page 181
; letrec noisy (l) = if null?(l) then 0 
;   else begin print(car(l)); 
;              (noisy cdr(l)) end
; in begin
;   spawn(proc (d) (noisy [1,2,3,4,5])) ;
;   spawn(proc (d) (noisy [6,7,8,9,10])) ;
;   print(100);
;   end
(define p18
  (a-program (letrec-exp '(noisy) '(l) 
                                  (list (if-exp (unop-exp (null?-unop) (var-exp 'l))
                                                (const-exp 0)
                                                (begin-exp (unop-exp (print-unop) (unop-exp (car-unop) (var-exp 'l)))
                                                           (list (call-exp (var-exp 'noisy) (unop-exp (cdr-unop) (var-exp 'l))))))
                                        )
                                  (begin-exp (spawn-exp  (proc-exp 'd (call-exp (var-exp 'noisy) (const-list-exp '(1 2 3 4 5)))) )
                                             (list (spawn-exp  (proc-exp 'd (call-exp (var-exp 'noisy) (const-list-exp '(6 7 8 9 10)))) )
                                                   (unop-exp (print-unop) (const-exp 100)))))))

; two-non-cooperating-threads but now with yield()

; letrec noisy (l) = if null?(l) then 0 
;   else begin print(car(l));
;              yield();
;              (noisy cdr(l)) end
; in begin
;   spawn(proc (d) (noisy [1,2,3,4,5])) ;
;   spawn(proc (d) (noisy [6,7,8,9,10])) ;
;   print(100);
;   end
(define p19
  (a-program (letrec-exp '(noisy) '(l) 
                                  (list (if-exp (unop-exp (null?-unop) (var-exp 'l))
                                                (const-exp 0)
                                                (begin-exp (unop-exp (print-unop) (unop-exp (car-unop) (var-exp 'l)))
                                                           (list (yield-exp) (call-exp (var-exp 'noisy) (unop-exp (cdr-unop) (var-exp 'l))))))
                                        )
                                  (begin-exp (spawn-exp  (proc-exp 'd (call-exp (var-exp 'noisy) (const-list-exp '(1 2 3 4 5)))) )
                                             (list (spawn-exp  (proc-exp 'd (call-exp (var-exp 'noisy) (const-list-exp '(6 7 8 9 10)))) )
                                                   (unop-exp (print-unop) (const-exp 100)))))))

; let buffer = 0
;   in let producer = proc (n) letrec wait(k) = if zero?(k) then set buffer = n else begin print(-(k,-200)); (wait -(k,1)) end
;                              in (wait 5)
;      in let consumer = proc (d) letrec busywait (k) = if zero?(buffer) then begin print(-(k,-100)); (busywait -(k,-1)) end else buffer
;                                 in (busywait 0)
;      in begin spawn(proc (d) (producer 44)); print(300); (consumer 86) end
(define p20
  (a-program (let-exp 'buffer
                      (const-exp 0)
                      (let-exp 'producer
                               (proc-exp 'n (letrec-exp '(wait) '(k) 
                                                        (list (if-exp (unop-exp (zero?-unop) (var-exp 'k))
                                                                      (set-exp 'buffer (var-exp 'n))
                                                                      (begin-exp (unop-exp (print-unop) (diff-exp (var-exp 'k) (const-exp -200)))
                                                                                 (list (call-exp (var-exp 'wait) (diff-exp (var-exp 'k) (const-exp 1))))))
                                                              )
                                                        (call-exp (var-exp 'wait) (const-exp 5))))
                               (let-exp 'consumer
                                        (proc-exp 'd (letrec-exp '(busywait) '(k) 
                                                                 (list (if-exp (unop-exp (zero?-unop) (var-exp 'buffer))
                                                                               (begin-exp (unop-exp (print-unop) (diff-exp (var-exp 'k) (const-exp -100)))
                                                                                          (list (call-exp (var-exp 'busywait) (diff-exp (var-exp 'k) (const-exp -1)))))
                                                                               (var-exp 'buffer)))
                                                                 (call-exp (var-exp 'busywait) (const-exp 0))))
                                        
                                        (begin-exp (spawn-exp  (proc-exp 'd (call-exp (var-exp 'producer) (const-exp 44))))
                                                   (list (unop-exp (print-unop) (const-exp 300))
                                                         (call-exp (var-exp 'consumer) (const-exp 86))
                                                         )))))))

; an unsafe counter
; Page 186 (with a yield expression that returns -1 to trigger the effect)
; HINT: try using timeslices 1, 10 and 100
; let x = 0
; in let incr_x = proc (id) proc (dummy) begin set x = -(x,yield()) ; print(x) end
;    in begin
;       spawn((incr_x 100));
;       spawn((incr_x 200));
;       spawn((incr_x 300)); 
;        end
(define p21
  (a-program (let-exp 'x
                      (const-exp 0)
                      (let-exp 'incr_x
                               (proc-exp 'id (proc-exp 'dummy (begin-exp (set-exp 'x (diff-exp (var-exp 'x) (yield-exp)))
                                                                         (list (unop-exp (print-unop) (var-exp 'x))))))
                               (begin-exp (spawn-exp  (call-exp (var-exp 'incr_x) (const-exp 100)))
                                          (list (spawn-exp  (call-exp (var-exp 'incr_x) (const-exp 200)))
                                                (spawn-exp  (call-exp (var-exp 'incr_x) (const-exp 300)))
                                                         ))))))

; an safe counter
; Page 188 (with a yield expression that returns -1 as above)
; let x = 0
; in let mut = mutex()
; in let incr_x = proc (id) proc (dummy) begin wait(mut); set x = -(x,yield()); signal(mut); print(x) end
;    in begin
;       spawn((incr_x 100));
;       spawn((incr_x 200));
;       spawn((incr_x 300));  
;        end
(define p22
  (a-program (let-exp 'x
                      (const-exp 0)
                      (let-exp 'mut
                               (mutex-exp)
                               (let-exp 'incr_x
                                        (proc-exp 'id (proc-exp 'dummy 
                                                                (begin-exp (wait-exp (var-exp 'mut))
                                                                           (list (set-exp 'x (diff-exp (var-exp 'x) (yield-exp)))
                                                                                 (signal-exp (var-exp 'mut))
                                                                                 (unop-exp (print-unop) (var-exp 'x))))))
                                        (begin-exp (spawn-exp  (call-exp (var-exp 'incr_x) (const-exp 100)))
                                                   (list (spawn-exp  (call-exp (var-exp 'incr_x) (const-exp 200)))
                                                         (spawn-exp  (call-exp (var-exp 'incr_x) (const-exp 300)))
                                                         )))))))
; let m = mutex()  in 
;  begin wait(m);
;        spawn(proc(dummy) begin wait(m);print(300) end);
;        200 
;  end
(define p23 
  (a-program 
   (let-exp 'm
            (mutex-exp)
            (begin-exp (wait-exp (var-exp 'm))
                       (list (spawn-exp  (proc-exp 'dummy 
                                                   (begin-exp (wait-exp (var-exp 'm))
                                                              (list (unop-exp (print-unop) (const-exp 300))
                                                                    
                                                                    ))))
                             (const-exp 200)
                             )))))
(define (test timeslice p)
  (begin
    (display (program->string p))
    (display "\n")
    (value-of-program timeslice p)
    ))



