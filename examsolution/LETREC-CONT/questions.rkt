#lang eopl
(require "syntax.rkt")
(require "LETREC-CONT.rkt")

;; let x=0 in (1-x)

(define ex1
  (a-program
   (let-exp 'x (const-exp 0) (diff-exp (const-exp 1) (var-exp 'x)))))

;; Evaluating let x = 0 in (1 - x) in cc [ ]
;; Evaluating 0 in cc let x = [ ] in <(1 - x)>
;; Sending Number: 0 to cc let x = [ ] in <(1 - x)>
;; Evaluating (1 - x) in cc [ ]
;; Evaluating 1 in cc ([ ] - <x>)
;; Sending Number: 1 to cc ([ ] - <x>)
;; Evaluating x in cc (1 - [ ])
;; Sending Number: 0 to cc (1 - [ ])
;; Sending Number: 1 to cc [ ]
;; End of computation.



;; (15 - if zero?0 then 1 else 

(define ex2
  (a-program
   (diff-exp (const-exp 15)
             (if-exp (zero?-exp (const-exp 0))
                     (const-exp 1)
                     (const-exp 2)))))

;; Evaluating (15 - if zero? 0 then 1 else 2) in cc [ ]
;; Evaluating 15 in cc ([ ] - <if zero? 0 then 1 else 2>)
;; Sending Number: 15 to cc ([ ] - <if zero? 0 then 1 else 2>)
;; Evaluating if zero? 0 then 1 else 2 in cc (15 - [ ])
;; Evaluating zero? 0 in cc (15 - if [ ] then <1> else <2>)
;; Evaluating 0 in cc (15 - if zero?([ ]) then <1> else <2>)
;; Sending Number: 0 to cc (15 - if zero?([ ]) then <1> else <2>)
;; Sending Boolean: #t to cc (15 - if [ ] then <1> else <2>)
;; Evaluating 1 in cc (15 - [ ])
;; Sending Number: 1 to cc (15 - [ ])
;; Sending Number: 14 to cc [ ]
;; End of computation.;; 



;; let double = proc (f) proc (x) (f(f x)) in ((double proc(x) (x-1)3)

(define ex3
  (a-program
   (let-exp 'double
            (proc-exp 'f
                      (proc-exp 'x
                                (call-exp  (var-exp 'f)
                                           (call-exp (var-exp 'f)
                                                     (var-exp 'x)))))
            (call-exp (call-exp (var-exp 'double)
                                (proc-exp 'x
                                          (diff-exp (var-exp 'x)
                                                    (const-exp 1))))
                      (const-exp 3)))))

;; Evaluating let double = proc(f)proc(x)(f (f x)) in ((double proc(x)(x - 1)) 3) in cc [ ]
;; Evaluating proc(f)proc(x)(f (f x)) in cc let double = [ ] in <((double proc(x)(x - 1)) 3)>
;; Sending Procedure: {proc}  to cc let double = [ ] in <((double proc(x)(x - 1)) 3)>
;; Evaluating ((double proc(x)(x - 1)) 3) in cc [ ]
;; Evaluating (double proc(x)(x - 1)) in cc ([ ] <3>)
;; Evaluating double in cc (([ ] <proc(x)(x - 1)>) <3>)
;; Sending Procedure: {proc}  to cc (([ ] <proc(x)(x - 1)>) <3>)
;; Evaluating proc(x)(x - 1) in cc (({proc} [ ] <3>)
;; Sending Procedure: {proc}  to cc (({proc} [ ] <3>)
;; Evaluating proc(x)(f (f x)) in cc ([ ] <3>)
;; Sending Procedure: {proc}  to cc ([ ] <3>)
;; Evaluating 3 in cc ({proc} [ ]
;; Sending Number: 3 to cc ({proc} [ ]
;; Evaluating (f (f x)) in cc [ ]
;; Evaluating f in cc ([ ] <(f x)>)
;; Sending Procedure: {proc}  to cc ([ ] <(f x)>)
;; Evaluating (f x) in cc ({proc} [ ]
;; Evaluating f in cc ({proc} ([ ] <x>)
;; Sending Procedure: {proc}  to cc ({proc} ([ ] <x>)
;; Evaluating x in cc ({proc} ({proc} [ ]
;; Sending Number: 3 to cc ({proc} ({proc} [ ]
;; Evaluating (x - 1) in cc ({proc} [ ]
;; Evaluating x in cc ({proc} ([ ] - <1>)
;; Sending Number: 3 to cc ({proc} ([ ] - <1>)
;; Evaluating 1 in cc ({proc} (3 - [ ])
;; Sending Number: 1 to cc ({proc} (3 - [ ])
;; Sending Number: 2 to cc ({proc} [ ]
;; Evaluating (x - 1) in cc [ ]
;; Evaluating x in cc ([ ] - <1>)
;; Sending Number: 2 to cc ([ ] - <1>)
;; Evaluating 1 in cc (2 - [ ])
;; Sending Number: 1 to cc (2 - [ ])
;; Sending Number: 1 to cc [ ]
;; End of computation.
;; #(struct:num-val 1);; 



(define (test p)
  (begin
    (display (program->string p))
    (display "\n")
    (value-of-program p)))


;; 