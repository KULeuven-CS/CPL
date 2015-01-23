#lang eopl
(require "../../chapter3/LETREC/syntax.rkt")

(require "../../chapter3/LETREC/LETREC.rkt") ;; VOOR STATIC
;; (require "LETREC-dynamic.rkt") ;; VOOR DYNAMIC

;; De var 'd is het getal dat verdubbeld moet worden
;; Geeft d*2 terug indien de normale LETREC wordt gebruikt en 0 indien de dynamic versie wordt gebruikt

;; Runnen met door "(test scoping)" uit te voeren
(define scoping
  (a-program
   (let-exp 'd (const-exp 6) 
            (let-exp 'f 
                      (proc-exp 'x 
                                   (let-exp 'z (const-exp 1) 
                                               (proc-exp 'w (if-exp (zero?-exp (var-exp 'z))
                                                                               (const-exp 0)
                                                                               (letrec-exp 'double 'x 
                                                                                           (if-exp (zero?-exp (var-exp 'x)) 
                                                                                                   (const-exp 0)
                                                                                                   (diff-exp (call-exp (var-exp 'double) (diff-exp (var-exp 'x) (const-exp 1)))
                                                                                                             (const-exp -2)))
                                                                                           (call-exp (var-exp 'double) (var-exp 'x)))))))
                      (let-exp 'z (const-exp 0) (call-exp (call-exp (var-exp 'f) (var-exp 'd)) (const-exp 0)))))))

(define (test p)
  (begin
    (display (program->string p))
    (display "\n")
    (value-of-program p)))