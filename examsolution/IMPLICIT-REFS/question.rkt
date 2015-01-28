#lang eopl
(require "syntax.rkt")
(require "IMPLICIT-REFS.rkt")
(require "store.rkt")

;; let d = let f = proc(x) x
;;       in proc(g) begin f:=g; (f 1) end
;; in (d (proc(x)2))

(define ex
  (a-program
   (let-exp 'd
            (let-exp 'f
                     (proc-exp 'x (var-exp 'x))
                     (proc-exp 'g (begin-exp
                                    (assign-exp 'f (var-exp 'g))
                                    (list (call-exp (var-exp 'f) (const-exp 1))))))
            (call-exp (var-exp 'd) (proc-exp 'x (const-exp 2))))))

;; SOURCE:
;; let d = let f = proc(x)x in proc(g)begin f := g;(f 1) end in (d proc(x)2)
;; 
;; RESULT:
;; Evaluating let d = let f = proc(x)x in proc(g)begin f := g;(f 1) end in (d proc(x)2)
;;     env [empty-env]
;;     store ()
;; Evaluating let f = proc(x)x in proc(g)begin f := g;(f 1) end
;;     env [empty-env]
;;     store ()
;; Evaluating proc(x)x
;;     env [empty-env]
;;     store ()
;; Evaluating proc(g)begin f := g;(f 1) end
;;     env [f = 0, empty-env]
;;     store ((0 #(struct:proc-val #(struct:procedure x #(struct:var-exp x) #(struct:empty-env)))))
;; Evaluating (d proc(x)2)
;;     env [d = 1, empty-env]
;;     store ((0 #(struct:proc-val #(struct:procedure x #(struct:var-exp x) #(struct:empty-env)))) (1 #(struct:proc-val #(;; struct:procedure g #(struct:begin-exp #(struct:assign-exp f #(struct:var-exp g)) (#(struct:call-exp #(struct:var-;; exp f) #(struct:const-exp 1)))) #(struct:extend-env f 0 #(struct:empty-env))))))
;; Evaluating d
;;     env [d = 1, empty-env]
;;     store ((0 #(struct:proc-val #(struct:procedure x #(struct:var-exp x) #(struct:empty-env)))) (1 #(struct:proc-val #(;; struct:procedure g #(struct:begin-exp #(struct:assign-exp f #(struct:var-exp g)) (#(struct:call-exp #(struct:var-;; exp f) #(struct:const-exp 1)))) #(struct:extend-env f 0 #(struct:empty-env))))))
;; Evaluating proc(x)2
;;     env [d = 1, empty-env]
;;     store ((0 #(struct:proc-val #(struct:procedure x #(struct:var-exp x) #(struct:empty-env)))) (1 #(struct:proc-val #(;; struct:procedure g #(struct:begin-exp #(struct:assign-exp f #(struct:var-exp g)) (#(struct:call-exp #(struct:var-;; exp f) #(struct:const-exp 1)))) #(struct:extend-env f 0 #(struct:empty-env))))))
;; Evaluating begin f := g;(f 1) end
;;     env [g = 2, f = 0, empty-env]
;;     store ((0 #(struct:proc-val #(struct:procedure x #(struct:var-exp x) #(struct:empty-env)))) (1 #(struct:proc-val #(;; struct:procedure g #(struct:begin-exp #(struct:assign-exp f #(struct:var-exp g)) (#(struct:call-exp #(struct:var-;; exp f) #(struct:const-exp 1)))) #(struct:extend-env f 0 #(struct:empty-env))))) (2 #(struct:proc-val #(;; struct:procedure x #(struct:const-exp 2) #(struct:extend-env d 1 #(struct:empty-env))))))
;; Evaluating f := g
;;     env [g = 2, f = 0, empty-env]
;;     store ((0 #(struct:proc-val #(struct:procedure x #(struct:var-exp x) #(struct:empty-env)))) (1 #(struct:proc-val #(;; struct:procedure g #(struct:begin-exp #(struct:assign-exp f #(struct:var-exp g)) (#(struct:call-exp #(struct:var-;; exp f) #(struct:const-exp 1)))) #(struct:extend-env f 0 #(struct:empty-env))))) (2 #(struct:proc-val #(;; struct:procedure x #(struct:const-exp 2) #(struct:extend-env d 1 #(struct:empty-env))))))
;; Evaluating g
;;     env [g = 2, f = 0, empty-env]
;;     store ((0 #(struct:proc-val #(struct:procedure x #(struct:var-exp x) #(struct:empty-env)))) (1 #(struct:proc-val #(;; struct:procedure g #(struct:begin-exp #(struct:assign-exp f #(struct:var-exp g)) (#(struct:call-exp #(struct:var-;; exp f) #(struct:const-exp 1)))) #(struct:extend-env f 0 #(struct:empty-env))))) (2 #(struct:proc-val #(;; struct:procedure x #(struct:const-exp 2) #(struct:extend-env d 1 #(struct:empty-env))))))
;; Evaluating (f 1)
;;     env [g = 2, f = 0, empty-env]
;;     store ((0 #(struct:proc-val #(struct:procedure x #(struct:const-exp 2) #(struct:extend-env d 1 #(struct:empty-;; env))))) (1 #(struct:proc-val #(struct:procedure g #(struct:begin-exp #(struct:assign-exp f #(struct:var-exp g)) (#;; (struct:call-exp #(struct:var-exp f) #(struct:const-exp 1)))) #(struct:extend-env f 0 #(struct:empty-env))))) (2 #(;; struct:proc-val #(struct:procedure x #(struct:const-exp 2) #(struct:extend-env d 1 #(struct:empty-env))))))
;; Evaluating f
;;     env [g = 2, f = 0, empty-env]
;;     store ((0 #(struct:proc-val #(struct:procedure x #(struct:const-exp 2) #(struct:extend-env d 1 #(struct:empty-;; env))))) (1 #(struct:proc-val #(struct:procedure g #(struct:begin-exp #(struct:assign-exp f #(struct:var-exp g)) (#;; (struct:call-exp #(struct:var-exp f) #(struct:const-exp 1)))) #(struct:extend-env f 0 #(struct:empty-env))))) (2 #(;; struct:proc-val #(struct:procedure x #(struct:const-exp 2) #(struct:extend-env d 1 #(struct:empty-env))))))
;; Evaluating 1
;;     env [g = 2, f = 0, empty-env]
;;     store ((0 #(struct:proc-val #(struct:procedure x #(struct:const-exp 2) #(struct:extend-env d 1 #(struct:empty-;; env))))) (1 #(struct:proc-val #(struct:procedure g #(struct:begin-exp #(struct:assign-exp f #(struct:var-exp g)) (#;; (struct:call-exp #(struct:var-exp f) #(struct:const-exp 1)))) #(struct:extend-env f 0 #(struct:empty-env))))) (2 #(;; struct:proc-val #(struct:procedure x #(struct:const-exp 2) #(struct:extend-env d 1 #(struct:empty-env))))))
;; Evaluating 2
;;     env [x = 3, d = 1, empty-env]
;;     store ((0 #(struct:proc-val #(struct:procedure x #(struct:const-exp 2) #(struct:extend-env d 1 #(struct:empty-;; env))))) (1 #(struct:proc-val #(struct:procedure g #(struct:begin-exp #(struct:assign-exp f #(struct:var-exp g)) (#;; (struct:call-exp #(struct:var-exp f) #(struct:const-exp 1)))) #(struct:extend-env f 0 #(struct:empty-env))))) (2 #(;; struct:proc-val #(struct:procedure x #(struct:const-exp 2) #(struct:extend-env d 1 #(struct:empty-env))))) (3 #(;; struct:num-val 1)))
;; #(struct:num-val 2)
;; 
;; STORE:
;; ((0 #(struct:proc-val #(struct:procedure x #(struct:const-exp 2) #(struct:extend-env d 1 #(struct:empty-env))))) (1 #(;; struct:proc-val #(struct:procedure g #(struct:begin-exp #(struct:assign-exp f #(struct:var-exp g)) (#(struct:call-exp #;; (struct:var-exp f) #(struct:const-exp 1)))) #(struct:extend-env f 0 #(struct:empty-env))))) (2 #(struct:proc-val #(;; struct:procedure x #(struct:const-exp 2) #(struct:extend-env d 1 #(struct:empty-env))))) (3 #(struct:num-val 1)));; 

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