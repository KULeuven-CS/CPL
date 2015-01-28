# IMPLICIT-REFS

Extension of IMPLIC-REFS to show traces of what's going on in the environment and the store.

## Example

```racket
;; let f = proc(x)(x - 11) in (f (f 7))
(define p8
  (a-program (let-exp 'f (proc-exp 'x (diff-exp (var-exp 'x) (const-exp 11)))
                      (call-exp (var-exp 'f) (call-exp (var-exp 'f) (const-exp 7))))))
```

```
SOURCE:
let f = proc(x)(x - 11) in (f (f 7))

RESULT:
Evaluating let f = proc(x)(x - 11) in (f (f 7))
    env [empty-env]
    store ()
Evaluating proc(x)(x - 11)
    env [empty-env]
    store ()
Evaluating (f (f 7))
    env [f = 0, empty-env]
    store ((0 #(struct:proc-val #(struct:procedure x #(struct:diff-exp #(struct:var-exp x) #(struct:const-exp 11)) #(struct:empty-env)))))
Evaluating f
    env [f = 0, empty-env]
    store ((0 #(struct:proc-val #(struct:procedure x #(struct:diff-exp #(struct:var-exp x) #(struct:const-exp 11)) #(struct:empty-env)))))
Evaluating (f 7)
    env [f = 0, empty-env]
    store ((0 #(struct:proc-val #(struct:procedure x #(struct:diff-exp #(struct:var-exp x) #(struct:const-exp 11)) #(struct:empty-env)))))
Evaluating f
    env [f = 0, empty-env]
    store ((0 #(struct:proc-val #(struct:procedure x #(struct:diff-exp #(struct:var-exp x) #(struct:const-exp 11)) #(struct:empty-env)))))
Evaluating 7
    env [f = 0, empty-env]
    store ((0 #(struct:proc-val #(struct:procedure x #(struct:diff-exp #(struct:var-exp x) #(struct:const-exp 11)) #(struct:empty-env)))))
Evaluating (x - 11)
    env [x = 1, empty-env]
    store ((0 #(struct:proc-val #(struct:procedure x #(struct:diff-exp #(struct:var-exp x) #(struct:const-exp 11)) #(struct:empty-env)))) (1 #(struct:num-val 7)))
Evaluating x
    env [x = 1, empty-env]
    store ((0 #(struct:proc-val #(struct:procedure x #(struct:diff-exp #(struct:var-exp x) #(struct:const-exp 11)) #(struct:empty-env)))) (1 #(struct:num-val 7)))
Evaluating 11
    env [x = 1, empty-env]
    store ((0 #(struct:proc-val #(struct:procedure x #(struct:diff-exp #(struct:var-exp x) #(struct:const-exp 11)) #(struct:empty-env)))) (1 #(struct:num-val 7)))
Evaluating (x - 11)
    env [x = 2, empty-env]
    store ((0 #(struct:proc-val #(struct:procedure x #(struct:diff-exp #(struct:var-exp x) #(struct:const-exp 11)) #(struct:empty-env)))) (1 #(struct:num-val 7)) (2 #(struct:num-val -4)))
Evaluating x
    env [x = 2, empty-env]
    store ((0 #(struct:proc-val #(struct:procedure x #(struct:diff-exp #(struct:var-exp x) #(struct:const-exp 11)) #(struct:empty-env)))) (1 #(struct:num-val 7)) (2 #(struct:num-val -4)))
Evaluating 11
    env [x = 2, empty-env]
    store ((0 #(struct:proc-val #(struct:procedure x #(struct:diff-exp #(struct:var-exp x) #(struct:const-exp 11)) #(struct:empty-env)))) (1 #(struct:num-val 7)) (2 #(struct:num-val -4)))
#(struct:num-val -15)

STORE:
((0 #(struct:proc-val #(struct:procedure x #(struct:diff-exp #(struct:var-exp x) #(struct:const-exp 11)) #(struct:empty-env)))) (1 #(struct:num-val 7)) (2 #(struct:num-val -4)))
```
