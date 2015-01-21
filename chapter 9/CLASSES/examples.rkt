#lang eopl
(require "syntax.rkt")
(require "CLASSES.rkt")

;;; Example programs

; Fig 9.1 page 327
(define p1
  (a-program
   (list
    (a-class-decl 'c1 'object (list 'i 'j)
                  (list
                   (a-method-decl 'initialize '(x) 
                                  (begin-exp
                                    (assign-exp 'i (var-exp 'x))
                                    (list
                                     (assign-exp 'j (diff-exp (const-exp 0) (var-exp 'x))))) )
                  
                  (a-method-decl 'countup '(d) 
                                  (begin-exp
                                    (assign-exp 'i (sum-exp (var-exp 'i) (var-exp 'd)))
                                    (list
                                     (assign-exp 'j (diff-exp (var-exp 'j) (var-exp 'd))))) )
                  (a-method-decl 'getstate '() 
                                  (list-exp
                                    (list (var-exp 'i) (var-exp 'j)
                                     )) )
                  ))
    )
   (let-exp '(t1 t2 o1) 
            (list (const-exp 0) (const-exp 0) (new-object-exp 'c1 (list (const-exp 3))))           
            (begin-exp
     (assign-exp 't1 (method-call-exp (var-exp 'o1) 'getstate '()))
     (list
      (method-call-exp (var-exp 'o1) 'countup (list (const-exp 2)))
      (assign-exp 't2 (method-call-exp (var-exp 'o1) 'getstate '()))
      (list-exp (list (var-exp 't1) (var-exp 't2)))
      )))))

; Fig 9.2, page 328
(define p2
  (a-program
   (list
    (a-class-decl 'interior-node 'object (list 'left 'right)
                  (list
                   (a-method-decl 'initialize '(l r) 
                                  (begin-exp
                                    (assign-exp 'left (var-exp 'l))
                                    (list
                                     (assign-exp 'right (var-exp 'r)))) )
                   (a-method-decl 'sum '() 
                                  (sum-exp (method-call-exp (var-exp 'left) 'sum '()) 
                                           (method-call-exp (var-exp 'right) 'sum '())) )
                   ))
    (a-class-decl 'leaf-node 'object (list 'value)
                  (list
                   (a-method-decl 'initialize '(v) 
                                  (assign-exp 'value (var-exp 'v)) )
                   
                   (a-method-decl 'sum '() 
                                  (var-exp 'value) )
                   ))
    )
   (let-exp '(o1) 
            (list (new-object-exp 'interior-node 
                                  (list (new-object-exp 'interior-node (list (new-object-exp 'leaf-node (list (const-exp 3)))
                                                                             (new-object-exp 'leaf-node (list (const-exp 4)))))
                                        (new-object-exp 'leaf-node (list (const-exp 5))))))           
            (method-call-exp (var-exp 'o1) 'sum (list )))))

; odd-even example from page 329
(define p3
  (a-program
   (list
    (a-class-decl 'oddeven 'object (list)
                  (list
                   (a-method-decl 'initialize '() 
                                  (const-exp 1) )
                   (a-method-decl 'even '(n) 
                                  (if-exp (zero?-exp (var-exp 'n))
                                          (const-exp 1)
                                          (method-call-exp (self-exp) 'odd (list (diff-exp (var-exp 'n) (const-exp 1))))))
                  (a-method-decl 'odd '(n) 
                                  (if-exp (zero?-exp (var-exp 'n))
                                          (const-exp 0)
                                          (method-call-exp (self-exp) 'even (list (diff-exp (var-exp 'n) (const-exp 1))))))
                  ))
    )
   (let-exp '(o1) 
            (list (new-object-exp 'oddeven (list)))           
            (method-call-exp (var-exp 'o1) 'odd (list (const-exp 13))))))

; Fig 9.3, page 330
(define p4
  (a-program
   (list
    (a-class-decl 'point 'object (list 'x 'y)
                  (list
                   (a-method-decl 'initialize '(initx inity) 
                                  (begin-exp
                                    (assign-exp 'x (var-exp 'initx))
                                    (list
                                     (assign-exp 'y (var-exp 'inity)))) )
                  (a-method-decl 'move '(dx dy) 
                                  (begin-exp
                                    (assign-exp 'x (sum-exp (var-exp 'x) (var-exp 'dx)))
                                    (list
                                     (assign-exp 'y (sum-exp (var-exp 'y) (var-exp 'dy))))) )
                  (a-method-decl 'get-location '() 
                                  (list-exp
                                    (list (var-exp 'x) (var-exp 'y)
                                     )) )
                  ))
    (a-class-decl 'colorpoint 'point (list 'color)
                  (list
                   (a-method-decl 'set-color '(c) 
                                  (assign-exp 'color (var-exp 'c)) )
                  (a-method-decl 'get-color '() 
                                  (var-exp 'color) )
                  ))
    )
   (let-exp '(p cp) 
            (list  (new-object-exp 'point (list (const-exp 3) (const-exp 4))) (new-object-exp 'colorpoint (list (const-exp 10) (const-exp 20))))           
            (begin-exp
     (method-call-exp (var-exp 'p) 'move (list (const-exp 3) (const-exp 4)))
     (list
      (method-call-exp (var-exp 'cp) 'set-color (list (const-exp 87)))
      (method-call-exp (var-exp 'cp) 'move (list (const-exp 10) (const-exp 20)))
      (list-exp (list (method-call-exp (var-exp 'p) 'get-location (list)) 
                      (method-call-exp (var-exp 'cp) 'get-location (list))
                      (method-call-exp (var-exp 'cp) 'get-color (list))))
      )))))

; ERROR: Fig 9.3, page 330 but with init of color removed
; Illustrate what happens on accessing an unitialized field
(define p5
  (a-program
   (list
    (a-class-decl 'point 'object (list 'x 'y)
                  (list
                   (a-method-decl 'initialize '(initx inity) 
                                  (begin-exp
                                    (assign-exp 'x (var-exp 'initx))
                                    (list
                                     (assign-exp 'y (var-exp 'inity)))) )
                  (a-method-decl 'move '(dx dy) 
                                  (begin-exp
                                    (assign-exp 'x (sum-exp (var-exp 'x) (var-exp 'dx)))
                                    (list
                                     (assign-exp 'y (sum-exp (var-exp 'y) (var-exp 'dy))))) )
                  (a-method-decl 'get-location '() 
                                  (list-exp
                                    (list (var-exp 'x) (var-exp 'y)
                                     )) )
                  ))
    (a-class-decl 'colorpoint 'point (list 'color)
                  (list
                   (a-method-decl 'set-color '(c) 
                                  (assign-exp 'color (var-exp 'c)) )
                  (a-method-decl 'get-color '() 
                                  (var-exp 'color) )
                  ))
    )
   (let-exp '(p cp) 
            (list  (new-object-exp 'point (list (const-exp 3) (const-exp 4))) (new-object-exp 'colorpoint (list (const-exp 10) (const-exp 20))))           
            (begin-exp
     (method-call-exp (var-exp 'p) 'move (list (const-exp 3) (const-exp 4)))
     (list
      ;(method-call-exp (var-exp 'cp) 'set-color (list (const-exp 87)))
      (method-call-exp (var-exp 'cp) 'move (list (const-exp 10) (const-exp 20)))
      (list-exp (list (method-call-exp (var-exp 'p) 'get-location (list)) 
                      (method-call-exp (var-exp 'cp) 'get-location (list))
                      (method-call-exp (var-exp 'cp) 'get-color (list))))
      )))))

; Fi9 9.4, page 331: field shadowing
(define p6
  (a-program
   (list
    (a-class-decl 'c1 'object (list 'x 'y)
                  (list
                   (a-method-decl 'initialize '(x) 
                                  (const-exp 1) )
                   (a-method-decl 'setx1 '(v) (assign-exp 'x (var-exp 'v)) )
                   (a-method-decl 'sety1 '(v) (assign-exp 'y (var-exp 'v)) )
                   (a-method-decl 'getx1 '() (var-exp 'x) )
                   (a-method-decl 'gety1 '() (var-exp 'y) )
                  ))
    (a-class-decl 'c2 'c1 (list 'y)
                  (list
                   (a-method-decl 'sety2 '(v) (assign-exp 'y (var-exp 'v)) )
                   (a-method-decl 'getx2 '() (var-exp 'x) )
                   (a-method-decl 'gety2 '() (var-exp 'y) )
                   ))
    )
   (let-exp '(o2) 
            (list (new-object-exp 'c2 (list)))           
            (begin-exp
     (method-call-exp (var-exp 'o2) 'setx1 (list (const-exp 101)))
     (list
      (method-call-exp (var-exp 'o2) 'sety1 (list (const-exp 102)))
      (method-call-exp (var-exp 'o2) 'sety2 (list (const-exp 999)))
      (list-exp (list 
                 (method-call-exp (var-exp 'o2) 'getx1 '()) 
                 (method-call-exp (var-exp 'o2) 'gety1 '())
                 (method-call-exp (var-exp 'o2) 'getx2 '())
                 (method-call-exp (var-exp 'o2) 'gety2 '())
                 ))
      )))))

; dynamic dispatch / overriding example page 332
(define p7
  (a-program
   (list
    (a-class-decl 'c1 'object '()
                  (list
                   (a-method-decl 'initialize '() 
                                  (const-exp 1) )
                   (a-method-decl 'm1 '() (const-exp 11) )
                   (a-method-decl 'm2 '() (method-call-exp (self-exp) 'm1 '()) )
                   ))
    (a-class-decl 'c2 'c1 '()
                  (list
                   (a-method-decl 'm1 '() (const-exp 22) )
                   ))
    )
   (let-exp '(o1 o2) 
            (list (new-object-exp 'c1 (list)) (new-object-exp 'c2 (list)))           
            (list-exp (list 
                 (method-call-exp (var-exp 'o1) 'm1 '()) 
                 (method-call-exp (var-exp 'o2) 'm1 '())
                 (method-call-exp (var-exp 'o2) 'm2 '())
                 )))))

; Fig 9.6, page 334
(define p8
  (a-program
   (list
    (a-class-decl 'c1 'object '()
                  (list
                   (a-method-decl 'initialize '() 
                                  (const-exp 1) )
                   (a-method-decl 'm1 '() (method-call-exp (self-exp) 'm2 '()) )
                   (a-method-decl 'm2 '() (const-exp 13) )
                   ))
    (a-class-decl 'c2 'c1 '()
                  (list
                   (a-method-decl 'm1 '() (const-exp 22) )
                   (a-method-decl 'm2 '() (const-exp 23) )
                   (a-method-decl 'm3 '() (super-call-exp 'm1 '()) )
                   ))
    (a-class-decl 'c3 'c2 '()
                  (list
                   (a-method-decl 'm1 '() (const-exp 32) )
                   (a-method-decl 'm2 '() (const-exp 33) )
                                      ))
    )
   (let-exp '(o3) 
            (list (new-object-exp 'c3 (list)))           
            (method-call-exp (var-exp 'o3) 'm3 '()))))

(define (test p)
  (begin
    (display "SOURCE:\n")
    (display (program->string p))
    (display "\n\n")
    (display "RESULT:\n")
    (display (value-of-program p))
    ))



