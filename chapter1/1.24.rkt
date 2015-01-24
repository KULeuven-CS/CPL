#lang eopl
(require rackunit)
; ex 1.24
(define every?
  (lambda (pred lst)
    (if (null? lst)
        #t
        (if (not (pred (car lst)))
            #f
            (every? pred (cdr lst))))))

(check-false (every? number? '(a b c 3 e)))
(check-true (every? number? '(1 2 3 5 4)))
