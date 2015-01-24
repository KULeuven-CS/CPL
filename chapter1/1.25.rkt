#lang eopl
(require rackunit)
; ex 1.25
(define exists?
  (lambda (pred lst)
    (if (null? lst)
        #f
        (if (pred (car lst))
            #t
            (exists? pred (cdr lst))))))

(check-true (exists? number? '(a b c 3 e)))
(check-false (exists? number? '(a b c d e)))