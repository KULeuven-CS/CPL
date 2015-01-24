#lang eopl
(require rackunit)
; ex 1.26
(define up
  (lambda (lst)
    (if (null? lst)
        '()
        (if (list? (car lst))
            (append  (car lst) (up (cdr lst)))
            (cons (car lst) (up (cdr lst)))))))

(check-equal? (up '((1 2) (3 4))) '(1 2 3 4))
(check-equal? (up '((x (y)) z)) '(x (y) z))
