#lang eopl
(require rackunit)

;;; ex 1.19
(define list-set
  (lambda (lst n x)
    (if (zero? n)
        (cons x (cdr lst))
        (cons (car lst) (list-set (cdr lst) (- n 1) x)))))

(check-equal? (list-set '(a b c d) 2 '(1 2)) '(a b (1 2) d))
(check-equal? (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3) '(1 5 10))