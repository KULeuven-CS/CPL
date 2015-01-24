#lang eopl
(require rackunit)
; ex 1.22
(define filter-in
  (lambda (pred lst)
    (if (null? lst)
        '()
        (if (pred (car lst))
            (cons (car lst) (filter-in pred (cdr lst)))
            (filter-in pred (cdr lst))))))

(check-equal? (filter-in number? '(a 2 (1 3) b 7)) '(2 7))
(check-equal? (filter-in symbol? '(a (b c) 17 foo)) '(a foo))