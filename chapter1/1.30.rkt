#lang eopl
(require rackunit)
; ex 1.30 - very inefficient sorting

(define select-with-pred
  (lambda (pred selected lst)
    (if (null? lst)
        selected
        (if (pred (car lst) selected)
            (select-with-pred pred (car lst) (cdr lst))
            (select-with-pred pred selected (cdr lst))))))

(define select
  (lambda ( pred lst)
    (select-with-pred pred (car lst) (cdr lst))))

(define subtract-el
  (lambda (el lst)
    (if (null? lst)
        '()
        (if (equal? (car lst) el) 
            (cdr lst)
            (cons (car lst) (subtract-el el (cdr lst)))))))

(define sort/predicate
  (lambda (pred loi)
    (if (null? loi)
        '()
        (cons (select pred loi) (sort/predicate pred (subtract-el (select pred loi) loi))))))

(check-equal? (sort/predicate < '(8 2 5 2 3)) '(2 2 3 5 8))
(check-equal? (sort/predicate > '(8 2 5 2 3)) '(8 5 3 2 2))
