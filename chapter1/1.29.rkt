#lang eopl
(require rackunit)
; ex 1.29 - very inefficient sorting

(define smallest-el
  (lambda (smallest lst)
    (if (null? lst)
        smallest
        (if (> smallest (car lst))
            (smallest-el (car lst) (cdr lst))
            (smallest-el smallest (cdr lst))))))

(define smallest
  (lambda (lst)
    (smallest-el (car lst) (cdr lst))))

(define subtract-el
  (lambda (el lst)
    (if (null? lst)
        '()
        (if (equal? (car lst) el) 
            (cdr lst)
            (cons (car lst) (subtract-el el (cdr lst)))))))

(define sort
  (lambda (loi)
    (if (null? loi)
        '()
        (cons (smallest loi) (sort (subtract-el (smallest loi) loi))))))

(check-equal? (sort '(8 2 5 2 3)) '(2 2 3 5 8))
