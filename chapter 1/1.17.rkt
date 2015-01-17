#lang eopl
(require rackunit)

;;; Exercise 1.17
(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (cons (car lst) '()) (down (cdr lst))))))
