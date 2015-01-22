#lang eopl
(require rackunit)

;;; Exercise 1.16
(define invert
  (lambda (lst)
    (if(null? lst)
       lst
       (cons (list (car (cdr (car lst))) (car (car lst)))(invert (cdr lst))))))
