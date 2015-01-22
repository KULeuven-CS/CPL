#lang eopl
(require rackunit)

;;; Exercise 1.15
(define duple
  (lambda (n x)
    (if(zero? n)
       '()
       (cons x (duple (- n 1) x)))))

(check-equal? (duple 2 3) '(3 3))
(check-equal? (duple 2 '(ha ha)) '((ha ha) (ha ha)))
(check-equal? (duple 0 '(haha)) '())
