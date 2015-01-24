#lang eopl
(require rackunit)
; ex 1.21
(define product
  (lambda (sos1 sos2)
    (if (null? sos1)
        '()
        (append (subproduct (car sos1) sos2) (product (cdr sos1) sos2)))))

        
(define subproduct
  (lambda (s sos2)
    (if (null? sos2)
        '()
        (cons (list s (car sos2)) (subproduct s (cdr sos2))))))

(check-equal? (product '(a b c) '(x y)) '((a x) (a y) (b x) (b y) (c x) (c y)))