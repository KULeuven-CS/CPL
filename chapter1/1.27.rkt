#lang eopl
(require rackunit)
; ex 1.27
(define flatten
  (lambda (slist)
    (if (null? slist)
        '()
        (if (list? (car slist))
            (append (flatten (car slist)) (flatten (cdr slist)))
            (cons (car slist) (flatten (cdr slist)))))))

(check-equal? (flatten '(a b c)) '(a b c))
(check-equal? (flatten '((a) () (b ()) () (c))) '(a b c))
(check-equal? (flatten '((a b) c (((d)) e))) '(a b c d e))
(check-equal? (flatten '(a b (() (c)))) '(a b c))