#lang eopl
(require rackunit)

;;; Exercise 1.18
(define swapper
  (lambda (s1 s2 slist)
    (if(null? slist)
       '()
       (let ((s (car slist)))
         (let ((l (cdr slist)))
           (if (symbol? s)
               (cond
                 ((equal? s1 s) (cons s2 (swapper s1 s2 l)))
                 ((equal? s2 s) (cons s1 (swapper s1 s2 l)))
                 (else (cons s (swapper s1 s2 l)))
               )
               (cons (swapper s1 s2 s) (swapper s1 s2 l))))))))

(check-equal? (swapper 'a 'd '(a b c d)) 
              '(d b c a))
(check-equal? (swapper 'a 'd '(a b () c d)) 
              '(d b () c a))
(check-equal? (swapper 'x 'y '((x) y (z (x))))
              '((y) x (z (y))))
