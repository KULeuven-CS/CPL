#lang eopl
(require rackunit)

;;; Exercise 1.20
(define count-occurences 
  (lambda (s slist)
    (if(null? slist)
       0
       (let ((h (car slist)))
         (let ((t (cdr slist)))
           (if (symbol? h)
             (if (equal? s h)
               (+ 1 (count-occurences s t))
               (count-occurences s t)
             )
             (+ (count-occurences s h) (count-occurences s t))))))))

(check-equal? (count-occurences 'x '((f x ) y (((x z) x)))) 3)
(check-equal? (count-occurences 'x '((f x ) y (((x z) () x)))) 3)
(check-equal? (count-occurences 'w '((f x ) y (((x z) x)))) 0)  
