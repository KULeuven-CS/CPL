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

;;; Exercise 1.16
(define invert
  (lambda (lst)
    (if(null? lst)
       lst
       (cons (list (car (cdr (car lst))) (car (car lst)))(invert (cdr lst))))))

(check-equal? (invert '((a 1) (a 2) (b 1) (b 2)))
              '((1 a) (2 a) (1 b) (2 b)))

;;; Exercise 1.17
(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (cons (car lst) '()) (down (cdr lst))))))

(check-equal? (down '(1 2 3)) '((1) (2) (3)))
(check-equal? (down '((a) (fine) (idea))) '(((a)) ((fine)) ((idea))))

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