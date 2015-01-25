#lang eopl
(require rackunit)
; ex 1.23
(define list-index
  (lambda (pred lst)
    (list-index-actual 0 pred lst)))

; actual implementation
(define list-index-actual
  (lambda (index pred lst)
    (if (null? lst)
        #f
        (if (pred (car lst))
            index
            (list-index-actual (+ index 1) pred (cdr lst))))))

(check-equal? (list-index number? '(a 2 (1 3) b 7)) 1)
(check-equal? (list-index symbol? '(a (b c)17 foo)) 0)
(check-false (list-index symbol? '(1 2 (a b) 3)))