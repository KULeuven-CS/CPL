#lang eopl
(require racket/base)
(require rackunit)
(require slideshow)

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

;; Exercise 2.25
(define max-interior 
  (lambda (tree)
	(cdr max-interior2 tree)))

(define max-interior2
  (lambda (tree)
	(cases bintree tree
		   (leaf-node (num) (list 'null num))
		   (interior-node (key left right) 
						  (biggest (max-interior left) (max-interior right))))))
