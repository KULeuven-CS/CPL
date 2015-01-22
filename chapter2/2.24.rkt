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

;;; Exercise 2.24
(define bintree-to-list 
  (lambda (tree)
	(cases bintree tree
		  (leaf-node (num) (list 'leaf-node num))
		  (interior-node (key left right) (list 'interior-node key (bintree-to-list left) (bintree-to-list right))))))

;Testing
(define tree
  (interior-node 't (leaf-node 5) (leaf-node -5)))

(check-equal? (bintree-to-list tree)
		 '(interior-node t (leaf-node 5) (leaf-node -5)))
