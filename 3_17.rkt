#lang racket
#|
Exercise 3.17
-------------
Devise a correct version of the count-pairs procedure of exercise 3.16 
that returns the number of distinct pairs in any structure. 
(Hint: Traverse the structure, maintaining an auxiliary data structure 
that is used to keep track of which pairs have already been counted.)

Notes
-----
Simple enough, they tell you what to do.
|#

(define (join a-cons a-list) ; append a cons onto a list.
  (append a-list (list a-cons)))
(define (in? element list) ; boolean, element is in list
  (not (eq? (memq element list) false)))

(define (count-pairs graph)
  (let ([visited-nodes '()])
    (define (inner node)
      (cond ((not (pair? node)) 0) 
            ((in? node visited-nodes) 0)
            (else (begin 
                    (set! visited-nodes (join node visited-nodes))
                    (+ (inner (car node)) (inner (cdr node)) 1)))
            ))
    (+ (inner (car graph)) (inner (cdr graph)) 1)
    ))  

(displayln "Four branches:")
(define c (cons 3 3))
(define b (cons 2 c))
(define a (cons b c))
(count-pairs a)
(displayln "Seven branches: ")
(define z (cons 3 3))
(define y (cons z z))
(define x (cons y y))
(count-pairs x)
(displayln "Three branches: ")
(define v (cons 3 3))
(define u (cons 2 v))
(define t (cons 1 u))
(count-pairs t) 