#lang racket
#| 
https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-22.html
|#
(displayln "Exercise 3.16")
(displayln "All the terminating options can be done with normal cons. 
I believe you need mcons for the non-terminating one to construct a cycle.")
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))
(displayln "Count of four:")
(define c (cons 3 3))
(define b (cons 2 c))
(define a (cons b c))
(count-pairs a)
(displayln "Count of seven: ")
(define z (cons 3 3))
(define y (cons z z))
(define x (cons y y))
(count-pairs x)
(displayln "Three: ")
(define v (cons 3 3))
(define u (cons 2 v))
(define t (cons 1 u))
(count-pairs t)
