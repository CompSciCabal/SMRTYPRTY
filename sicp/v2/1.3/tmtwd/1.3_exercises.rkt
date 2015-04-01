#lang racket

;1.31, how would I return 0 if the init value of a is greater 
;than the init value of b?
(define (product f a b)
  (if (> a b)
      1
      (* (f a)
         (product f
                  ((lambda(x) (+ x 1)) a)
                  b))))
; factorial function using product function
(define (fact n)
  (product (lambda(x) x) 1 n))
