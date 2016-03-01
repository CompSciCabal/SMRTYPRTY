;; Ex1.5

#lang racket

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

(define (normal-test x y)
  ((if (= (x) 0)
       (lambda () 0)
       y)))

(test 0 (normal-test (lambda () 0) p))