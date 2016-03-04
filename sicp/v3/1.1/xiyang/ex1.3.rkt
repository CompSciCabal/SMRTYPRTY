#lang racket

;; Ex 1.3

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (sum-of-squares-two-largest x y z)
  (cond ((and (>= x z) (>= y z)) (sum-of-squares x y))
        ((and (>= x y) (>= z y)) (sum-of-squares x z))
        (else (sum-of-squares y z))))

(sum-of-squares-two-largest 3 2 2)
