#lang racket
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

