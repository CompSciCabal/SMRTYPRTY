#lang racket

(define (double a) (* a 2))
(define (halve a) (/ a 2))

(define (fast-* a b)
  (fast-*-iter a b 0))

; a * b = (a * 2) * (b / 2)

(define (fast-*-iter a b s)
  (if (= b 0)
      s
      (fast-*-iter 
       (if (even? b) 
           (double a)
           a)
       (if (even? b)
           (halve b)
           (- b 1))
       (if (even? b)
           s
           (+ s a)))))
