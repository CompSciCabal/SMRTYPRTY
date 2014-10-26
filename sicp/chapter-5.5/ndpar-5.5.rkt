#lang racket

;; -------------------------------------------------------
;; Tests and Exercises
;; -------------------------------------------------------

;; Object program in factorial.scm

(display (compile
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n)))
 'val
 'next))

;; Exercise 5.33, p.594
;; Object program in factorial-alt.scm

(display (compile
 '(define (factorial-alt n)
    (if (= n 1)
        1
        (* n (factorial (- n 1)))))
 'val
 'next))
