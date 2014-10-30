#lang racket

;; -------------------------------------------------------
;; Compiler Tests and Exercises
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
 '(define (factorial n)
    (if (= n 1)
        1
        (* n (factorial (- n 1)))))
 'val
 'next))

;; Exercise 5.34, p.594
;; Object program in factorial-iter.scm

(display (compile
 '(define (factorial n)
    (define (iter prod count)
      (if (< n count)
          prod
          (iter (* count prod)
                (+ count 1))))
    (iter 1 1))
 'val
 'next))

;; Exercise 5.35, p.595

(display (compile
 '(define (f x)
    (+ x (g (+ x 2))))
 'val
 'next))

;; -------------------------------------------------------
;; Compiler + Evaluator
;; -------------------------------------------------------

(compile
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))