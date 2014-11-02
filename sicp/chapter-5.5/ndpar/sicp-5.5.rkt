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

;; Exercise 5.36, p.595
;; Order of evaluation

(display (compile
 '(define (f x y) (+ x y))
 'val
 'next))

;; -------------------------------------------------------
;; Compiler + Evaluator
;; -------------------------------------------------------

;; Exercise 5.45.a, p.608
;; Compiler efficiency

(compile
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))

; 1  3  5
; 2  5 11
; 3  8 17
; 4 11 23
; 5 14 29
; 6 17 35

; Recursive factorials
; ┌─────────────────────┬─────────┬──────────┐
; │                     │max depth│ # pushes │
; │                     │ (space) │  (time)  │
; ├─────────────────────┼─────────┼──────────┤
; │interpretor (ex 5.27)│  5n + 3 │ 32n - 16 │
; │compiler    (ex 5.45)│  3n - 1 │  6n -  1 │
; │reg.machine (ex 5.14)│  2n - 2 │  2n -  2 │
; ├─────────────────────┼─────────┼──────────┤
; │compiler/interpretor │     3/5 │     3/16 │
; │machine/interpretor  │     2/5 │     1/16 │
; └─────────────────────┴─────────┴──────────┘

;; Exercise 5.46, p.609
;; Compiler efficiency

(compile
 '(define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2))))))

; 2  5  15  1
; 3  8  25  2
; 4 11  45  3
; 5 14  75  5
; 6 17 125  8
; 7 20 205 13

; Recursive Fibonacci
; ┌─────────────────────┬─────────┬──────────────────┐
; │                     │max depth│     # pushes     │
; │                     │ (space) │      (time)      │
; ├─────────────────────┼─────────┼──────────────────┤
; │interpretor (ex 5.29)│  5n + 3 │ 56 Fib(n+1) - 40 │
; │compiler    (ex 5.46)│  3n - 1 │ 10 Fib(n+1) -  5 │
; │reg.machine  (ex 5.6)│  2n - 2 │                  │
; ├─────────────────────┼─────────┼──────────────────┤
; │compiler/interpretor │     3/5 │             5/28 │
; │machine/interpretor  │     2/5 │                  │
; └─────────────────────┴─────────┴──────────────────┘
