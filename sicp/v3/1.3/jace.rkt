#lang racket


;; Helper functions
(define (inc n) (+ n 1))
(define (cube x) (* x x x))

;; sum-recursive
(define (sum-recurse term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum-recurse term (next a) next b))))



;; Ex 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
    (iter a 0))

;; Ex 1.31
; defining product
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
    (iter a 1))

;; defining PI
(define (pi n)
   (define (term x)
      (/ (* 4 (square x))
         (- (* 4 (square x)) 1)))
   (* 2 (product term 1 inc n)))


;; recursive definition
(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursive (next a) next b))))

;; Ex. 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(define (product-acc term a next b)
  (accumulate * 1 term a next b))



;; Ex. 1.34
;Suppose we have procedure f as:
;
;(define (f g) (g 2))
;
;and we were to evaluate 
;> (f f)
;
;Well, let's evaluate it in applicative order...
;
;(define f (lambda (x) (x 2)))
;
;> (f f)
;> (f (lambda (x) (x 2)) )
;> (lambda (x) (lambda (x) (x 2))
;              2)
;
;Applying 2 into x to evaluate... And in a perverse exchange, yields...
;> (lambda (2) (2 2))
;Which should cause a type error because 2 is a primitive and does not describe a procedure


;; Skipping 1.35 - 1.39. Too math for me at this point.


;; Helper function for Ex. 1.41 - 1.43
(define (square x)
  (* x x))

;; Ex. 1.41
(define (double proc)
  (lambda (x) (proc (proc x))))

; (((double (double double)) add1) 5)
; > 21
; (double double) - apply double twice - 4 times
; (double (double double) - apply (double double) itself - 4^2 = 16

;; Ex 1.42
(define (compose f g)
  (lambda (x) (f (g x))))


;; Ex 1.43
;; can i do a tail recursion on this?
(define (repeated f n)
  (cond
    [(= n 1) f]
    [else (compose f (repeated f (- n 1)))]))


