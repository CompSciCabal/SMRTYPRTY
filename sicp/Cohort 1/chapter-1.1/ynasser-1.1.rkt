#lang racket

;; Exercise 1.1
;; 10
;; 12
;; 8
;; 3
;; 6
;; nothing is printed
;; nothing is printed
;; 19
;; #f
;; 4
;; 16
;; 6
;; 16

;; Exercise 1.2
;; It's hard to read the equation which is printed in my copy of SICP. 
;; (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;; Exericse 1.3
(define (max-sum a b c)
	(+ (sqr (max a b))
	   (sqr (max c (min a b)))))

;; Exercise 1.4
;; "Observe that our model of evaluation allows for combinations whose
;; operators are compound expressions. Use this observation to describe
;; the behavior of the following procedure:"

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; The inside is evaluated first (resulting in either + or -), then
;; either (- a b) or (+ a b) will be evaluated.

;; Exercise 1.5
;; This is my favorite exercise in SICP.
(define (p) (p))
(define (test x y)
  (if (= x 0)
    0
    y))

;; (test 0 (p))

;; Applicative-order evaluation behaviour: will evaluate (= x 0) first
;; and will return 0 and never reach (p)
;; Normal-order evaluation behaviour: infinite loop

;; Exercise 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(define (average a b)
  (/ (+ a b) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (sqr guess) x)) 0.001))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x)
		     x)))

;; What happens when Alyssa uses the new-if to computer square roots?
;; infinite loop, because even when (good-enough? guess x) evaluates to
;; #t, new-if would try to evaluate the else-clause, which would recurse
;; deeper ... 

;; Exercise 1.7
;; How if fails for small numbers: since we are only checking precision to
;; 0.001, if the number is smaller than that, then the results of good-enough?
;; will not be good enough:
;; (good-enough? 0.0000000003 .0002) => #t
;; 
;; How it fails for large numbers: I'm not sure? Is there a precision problem
;; somewhere?

(define (new-good-enough? guess x)
  (define new (improve guess z)
    (if (< (/ (- (max new guess) (min new guess)) (max new guess)) 0.0000001)
      #t
      #f)))

;; This works better for small numbers less than zero because we ignore 
;; everything after the decimal number we are looking for.

;; Exercise 1.8
(define (better x y)
  (/ (+ (/ x (sqr y)) (* 2 y)) 3))

(define (good-enough-cube? guess x)
  (< (abs (- (* guess (sqr guess)) x)) 0.0001))

(define (cube-root x guess)
  (if (good-enough-cube? guess x)
    (floor guess)
    (cube-root x (better x guess))))

;(cube-root 27 2)
;(cube-root 1000 6)
