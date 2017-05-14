#lang racket

(require rackunit
         rackunit/log)

;; 1.3
(define (square x) (* x x))
(define (sum-of-squares-largest x y z)
  (cond [(and (> x y) (> x z)) (+ (square x) (square (if (> y z) y z)))]
        [(> x y) (+ (square x) (square z))]
        [(> x z) (+ (square x) (square y))]
        [else (+ (square y) (square z))]))

(check-equal? (sum-of-squares-largest 1 2 3) 13)
(check-equal? (sum-of-squares-largest 3 1 2) 13)
(check-equal? (sum-of-squares-largest 2 3 1) 13)

;; 1.4
;; If b is negative, we get (- a b), i.e. a + (- b).
;; If b is positive, we get (+ a b).

;; 1.5
;; With applicative order, the interpreter will spin in an infinite
;; loop as it tries to recursively expand (p). With normal order, (p)
;; will be passed unevaluated into the body of the if, where it will
;; be ignored since the true branch will be taken.

;; 1.6
;; Both branches are always evaluated, so it will never terminate.

;; 1.7
(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? old-guess guess)
  (< (abs (- old-guess guess)) 0.001))

(define (sqrt-iter2 old-guess guess x)
  (if (good-enough? old-guess guess)
      guess
      (sqrt-iter2 guess
                  (improve guess x)
                  x)))

(define (sqrt x)
  (sqrt-iter2 x 1.0 x))

(check-= (sqrt 9) 3 0.0001)
(check-= (sqrt 2) 1.4142 0.0001)
(check-= (sqrt 1000000) 1000 0.0001)
(check-= (sqrt 0.25) 0.5 0.0001)

;; 1.8

(define (cube-improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess))
     3))
 
(define (cube-iter old-guess guess x)
  (if (good-enough? old-guess guess)
      guess
      (cube-iter guess
                 (cube-improve guess x)
                 x)))

(define (cube-root x)
  (cube-iter x 1.0 x))

(check-= (cube-root 27) 3 0.0001)
(check-= (cube-root 0.008) 0.2 0.0001)
(check-= (cube-root 10) 2.1544 0.0001)
(check-= (cube-root 1000000) 100 0.0001)

(test-log #:display? #t #:exit? #t)