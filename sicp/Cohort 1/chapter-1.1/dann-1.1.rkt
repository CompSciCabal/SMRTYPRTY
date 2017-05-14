#lang racket

;;; SICP CHAPTER 1.1

(define (+rec x y) 
    (if (= x 0)
        y
        (+ 1 (+rec (- x 1) y))))

(define (+iter x y)
  (if (= x 0)
      y
      (+iter (- x 1) (+ y 1))))

(define (fib-rec n) 
  (if (< n 2)
      1
      (+ (fib-rec (- n 1)) (fib-rec (- n 2)))))
  
(define (fib n)
  (fib-iter 1 1 n))

(define (fib-iter a b count)
  (if (= count 0)
      a
      (fib-iter (+ a b) a (- count 1)))) 

;;; SICP 1.3
(define (sum-of-squares x y)
  (+ (* x x) (* y y)))

(define (sumsqlarge x y z) 
    (cond ((>= x y z) (sum-of-squares x y))
          ((>= y z x) (sum-of-squares y z))
          ((>= z x y) (sum-of-squares z x))
          ((>= z y x) (sum-of-squares z y))
          ((>= y x z) (sum-of-squares y x))
          ((>= x z y) (sum-of-squares x z))))

(define (sum-largest-squares x y z)
  (let ([nums (sort (list x y z) >)])
    (+ (car nums) (car (cdr nums)))))

;;;; SICP 1.7

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

(define (improve guess x)
    (average guess (/ x guess)))

(define (average x y)
    (/ (+ x y) 2))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

(define (sqrt1 x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter2 guess x lastguess)
    (if (good-enough2? guess x lastguess)
        guess
        (sqrt-iter2 (improve2 guess x) x guess)))

(define (good-enough2? guess x lastguess)
  (< (abs (- (/ lastguess guess) 1)) 0.00001))

(define (improve2 guess x)
  (average guess (/ x guess)))

(define (sqrt2 x)
  (sqrt-iter2 1.0 x 0))

;;; much better for small, ~same for large

;;; SICP 1.8

(define (3root-iter guess x)
    (if (3root-good-enough? guess x)
        guess
        (3root-iter (3root-improve guess x) x)))

(define (3root-good-enough? guess x)
    (< (abs (- (cube guess) x)) 0.001))

(define (3root-improve guess x)
    (/ (+ (/ x (* guess guess)) (* guess 2)) 3))

(define (cube x)
    (* x x x))

(define (3root x)
    (3root-iter 1.0 x))
