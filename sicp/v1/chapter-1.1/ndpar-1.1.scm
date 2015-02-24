#lang racket

; p.12
(define (square x)
  (* x x))

; p.13
(define (sum-of-squares x y)
  (+ (square x) (square y)))

; Exercise 1.2, p.21
;(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
;   (* 3 (- 6 2) (- 2 7)))

; Exercise 1.3, p.21
(define (sicp-1-3 x y z)
  (cond ((and (< x y) (< x z)) (sum-of-squares y z))
        ((and (< y z) (< y x)) (sum-of-squares x z))
        ((and (< z x) (< z y)) (sum-of-squares x y))))

; p.23
(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (my-sqrt x)
  (sqrt-iter 1.0 x))

; Exercise 1.7, p.25
;(sqrt (square 0.01))
;(my-sqrt (square 0.01))
;(my-sqrt (square (+ 1e+30 1e+6)))

(define (good-enough-2? prev guess)
  (< (abs (- prev guess)) 0.001))

(define (sqrt-iter-2 prev guess x)
  (if (good-enough-2? prev guess)
      guess
      (sqrt-iter-2 guess (improve guess x)
                   x)))

(define (my-sqrt-2 x)
  (sqrt-iter-2 0.0 1.0 x))

;(my-sqrt-2 (square 0.01)) ; works now for small numbers
;(my-sqrt-2 (square (+ 1e+30 1e+6))) ; still not working for big numbers

; Exercise 1.8, p.26
; Cube root
(define (cbrt x)
  (define (good-enough? prev guess)
    (< (abs (- prev guess)) 0.001))
  (define (improve guess)
    (/ (+ (* 2 guess) (/ x guess guess)) 3))
  (define (cbrt-iter prev guess)
    (if (good-enough? prev guess)
        guess
        (cbrt-iter guess (improve guess))))
  (cbrt-iter 0.0 1.0))
