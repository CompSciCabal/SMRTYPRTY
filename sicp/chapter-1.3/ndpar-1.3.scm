#lang racket
(require (only-in math/number-theory prime?))

; Exercise 1.29, p.60
; Simpson's rule for numerical integration
; N must be even
(define (inc n) (+ n 1))

(define (dec n) (- n 1))

(define (integral f a b n)
  (let ([h (/ (- b a) n)])
    (define (y k)
      (f (+ a (* k h))))
    (define (coef k)
      (cond ((or (= k 0) (= k n)) 1)
            ((odd? k) 4)
            ((even? k) 2)))
    (define (term k)
      (* (coef k) (y k)))
    (* (/ h 3) (sum term 0 inc n))))

; (integral cube 0 1 100)
; (integral cube 0 1 1000)

; (integral cube 0.0 1.0 100)
; (integral cube 0.0 1.0 1000)

; Exercise 1.30, p.60
; Itirative summation
(define (sum term a next b)
  (define (iter i result)
    (if (> i b)
        result
        (iter (next i) (+ (term i) result))))
  (iter a 0))

; Exercise 1.31, p.60
; Recursive product
(define (prod-rec factor a next b)
  (if (> a b)
      1
      (* (factor a)
         (prod-rec factor (next a) next b))))

; Iterative product
(define (product factor a next b)
  (define (iter i result)
    (if (> i b)
        result
        (iter (next i) (* (factor i) result))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (square x) (* x x))

(define (π/4 iter)
  (define (π-factor i)
    (* 1.0 (dec i) (inc i) (/ (square i))))
  (define (π-next i)
    (+ i 2))
  (product π-factor 3 π-next iter))

; Exercise 1.32, p.61
; Recursive accumulator
(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-rec combiner null-value term (next a) next b))))

; Iterative accumulator
(define (accumulate combiner null-value term a next b)
  (define (iter i result)
    (if (> i b)
        result
        (iter (next i) (combiner (term i) result))))
  (iter a null-value))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(define (prod-acc term a next b)
  (accumulate * 1 term a next b))

; Exercise 1.33, p.61
(define (filtered-accumulate combiner null-value pred term a next b)
  (define (iter i result)
    (cond ((> i b) result)
          ((pred i) (iter (next i) (combiner (term i) result)))
          (else (iter (next i) result))))
  (iter a null-value))

(define (square-primes a b)
  (filtered-accumulate + 0 prime? square a inc b))

(define (rel-prime-prod n)
  (define (rel-prime? i)
    (= (gcd i n) 1))
  (filtered-accumulate * 1 rel-prime? identity 1 inc (dec n)))

; Exercise 1.34, p.66
;(define (f g) (g 2))
;(f f)

; p.67
; Finding roots of equations by bisect method
(define (average x y)
  (/ (+ x y) 2))

(define (search f neg-point pos-point)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.001))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (positive? a-value) (negative? b-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

;(half-interval-method sin 2.0 4.0)

; p.69
; Fixed points of functions
(define (fixed-point f first-guess)
  (define (close-enough? x y)
    (< (abs (- x y)) 0.00001))
  (define (try guess)
    (let ((next (f guess)))
      ;      (printf "~a~n" next) ; Exercise 1.36
      (if (close-enough? next guess)
          next
          (try next))))
  (try first-guess))

;(fixed-point cos 1.0)

; Exercise 1.35, p.70
; Calculating Golden Ratio
(define (φ)
  (fixed-point (lambda (x) (+ 1 (/ x))) 1.5))

;(φ)

; Exercise 1.36, p.70
; Solving x^x = 1000
(define (x^x-1)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 1.5))

;(x^x-1) ; 36 steps

(define (x^x-2)
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
               1.5))

;(x^x-2) ; 12 steps

; Exercise 1.37, p.71
; Calculating continued fractions in K iterations
; Iterative process
(define (cont-frac n d k)
  (define (step i value)
    (if (> i k)
        value
        (step (+ 1 i)
              (/ (n i) (+ (d i) value)))))
  (step 1 0.0))

(define (1/φ k)
  (cont-frac (lambda (i) 1)
             (lambda (i) 1)
             k))

;(1/φ 11)

; Recursive process
(define (cont-frac-rec n d k)
  (define (step i)
    (if (> i k)
        0.0
        (/ (n i) (+ (d i) (step (+ i 1))))))
  (step 1))

; Calculating continued fractions with tolerance
(define (cont-frac-2 n d tolerance)
  (define (close-enough? x y)
    (< (abs (- x y)) tolerance))
  (define (step i value)
;    (let ((next (/ (n i) (+ (d i) value)))) ; no average damping
    (let ((next (average value (/ (n i) (+ (d i) value)))))
      (if (close-enough? value next)
          (values next i)
          (step (+ 1 i) next))))
  (step 1 0.0))

; Φ = 1/φ
(define (Φ tolerance)
  (cont-frac-2 (lambda (i) 1)
               (lambda (i) 1)
               tolerance))

;(Φ 0.0001) ;=> 11 without damping; 8 with damping

; Exercise 1.38, p.71
; Euler's expansion for e
(define (e k)
  (define (n i) 1)
  (define (d i)
    (if (= (remainder i 3) 2)
        (* i 2)
        1))
  (+ 2 (cont-frac n d k)))

; http://mathworld.wolfram.com/eContinuedFraction.html

; Doesn't converge well enough
; even with average damping
;(list (e 100) (e 101) (e 102) (e 103))

; Exercise 1.39, p.72
(define (tan-cf x k)
  (define (step i)
    (if (> i k)
        0.0
        (/ (expt x i) (- (* 2 i) 1 (step (+ i 1))))))
  (step 1))

(tan-cf (/ pi 4) 1)