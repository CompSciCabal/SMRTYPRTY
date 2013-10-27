#lang racket
(require (only-in math/number-theory prime?))

(define (inc n) (+ n 1))

(define (dec n) (- n 1))

(define (cube x) (* x x x))

(define (average x y)
  (/ (+ x y) 2))

; Exercise 1.29, p.60
; Simpson's rule for numerical integration
; N must be even
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
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (good-enough? neg-point pos-point)
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
(define (good-enough? x y)
  (< (abs (- x y)) 0.00001))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
;      (printf "~a~n" next) ; Exercise 1.36
      (if (good-enough? next guess)
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
  (define (step i acc)
    (if (= i 0)
        acc
        (step (dec i)
              (/ (n i) (+ (d i) acc)))))
  (step k 0.0))

(define (one _) 1)

(define (1/φ k)
  (cont-frac one one k))

;(1/φ 11)

; Recursive process
(define (cont-frac-rec n d k)
  (if (= k 0)
      0
      (/ (n k) (+ (d k) (cont-frac-rec n d (dec k))))))

; Calculating continued fractions with tolerance
(define (cont-frac-2 n d tolerance)
  (define (close-enough? x y)
    (< (abs (- x y)) tolerance))
  (define (step i value)
;    (let ((next (/ (n i) (+ (d i) value)))) ; no average damping
    (let ((next (average value (/ (n i) (+ (d i) value)))))
      (if (close-enough? value next)
          (values next i)
          (step (inc i) next))))
  (step 1 0.0))

; Φ = 1/φ
(define (Φ tolerance)
  (cont-frac-2 one one tolerance))

;(Φ 0.0001) ;=> 11 without damping; 8 with damping

; Exercise 1.38, p.71
; Euler's expansion for e
; http://mathworld.wolfram.com/eContinuedFraction.html
(define (e k)
  (define (n i) 1)
  (define (d i)
    (if (= 2 (remainder i 3))
        (* 2 (+ 1 (quotient i 3)))
        1))
  (+ 2 (cont-frac n d k)))

;(list (e 10) (e 15) (e 20))

; N.B. If you implement E function using
; incrementing index (cont-frac-2),
; it does not converge. Same with tangent.
; Φ, on the other hand, does converge.

; Exercise 1.39, p.72
(define (tan-cf x k)
  (define (n i)
    (if (= i 1) x (- (square x))))
  (define (d i)
    (- (* i 2) 1))
  (cont-frac n d k))

;(tan (/ pi 4))
;(tan-cf (/ pi 4) 9)

; p.72
(define ((average-damp f) x)
  (average x (f x)))

; p.74
(define dx 0.000001)

(define ((deriv g) x)
  (/ (- (g (+ x dx)) (g x)) dx))

(define ((newton-transform g) x)
  (- x (/ (g x) ((deriv g) x))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; Exercise 1.40, p.77
(define ((cubic a b c) x)
  (+ (cube x) (* a (square x)) (* b x) c))

;(newtons-method (cubic 1 1 -14) 1)

; Exercise 1.41, p.77
(define ((double f) x)
  (f (f x)))

;((double inc) 0)
;(((double (double double)) inc) 5) ;=> +16 = 21

; Exercise 1.42, p.77
(define ((compose f g) x)
  (f (g x)))

;((compose square inc) 6)

; Exercies 1.43, p.77
(define ((repeated-2 f n) x)
  (if (= n 1)
      (f x)
      ((repeated-2 f (dec n)) (f x))))

;((repeated-2 square 2) 5)

(define (repeated f n)
  (cond ((= n 1) f)
        ((even? n) (repeated (compose f f) (/ n 2)))
        ((odd? n) (compose f (repeated f (- n 1))))))

;((repeated square 2) 5)

; Exercise 1.44, p.78
(define ((smooth f) x)
  (/ (+ (f (- x dx))
        (f x)
        (f (+ x dx)))
     3))

(define (n-smooth f n)
  (repeated smooth n))

; Exercise 1.45, p.78
(define (nth-root n x damps)
  (fixed-point ((repeated average-damp damps)
                (lambda (y) (/ x (expt y (dec n)))))
               1.0))

; Square root
(define (2-root x)
  (nth-root 2 x 1))

;(2-root 4)

; Cube root
(define (3-root x)
  (nth-root 3 x 1))

;(3-root 8)

; Fourth root
(define (4-root x)
  (nth-root 4 x 2))

;(4-root 16)

(define (5-root x)
  (nth-root 5 x 2))

;(5-root 32)

(define (6-root x)
  (nth-root 6 x 1))

;(6-root 64)

(define (7-root x)
  (nth-root 7 x 1))

;(7-root 128)

(define (8-root x)
  (nth-root 8 x 1))

;(8-root 256)

(define (20-root x)
  (nth-root 20 x 1))

;(20-root 1048576)

; Exercise 1.46, p.78
(define ((iterative-improve good-enough? improve) guess)
  (define (try prev)
    (let ((next (improve prev)))
      (if (good-enough? next prev)
          next
          (try next))))
  (try guess))

(define (fixed-point-2 f guess)
  ((iterative-improve good-enough? f) guess))

;(fixed-point cos 1.0)
;(fixed-point-2 cos 1.0)

(define (sqrt-2 x)
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

;(sqrt-2 16)