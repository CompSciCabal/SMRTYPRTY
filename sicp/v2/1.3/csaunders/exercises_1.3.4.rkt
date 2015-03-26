#lang racket
;; From Section 1.3.3
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let [(next (f guess))]
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; From SICP
(define (average x y) (/ (+ x y) 2))
(define (average-damp fn)
  (lambda (x)
    (average x (fn x))))

(define (inc x) (+ x 1))
(define (cube x) (* x x x))
(define (square x) (* x x))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(displayln "exercise 1.40")
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(newtons-method (cubic 1 1 1) 1)
(newtons-method (cubic 3 2 1) 1)

(displayln "exercise 1.41")
(define (double fn)
  (lambda (x)
    (fn (fn x))))

((double inc) 1)
(((double (double double)) inc) 5)
;; The above expands out into a number of doubles
;;   (double (double (double (double inc))))
;; Then do the expansions
;;   (double (double (double (lambda (x) (inc (inc x))))))
;;   (double (double (inc (inc (inc (inc x))))))
;;   (double (inc (inc (inc (inc (inc (inc (inc (inc x)))))))))
;; We end up applying the function 2^4 times which results in the value of 21 (16 + 5)

(displayln "exercise 1.42")
(define (compose f g)
  (lambda (x)
    (f (g x))))

((compose square inc) 6)

(displayln "exercise 1.43")
(define (repeated fn n)
  (define (repeated-iter fn n composed)
    (if (= 1 n)
        composed
        (repeated-iter fn (- n 1) (compose fn composed))))
  (repeated-iter fn n fn))

((repeated square 2) 5)

(displayln "exercise 1.44")
(define (smooth fn)
  (let [(dx 0.00001)]
    (lambda (x)
      (/ (+ (fn (- x dx))
            (fn x)
            (fn (+ x dx)))
         3))))

(displayln "smoothing of cubic fn")
(newtons-method (smooth (cubic 1 1 1)) 1)
(newtons-method (smooth (cubic 3 2 1)) 1)

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

(displayln "n-fold smoothing of cubic fn")
(newtons-method (n-fold-smooth (cubic 1 1 1) 5) 1)
(newtons-method (n-fold-smooth (cubic 3 2 1) 5) 1)

(displayln "exercise 1.45")
(displayln "̄\\(°_o)/ ̄")

(displayln "exercise 1.46")
(define (iterative-improve good-enough? improve)
  (define (iterative-improve-iter guess)
    (if (good-enough? guess)
        guess
        (iterative-improve-iter (improve guess))))
  (lambda (guess)
    (iterative-improve-iter guess)))

(displayln "iterative-improve impl of sqrt")
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve-guess guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve-guess) 1.0))
(sqrt 2.0)

(define (fixd-point f)
  (define tolerance 0.00001)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve good-enough? f) 1.0))

(fixed-point (lambda (x) (+ 1 (/ 1 x)))
             1.0)
(fixd-point (lambda (x) (+ 1 (/ 1 x))))

