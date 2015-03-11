#lang racket
;; From SICP
(define (average x y)
  (/ (+ x y) 2))

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

(displayln "exercise 1.35")
(fixed-point (lambda (x) (+ 1 (/ 1 x)))
             1.0)

(displayln "exercise 1.36")
(define (logged-fixed-point f first-guess)
  (define (logger x)
      (displayln x)
      (f x))
  (fixed-point logger first-guess))

;; 35 Steps to Converge
(logged-fixed-point (lambda (x)
                      (/ (log 1000)
                         (log x)))
                    2.0)

(define (dampened-logged-fixed-point f first-guess)
  (define (average x)
    (let [(y (f x))]
      (/ (+ y (/ x y)) 2)))
  (logged-fixed-point average first-guess))

;; 10 Steps to Converge
(logged-fixed-point (lambda (x)
                      (let [(fx (/ (log 1000)
                                   (log x)))]
                        (average x fx)))
                    2.0)

(displayln "exercise 1.37 a.")
(define (cont-frac n d k)
           (define (cont-frac-iter n d k i)
             (let [(ni (n i))
                   (di (d i))]
             (if (= k i)
                 (/ ni di)
                 (/ ni (+ di (cont-frac-iter n d k (+ i 1)))))))
           (cont-frac-iter n d k 1))

;; 1 / goldenRatio == 1.6180339887498948482
;; The procedure will need to iterate 11 times to get 4 decimal
;; places of accuracy
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)

(displayln "exercise 1.37 b.")
;; Start from base case nk / dk and
;; work way up
(define (tr-cont-frac n d k)
  (define (cont-frac-iter k acc)
    (if (= k 0)
        acc
        (cont-frac-iter (- k 1)
                        (/ (n k) (+ (d k) acc)))))
  (cont-frac-iter k 0))
(tr-cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)

(displayln "exercise 1.38")
(define (approx-e steps)
  (+ 2.0 (tr-cont-frac (lambda (x) 1.0)
                (lambda (x)
                  (if (= (remainder x 3) 2)
                      (/ (+ x 1) 1.5)
                      1))
                steps)))
(approx-e 10)

(displayln "exercise 1.39")
(define (tan-cf x k)
  (tr-cont-frac (lambda (i)
                  (if (= 1 i)
                      x
                      (- (* x x))))
                (lambda (i) (- (* i 2) 1))
                k))

(tan-cf (/ pi 4) 10)
(tan-cf (/ pi 3) 10)