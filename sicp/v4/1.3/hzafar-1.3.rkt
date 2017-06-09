#lang racket

(require rackunit
         rackunit/log)

;; 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (fact n)
  (define (identity x) x)
  (product-iter identity 1 add1 n))

(check-equal? (fact 4) 24)
(check-equal? (fact 1) 1)

(define (pi-approx n)
  (define (flip n) (if (even? n) n (/ 1.0 n)))
  (* 4 (product flip 2 add1 n)
       (product flip 3 add1 (+ n 1))))

(check-= (pi-approx 100) pi 0.1)
(check-= (pi-approx 1000) pi 0.01)
(check-= (pi-approx 1000000) pi 0.00001)

;; 1.35
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

(define golden-ratio (/ (+ 1 (sqrt 5)) 2))
(check-= (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1) golden-ratio 0.00001)

;; 1.37
(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (sub1 i) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define one (lambda (i) 1.0))
(check-= (cont-frac one one 3) (/ 2.0 3) 0.00000001)
(check-= (cont-frac one one 4) (/ 1 golden-ratio) 0.1)
(check-= (cont-frac one one 8) (/ 1 golden-ratio) 0.001)
(check-= (cont-frac one one 12) (/ 1 golden-ratio) 0.0001)

;; 1.38
(define (e-approx n)
  (+ 2 (cont-frac one
             (lambda (i)
                 (if (= i 1)
                     i
                     (let ([r (remainder i 3)])
                          (cond [(= r 2) (+ (* 2/3 i) 2/3)]
                                [else 1.0]))))
             n)))

(check-= (e-approx 10) (exp 1) 0.0001)

;; 1.39
(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1) x (* -1 x x)))
             (lambda (i) (sub1 (* 2 i)))
             k))

(check-= (tan-cf 30.0 100) (tan 30) 0.0001)

(test-log #:display? #t #:exit? #t)