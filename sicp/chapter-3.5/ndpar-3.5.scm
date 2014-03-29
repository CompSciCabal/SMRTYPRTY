#lang planet neil/sicp

(define (square x) (* x x))

(define (average a b)
  (/ (+ a b) 2))

(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (display-line x)
  x)

;; -------------------------------------------------------
;; Streams, p.316
;; -------------------------------------------------------

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (stream-first s n)
  (define (iter i)
    (if (= i n)
        (newline)
        (begin (display-line (stream-ref s i))
               (iter (+ i 1)))))
  (iter 0))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))))

;; Exercise 3.50, p.324

(define (stream-map proc . streams)
  (if (stream-null? (car streams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car streams))
                   (apply stream-map (cons proc (map stream-cdr streams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

;; Exercise 3.51, p.325
;; Memoization is implemented indeed!

;(define x (stream-map show (stream-enumerate-interval 0 100)))

;; -------------------------------------------------------
;; Infinite Streams, p.326
;; -------------------------------------------------------

(define (scale-stream s factor)
  (stream-map (lambda (x) (* x factor)) s))

;; Explicit definition of integers

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

;; Implicit definition of integers

(define ones (cons-stream 1 ones))

(define integers2 (cons-stream 1 (add-streams ones integers2)))

;; Fibonacci stream

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

;; Exercise 3.53, p.330

(define powers2 (cons-stream 1 (add-streams powers2 powers2)))

;; Exercise 3.54, p.331

(define factorials (cons-stream 1 (mul-streams integers factorials)))

;; Exercise 3.55, p.331

(define (partial-sums s)
  (define (iter s)
    (cons-stream 0 (add-streams s (iter s))))
  (stream-cdr (iter s)))

;(stream-first (partial-sums integers) 5)

;; Exercise 3.56, p.331

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))))))))

(define s (cons-stream 1 (merge (scale-stream s 2)
                                (merge (scale-stream s 3)
                                       (scale-stream s 5)))))

;(stream-first s 15)

;; Exercise 3.58, p.332

(define (expand num den radix)
  (cons-stream (quotient (* num radix) den)
               (expand (remainder (* num radix) den)
                       den radix)))

;(/ 1.0 7) ;= 0.14285714285714285
;(stream-first (expand 1 7 10) 5) ;= 1 4 2 8 5
;(stream-first (expand 1 7 8) 5) ;= 1 1 1 1 1
;(/ 3.0 8) ;= 0.375
;(stream-first (expand 3 8 10) 5) ;= 3 7 5 0 0

;; Exercise 3.59, p.332

(define (integrate-series s)
  (mul-streams s (stream-map / integers)))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

;(stream-first exp-series 5)

(define cos-series
  (cons-stream 1 (scale-stream (integrate-series sin-series) -1)))

(define sin-series
  (cons-stream 0 (integrate-series cos-series)))

;(stream-first cos-series 5)
;(stream-first sin-series 6)

;; Exercise 3.60, p.333

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series s2 (stream-cdr s1)))))

; 1 0 -1 0 1/3 0 -2/45 0 1/315
;(stream-first (mul-series cos-series cos-series) 7)
; 0 0 1 0 -1/3 0 2/45 0 -1/315
;(stream-first (mul-series sin-series sin-series) 7)

(define one (add-streams (mul-series sin-series sin-series)
                         (mul-series cos-series cos-series)))

;(stream-first one 5)

;; Exercise 3.61, p.333

(define (invert-unit-series s)
  (cons-stream 1 (scale-stream (mul-series (stream-cdr s)
                                           (invert-unit-series s))
                               -1)))

;; Exercise 3.62, p.334

(define (div-series n d)
  (if (= 0 (stream-car d))
      (error "Division by zero")
      (mul-series n (invert-unit-series (scale-stream d (/ (stream-car d)))))))

(define tan-series
  (div-series sin-series cos-series))

; 0 1 0 1/3 0 2/15 0 17/315
;(stream-first tan-series 9)

;; -------------------------------------------------------
;; Iterations as Streams, p.334
;; -------------------------------------------------------

;; Square root stream

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0 (stream-map (lambda (guess)
                                   (sqrt-improve guess x))
                                 guesses)))
  guesses)

;(display-stream (sqrt-stream 2))

;; Approximation of π

(define (π-summands n)
  (cons-stream (/ 1.0 n) (stream-map - (π-summands (+ n 2)))))

(define π-stream
  (scale-stream (partial-sums (π-summands 1)) 4))

;(display-stream π-stream)

;; Sequence accelerator for alternating partial sums

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

;(display-stream (euler-transform π-stream))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform
                               (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

;(stream-first (accelerated-sequence euler-transform π-stream) 8)

;; Exercise 3.64, p.338

(define (stream-limit s tolerance)
  (define (iter s item)
    (let ((head (stream-car s)))
      (if (< (abs (- head item)) tolerance)
          head
          (iter (stream-cdr s) head))))
  (iter (stream-cdr s) (stream-car s)))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

;(sqrt 2 1e-6)

;; Exercise 3.65, p.338

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

;(display-stream ln2-stream)
;(display-stream (euler-transform ln2-stream))
;(stream-first (accelerated-sequence euler-transform ln2-stream) 10)

;; -------------------------------------------------------
;; Infinite Pairs, p.338
;; -------------------------------------------------------

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define int-pairs (pairs integers integers))

;; Exercise 3.66, p.341

;;    NB. J program to compute index of pair (m,n)
;;    N=. (2 ^ <:@{.) + (2 ^ {.) * {: - {. + 1:
;;    D=. [: <: [: +/ 2 ^ i.
;;    index=. D@{. + N`0:@.(=/)
;;    index 1 100
;; 197
;;    index 99 100
;; 9.5073795e29
;;    index 100 100
;; 1.2676506e30

(equal? '(1 1) (stream-ref int-pairs 0))
(equal? '(1 2) (stream-ref int-pairs 1))
(equal? '(1 3) (stream-ref int-pairs 3))
(equal? '(1 4) (stream-ref int-pairs 5))
(equal? '(1 100) (stream-ref int-pairs 197))

(equal? '(2 2) (stream-ref int-pairs 2))
(equal? '(2 3) (stream-ref int-pairs 4))
(equal? '(2 4) (stream-ref int-pairs 8))
(equal? '(2 5) (stream-ref int-pairs 12))

(equal? '(3 3) (stream-ref int-pairs 6))
(equal? '(3 4) (stream-ref int-pairs 10))
(equal? '(3 5) (stream-ref int-pairs 18))
(equal? '(3 6) (stream-ref int-pairs 26))

(equal? '(4 4) (stream-ref int-pairs 14))
(equal? '(4 5) (stream-ref int-pairs 22))
(equal? '(4 6) (stream-ref int-pairs 38))
(equal? '(4 7) (stream-ref int-pairs 54))

(equal? '(5 5) (stream-ref int-pairs 30))
(equal? '(5 6) (stream-ref int-pairs 46))
(equal? '(5 7) (stream-ref int-pairs 78))
(equal? '(5 8) (stream-ref int-pairs 110))

(equal? '(6 6) (stream-ref int-pairs 62))
(equal? '(6 7) (stream-ref int-pairs 94))
(equal? '(6 8) (stream-ref int-pairs 158))
(equal? '(6 9) (stream-ref int-pairs 222))

(equal? '(7 7) (stream-ref int-pairs 126))
(equal? '(7 8) (stream-ref int-pairs 190))
(equal? '(7 9) (stream-ref int-pairs 318))
(equal? '(7 10) (stream-ref int-pairs 446))

(equal? '(8 8) (stream-ref int-pairs 254))
(equal? '(8 9) (stream-ref int-pairs 382))
(equal? '(8 10) (stream-ref int-pairs 638))
(equal? '(8 11) (stream-ref int-pairs 894))
