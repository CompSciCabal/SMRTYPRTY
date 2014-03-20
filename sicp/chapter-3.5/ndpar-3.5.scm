#lang planet neil/sicp

(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (display-line x)
  x)

;; -------------------------------------------------------------------
;; Streams, p.316
;; -------------------------------------------------------------------

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

(define (stream-first s n)
  (define (iter i)
    (if (= i n)
        nil
        (cons (stream-ref s i) (iter (+ i 1)))))
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

;; -------------------------------------------------------------------
;; Infinite Streams, p.326
;; -------------------------------------------------------------------

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

;; Exerciese 3.60, p.333

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