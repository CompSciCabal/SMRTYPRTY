#lang planet neil/sicp

;; Hello! I see you are reading my homework for Nov. 1. I won't attend the
;; meeting on that day, as I'm going to SoOnCon in KW.

;; Exercise 2.1. Define a better version of make-rat that handles both positive
;; and negative arguments. Make-rat should normalize the sign so that if the 
;; rational number is positive, both the numerator and denominator are positive,
;; and if the rational number is negative, only the numerator is negative.

;; old version:
;; (define (make-rat n d)
;;  (let ((g (gcd n d)))
;;    (cons (/ n g) (/ d g))))

(define numer car)
(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (cond
      [(and (> 0 n) (> 0 d)) (cons (/ (abs n) g) (/ (abs d) g))]
      [(or (> 0 n) (> 0 d)) (cons (/ (- 0 (abs n)) g) (/ (abs d) g))]
      [else (cons (/ n g) (/ d g))])))

;; Exercise 2.2 
(define (make-point x y) (cons x y))
(define x-point car)
(define y-point cdr)

(define (make-segment p1 p2) (cons p1 p2))
(define start-segment car)
(define end-segment cdr)

;; why is average is not built in for me?
(define (avg a b) (/ (+ a b) 2))

(define (midpoint-segment s)
  (make-point (avg (x-point (start-segment s)) (x-point (end-segment s)))
              (avg (y-point (start-segment s)) (y-point (end-segment s)))))

;; Exercise 2.3
;; "Implement a representation for rectangles in a plane."
(define (make-rectangle s0 s1 s2 s3) (cons s0 s1 s2 s3))
(define (side-1 r) (car r)) (define (side-2 r) (car (cdr r)))
(define (side-3 r) (car (cdr (cdr r))))
(define (side-4 r) (car (cdr (cdr (cdr r)))))
(define (sqr x) (* x x))
(define (segment-length s) 
  (sqrt (+ (sqr (- (x-point (start-segment s)) (x-point (end-segment s))))
           (sqr (- (y-point (start-segment s)) (y-point (end-segment s)))))))
  
(define (perimeter r)
  (apply + (map (lambda (x) (segment-length x)) (list (side-1 r) (side-2 r) (side-3 r) (side-4 r)))))

(define (area r) (* ( - (/ (perimeter r) 2) (segment-length (side-1 r))) (segment-length (side-1 r))))

;; Exercise 2.4
;; For this alternative procedural representation of pairs, verify that
;; (car (cons x y)) yields x for any objects x and y:

(define (new-cons x y) (lambda (m) (m x y)))
(define (new-car z) (z (lambda (p q) p)))

;; What is the corresponding definition of new-cdr?
;; I think this is correct?
(define (new-cdr z) (z (lambda (p q) q)))

;; Exercise 2.5
;; "Show that we can represent pairs of nonnegative integers using only numbers
;; andarithmetic operations if we represent the pair a and b as the integer that
;; is the product 2^a3^b. Give the corresponding definitions of the procedures
;; cons, car, and cdr."

;; This makes a lot of sense from a number theory/unique factorization point of 
;; view. I feel like I once had a dream about representing pairs like this.

(define (pow x y)
  (if (eq? y 0)
      1
      (* x (pow x (- y 1)))))

(define (new-cons-2 x y) (* (pow 2 x) (pow 3 y)))
(define (divisible? a b) ;; is a divisible by b?
  (if (integer? (/ a b))
      #t
      #f))

(define (divide-out z num count)
  (if (divisible? z num)
      (divide-out (/ z num) num (+ 1 count))
      count))

(define (new-car-2 z) (divide-out z 2 0))
(define (new-cdr-2 z) (divide-out z 3 0))

;; Exercise 2.6
;; Define one and two as Church numerals directly. Also give a direct
;; definition of + not in terms of repeated use of add-1.

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;(add-1 zero)
;(lambda (f) (lambda (x) (f ((zero f) x))))
;(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
(define one (lambda (f) (lambda (x) (f x))))

;; (add-1 one) =>
(define two (lambda (f) (lambda (x) (f (f x)))))

;; what
(define (new+ a b) 
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

;;(define (inc n) (+ 1 n))
;; (((new+ two one) inc) 0) -> 3
;; (define four (new+ two two))
;; ((four inc) 0)