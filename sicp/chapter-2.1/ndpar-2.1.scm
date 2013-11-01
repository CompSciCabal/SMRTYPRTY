#lang racket

(define (inc n) (+ n 1))

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

;; p.84
;(define (add-rat x y)
;  (make-rat (+ (* (numer x) (denom y))
;               (* (numer y) (denom x)))
;            (* (denom x) (denom y))))

;; p.86
(define numer car)
(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; Exercise 2.1, p.87
;; Rational numbers
(define (make-rat n d)
  (let* ((g (gcd n d))
         (n1 (/ n g))
         (d1 (/ d g)))
    (cond ((< n 0 d) (cons n1 d1))
          ((< d 0 n) (cons (- n1) (- d1)))
          (else (cons (abs n1) (abs d1))))))

;(print-rat (make-rat 1 3))
;(print-rat (make-rat -1 3))
;(print-rat (make-rat 1 -3))
;(print-rat (make-rat -1 -3))

;; Exercise 2.2, p.89
;; Points and segments
(define (make-point x y)
  (cons x y))

(define x-point car)
(define y-point cdr)

(define (distance p1 p2)
  (sqrt (+ (square (- (x-point p1) (x-point p2)))
           (square (- (y-point p1) (y-point p2))))))

(define (make-segment start end)
  (cons start end))

(define start-segment car)
(define end-segment cdr)

(define (segment-length seg)
  (distance (start-segment seg) (end-segment seg)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment seg)
  (make-point (average (x-point (start-segment seg))
                       (x-point (end-segment seg)))
              (average (y-point (start-segment seg))
                       (y-point (end-segment seg)))))

;(print-point (midpoint-segment (make-segment (make-point 1 1)
;                                             (make-point 3 9))))

;; Exercise 2.3, p.90
;; Rectangles
(define (perimeter rec)
  (* 2 (+ (rlength rec) (rwidth rec))))

(define (area rec)
  (* (rlength rec) (rwidth rec)))

;; Representation 1
(define (make-rec-1 length width center slope)
  (cons (cons length width) (cons center slope)))

;(define (rlength rec)
;  (caar rec))

;(define (rwidth rec)
;  (cdar rec))

;(define rectangle (make-rec-1 10 5 (make-point 3 3) 3/2))

;; Representation 2
(define (make-rec-2 aside bside)
  (cons aside bside))

(define (side-lengths rec)
  (let ((a (segment-length (car rec)))
        (d (segment-length (cdr rec))))
    (if (< a d)
        (cons d a)
        (cons a d))))

(define (rlength rec)
  (car (side-lengths rec)))

(define (rwidth rec)
  (cdr (side-lengths rec)))

(define rectangle (make-rec-2 (make-segment (make-point 0 0)
                                            (make-point 6 8))
                              (make-segment (make-point 0 0)
                                            (make-point -4 3))))

;(perimeter rectangle)
;(area rectangle)

;; Exercise 2.4, p.92
;; Alternative representation of pairs
(define ((cons-1 x y) m)
  (m x y))

(define (car-1 z)
  (z (lambda (p q) p)))

(define (cdr-1 z)
  (z (lambda (p q) q)))

(define x 5)
(define y 10)
;(= x (car-1 (cons-1 x y)))
;(= y (cdr-1 (cons-1 x y)))

;; Exercise 2.5, p.92
;; Yet another representation of pairs
(define (cons-2 x y)
  (* (expt 2 x) (expt 3 y)))

(define (factor-out x n)
  (define (iter y acc)
    (if (= (remainder y n) 0)
        (iter (/ y n) (inc acc))
        acc))
  (iter x 0))

(define (car-2 z)
  (factor-out z 2))

(define (cdr-2 z)
  (factor-out z 3))

;(= x (car-2 (cons-2 x y)))
;(= y (cdr-2 (cons-2 x y)))

;; Exercise 2.6, p.93
;; Church numerals
(define (zero _) identity)

;(zero inc)
;((zero inc) "something")

(define ((add-1 n) f)
  (lambda (x) (f ((n f) x))))

;(add-1 zero)
;((add-1 zero) zero)
;(((add-1 zero) zero) "foo")
;((((add-1 zero) zero) "foo") "bar")

(define ((one f) x)
  (f x))

;one
;(one zero)
;((one zero) "foo")
;(((one zero) "foo") "bar")

;(add-1 one)
;((add-1 one) zero)
;(((add-1 one) zero) "foo")
;((((add-1 one) zero) "foo") "bar")

(define ((two f) x)
  (f (f x)))

;(= 5 ((zero inc) 5))

;(((add-1 zero) inc) 5)
;(= 6 ((one inc) 5))

;(((add-1 one) inc) 5)
;(= 7 ((two inc) 5))

(define ((plus n m) f)
  (lambda (x) (f ((n (m f)) x))))

;(= 8 (((plus one two) inc) 5))
