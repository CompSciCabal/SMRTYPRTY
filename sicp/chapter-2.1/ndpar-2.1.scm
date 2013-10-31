#lang racket

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

; p.84
;(define (add-rat x y)
;  (make-rat (+ (* (numer x) (denom y))
;               (* (numer y) (denom x)))
;            (* (denom x) (denom y))))

; p.86
(define numer car)
(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; Exercise 2.1, p.87
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

; Exercise 2.2, p.89
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

; Exercise 2.3, p.90
(define (perimeter rec)
  (* 2 (+ (rlength rec) (rwidth rec))))

(define (area rec)
  (* (rlength rec) (rwidth rec)))

; Representation 1
(define (make-rec-1 length width center slope)
  (cons (cons length width) (cons center slope)))

;(define (rlength rec)
;  (caar rec))

;(define (rwidth rec)
;  (cdar rec))

;(define rectangle (make-rec-1 10 5 (make-point 3 3) 3/2))

; Representation 2
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