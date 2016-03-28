(require-extension sicp)
(require-extension numbers)

;; Code from rational number math in 2.1

;; (define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; Improved make-rat
;; (define (make-rat n d)
;;   (let ((g (gcd n d)))
;;     (cons (/ n g)
;;           (/ d g))))

;; from 1.2.5
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Exercise 2.1: Define a better version of make-rat that handles both
;; positive and negative arguments. Make-rat should normalize the sign
;; so that if the rational number is positive, both the numerator and
;; denominator are positive, and if the rational number is negative,
;; only the numerator is negative.

(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((n (/ n g))
          (d (/ d g)))
      (let ((f (if (negative? d)
                   (lambda (x) (* -1 x))
                   identity)))
        (cons (f n) (f d))))))

;; Exercise 2.2: Consider the problem of representing line segments in
;; a plane. Each segment is represented as a pair of points: a
;; starting point and an ending point. Define a constructor
;; make-segment and selectors start-segment and end-segment that
;; define the representation of segments in terms of points.
;; Furthermore, a point can be represented as a pair of numbers: the x
;; coordinate and the y coordinate. Accordingly, specify a constructor
;; make-point and selectors x-point and y-point that define this
;; representation. Finally, using your selectors and constructors,
;; define a procedure midpoint-segment that takes a line segment as
;; argument and returns its midpoint (the point whose coordinates are
;; the average of the coordinates of the endpoints). To try your
;; procedures, you’ll need a way to print points:

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment s)
  (let ((x (/ (+ (x-point (start-segment s))
                 (x-point (end-segment s)))
              2))
        (y (/ (+ (y-point (start-segment s))
                 (y-point (end-segment s)))
              2)))
    (make-point x y)))

;; Exercise 2.3: Implement a representation for rectangles in a plane.
;; (Hint: You may want to make use of Exercise 2.2.) In terms of your
;; constructors and selectors, create procedures that compute the
;; perimeter and the area of a given rectangle. Now implement a
;; different representation for rectangles. Can you design your system
;; with suitable abstraction barriers, so that the same perimeter and
;; area procedures will work using either representation?

(define (make-rectangle bottom-left top-right)
  (make-segment bottom-left top-right))

(define (height r)
  (- (y-point (end-segment r))
     (y-point (start-segment r))))

(define (width r)
  (- (x-point (end-segment r))
     (x-point (start-segment r))))

(define (perimeter r)
  (+ (* 2 (height r))
     (* 2 (width r))))

(define (area r)
  (* (height r) (width r)))

(define (make-rectangle-1 bottom-left height width)
  (let ((dims (cons height width)))
    (cons bottom-left dims)))

(define (height r)
  (car (cdr r)))

(define (width r)
  (cdr (cdr r)))
