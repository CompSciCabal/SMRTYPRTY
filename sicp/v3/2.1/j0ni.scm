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

;; Exercise 2.1: Define a better version of make-rat that handles both positive
;; and negative arguments. Make-rat should normalize the sign so that if the
;; rational number is positive, both the numerator and denominator are positive,
;; and if the rational number is negative, only the numerator is negative.

(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((n (/ n g))
          (d (/ d g)))
      (let ((f (if (negative? d)
                   (lambda (x) (* -1 x))
                   identity)))
        (cons (f n) (f d))))))

;; Exercise 2.2: Consider the problem of representing line segments in a plane.
;; Each segment is represented as a pair of points: a starting point and an
;; ending point. Define a constructor make-segment and selectors start-segment
;; and end-segment that define the representation of segments in terms of
;; points. Furthermore, a point can be represented as a pair of numbers: the x
;; coordinate and the y coordinate. Accordingly, specify a constructor
;; make-point and selectors x-point and y-point that define this representation.
;; Finally, using your selectors and constructors, define a procedure
;; midpoint-segment that takes a line segment as argument and returns its
;; midpoint (the point whose coordinates are the average of the coordinates of
;; the endpoints). To try your procedures, you’ll need a way to print points:

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

;; Exercise 2.3: Implement a representation for rectangles in a plane. (Hint:
;; You may want to make use of Exercise 2.2.) In terms of your constructors and
;; selectors, create procedures that compute the perimeter and the area of a
;; given rectangle. Now implement a different representation for rectangles. Can
;; you design your system with suitable abstraction barriers, so that the same
;; perimeter and area procedures will work using either representation?

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

;; 2.1.3 What is meant by data

(define (cons-1 x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else
           (error "Argument not 0 or 1:
                   CONS" m))))
  dispatch)

(define (car-1 z) (z 0))
(define (cdr-1 z) (z 1))

;; Exercise 2.4: Here is an alternative procedural representation of pairs. For
;; this representation, verify that (car (cons x y)) yields x for any objects x
;; and y.

(define (cons-2 x y)
  (lambda (m) (m x y)))

(define (car-2 z)
  (z (lambda (p q) p)))

;; What is the corresponding definition of cdr? (Hint: To verify that this
;; works, make use of the substitution model of 1.1.5.)

(define (cdr-2 z)
  (z (lambda (p q) q)))

;; Exercise 2.5: Show that we can represent pairs of nonnegative integers using
;; only numbers and arithmetic operations if we represent the pair a and b as
;; the integer that is the product 2^a3^b. Give the corresponding definitions of
;; the procedures cons, car, and cdr.

(define (pow a b)
  (define (iter acc count)
    (if (= count 0)
        acc
        (iter (* acc a) (- count 1))))
  (iter 1 b))

(define (cons-3 a b)
  (* (pow 2 a)
     (pow 3 b)))

;; The property of this combination which allows us to extract a and b is known
;; as the fundamental theorem of arithmetic - 2 and 3 are prime and share no
;; common factors. This means that we can divide by 2 until all that is left is
;; a power of 3, or vice versa.

(define (power-reduce y x)
  (if (= 0 (remainder x y))
      (power-reduce y (/ x y))
      x))

(define (power-count y x)
  (define (iter acc n)
    (if (= n y)
        acc
        (iter (inc acc) (/ n y))))
  (iter 1 x))

(define (car-3 x)
  (power-count 2 (power-reduce 3 x)))

(define (cdr-3 x)
  (power-count 3 (power-reduce 2 x)))

;; Exercise 2.6: In case representing pairs as procedures wasn’t mind-boggling
;; enough, consider that, in a language that can manipulate procedures, we can
;; get by without numbers (at least insofar as nonnegative integers are
;; concerned) by implementing 0 and the operation of adding 1 as

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; This representation is known as Church numerals, after its inventor, Alonzo
;; Church, the logician who invented the λ-calculus.

;; Define one and two directly (not in terms of zero and add-1). (Hint: Use
;; substitution to evaluate (add-1 zero)). Give a direct definition of the
;; addition procedure + (not in terms of repeated application of add-1).

;; (add-1 zero)
;; (add-1 (lambda (f) (lambda (x) x)))
;; (lambda (f) (lambda (x) (f (((lambda (f1) (lambda (x) x)) f) x))))
;; (lambda (f) (lambda (x) (f ((lambda (x) x) x))))

(define one
  (lambda (f) (lambda (x) (f x))))

;; (add-1 (add-1 zero))
;; (add-1 (lambda (f) (lambda (x) (f x))))
;; (lambda (f) (lambda (x) (f (((lambda (f1) (lambda (x) (f1 x))) f) x))))
;; (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

;; We can make use of the repeated application of f, which corresponds with the
;; number represented, to apply add-1 enough times.

(define (church-add a b)
  ((a add-1) b))

;; this is useful for testing:

(define (church->int x)
  ((x inc) 0))

;; 2.1.4 Interval arithmetic

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval
                 (/ 1.0 (upper-bound y))
                 (/ 1.0 (lower-bound y)))))

;; Exercise 2.7: Alyssa’s program is incomplete because she has not specified
;; the implementation of the interval abstraction. Here is a definition of the
;; interval constructor:

(define (make-interval a b) (cons a b))

;; Define selectors upper-bound and lower-bound to complete the implementation.

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

;; Exercise 2.8: Using reasoning analogous to Alyssa’s, describe how the
;; difference of two intervals may be computed. Define a corresponding
;; subtraction procedure, called sub-interval.

(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))

;; Should represent the range of all possible values of y subtracted from all
;; possible values of x

;; Exercise 2.9: The width of an interval is half of the difference between its
;; upper and lower bounds. The width is a measure of the uncertainty of the
;; number specified by the interval. For some arithmetic operations the width of
;; the result of combining two intervals is a function only of the widths of the
;; argument intervals, whereas for others the width of the combination is not a
;; function of the widths of the argument intervals. Show that the width of the
;; sum (or difference) of two intervals is a function only of the widths of the
;; intervals being added (or subtracted). Give examples to show that this is not
;; true for multiplication or division.

(define (width x)
  (/ (- (upper-bound x)
        (lower-bound x))
     2))

(define (test-width-math fun)
  (let ((interval-1 (make-interval 7 9))
        (interval-2 (make-interval 3 7))
        (interval-3 (make-interval 7 10))
        (interval-4 (make-interval 2 3))
        (interval-5 (make-interval 7 10)))
    (print "interval-1 op interval-2 "
           (width interval-1) " "
           (width interval-2) " "
           (width (fun interval-1 interval-2)))
    (print "interval-2 op interval-3 "
           (width interval-2) " "
           (width interval-3) " "
           (width (fun interval-2 interval-3)))
    (print "interval-3 op interval-4 "
           (width interval-3) " "
           (width interval-4) " "
           (width (fun interval-3 interval-4)))
    (print "interval-4 op interval-5 "
           (width interval-4) " "
           (width interval-5) " "
           (width (fun interval-4 interval-5)))))

;; #;151> (test-width-math add-interval)
;; interval-1 op interval-2 1 2 3
;; interval-2 op interval-3 2 1.5 3.5
;; interval-3 op interval-4 1.5 0.5 2
;; interval-4 op interval-5 0.5 1.5 2
;; #;154> (test-width-math sub-interval)
;; interval-1 op interval-2 1 2 3
;; interval-2 op interval-3 2 1.5 3.5
;; interval-3 op interval-4 1.5 0.5 2
;; interval-4 op interval-5 0.5 1.5 2

;; for add-interval and sub-interval, (width (*-interval interval-1 interval-2))
;; is given by (+ (width interval-1) (width interval-2))

;; #;182> (test-width-math mul-interval)
;; interval-1 op interval-2 1 2 21
;; interval-2 op interval-3 2 3/2 49/2
;; interval-3 op interval-4 3/2 1/2 8
;; interval-4 op interval-5 1/2 3/2 8
;; #;186> (test-width-math div-interval)
;; interval-1 op interval-2 1 2 1.0
;; interval-2 op interval-3 2 3/2 0.35
;; interval-3 op interval-4 3/2 1/2 1.33333333333333
;; interval-4 op interval-5 1/2 3/2 0.114285714285714

;; Max figured these out algebraically. Hahaha. Too little too late.

;; Exercise 2.10: Ben Bitdiddle, an expert systems programmer, looks over
;; Alyssa’s shoulder and comments that it is not clear what it means to divide
;; by an interval that spans zero. Modify Alyssa’s code to check for this
;; condition and to signal an error if it occurs.

(define (spans-zero? a)
  (negative? (* (upper-bound a)
                (lower-bound a))))

(define (div-interval-safe x y)
  (if (spans-zero? y)
      (error "cannot divide by an interval spanning 0")
      (mul-interval x
                    (make-interval
                     (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))

(define (test-divide-by-zero)
  (let ((i-1 (make-interval 9 12))
        (i-2 (make-interval -3 5)))
    (div-interval-safe i-1 i-2)))

(define (test-divide-by-non-zero)
  (let ((i-1 (make-interval 9 12))
        (i-2 (make-interval 3 5)))
    (div-interval-safe i-1 i-2)))

;; Exercise 2.11: In passing, Ben also cryptically comments: “By testing the
;; signs of the endpoints of the intervals, it is possible to break mul-interval
;; into nine cases, only one of which requires more than two multiplications.”
;; Rewrite this procedure using Ben’s suggestion.

;; cases:
;; both positive
;; both negative
;; x pos, y neg
;; y pos, x neg
;; x spans 0, y pos
;; x spans 0, y neg
;; y spans 0, x pos
;; y spans 0, x neg
;; both span 0

(define (mul-interval-cases x y)
  (define (positive-interval? x)
    (positive? (lower-bound x)))

  (define (negative-interval? x)
    (negative? (upper-bound x)))

  (let ((xu (upper-bound x))
        (xl (lower-bound x))
        (yu (upper-bound y))
        (yl (lower-bound y)))

    (cond ((and (positive-interval? x)
                (positive-interval? y))
           (make-interval (* xl yl)
                          (* xu yu)))

          ((and (negative-interval? x)
                (negative-interval? y))
           (make-interval (* xu yu)
                          (* xl yl)))

          ((and (positive-interval? x)
                (negative-interval? y))
           (make-interval (* xu yl)
                          (* xl yu)))

          ((and (negative-interval? x)
                (positive-interval? y))
           (make-interval (* xl yu)
                          (* xu yl)))

          ((and (spans-zero? x)
                (positive-interval? y))
           (make-interval (* xl yu)
                          (* xu yu)))

          ((and (spans-zero? x)
                (negative-interval? y))
           (make-interval (* xu yl)
                          (* xl yu)))

          ((and (spans-zero? y)
                (positive-interval? x))
           (make-interval (* xu yl)
                          (* xu yu)))

          ((and (spans-zero? y)
                (negative-interval? x))
           (make-interval (* xl yu)
                          (* xl yl)))

          ((and (spans-zero? x)
                (spans-zero? y))
           (mul-interval x y))

          (else
           (error "unprecedented combination of intervals!")))))

;; After debugging her program, Alyssa shows it to a potential user,
;; who complains that her program solves the wrong problem. He wants a
;; program that can deal with numbers represented as a center value
;; and an additive tolerance; for example, he wants to work with
;; intervals such as 3.5 ±± 0.15 rather than [3.35, 3.65]. Alyssa
;; returns to her desk and fixes this problem by supplying an
;; alternate constructor and alternate selectors:

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))

(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))

;; Unfortunately, most of Alyssa’s users are engineers. Real
;; engineering situations usually involve measurements with only a
;; small uncertainty, measured as the ratio of the width of the
;; interval to the midpoint of the interval. Engineers usually specify
;; percentage tolerances on the parameters of devices, as in the
;; resistor specifications given earlier.
