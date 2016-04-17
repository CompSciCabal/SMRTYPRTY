(require-extension sicp)


;; Exercise 2.1: Define a better version of make-rat that handles both positive
;; and negative arguments. Make-rat should normalize the sign so that if the
;; rational number is positive, both the numerator and denominator are positive,
;; and if the rational number is negative, only the numerator is negative.

(define (make-rat n d)
  ;; Assume both n and d are not 0
  (let ((g (gcd n d))
        (multiplier (cond ((and (> n 0) (> d 0)) 1)
                          ((and (< n 0) (< d 0)) -1)
                          ((and (< n 0) (> d 0)) 1)
                          ((and (> n 0) (< d 0)) -1))))
    (cons (* multiplier (/ n g))
          (* multiplier (/ d g)))))



;; ----------------------------------------------------------------------
;; Exercise 2.2: Consider the problem of representing line segments in a plane.
;; Each segment is represented as a pair of points: a starting point and an
;; ending point. Define a constructor make-segment and selectors start-segment
;; and end-segment that define the representation of segments in terms of
;; points. Furthermore, a point can be represented as a pair of numbers: the xx
;; coordinate and the yy coordinate. Accordingly, specify a constructor
;; make-point and selectors x-point and y-point that define this representation.
;; Finally, using your selectors and constructors, define a procedure
;; midpoint-segment that takes a line segment as argument and returns its
;; midpoint (the point whose coordinates are the average of the coordinates of
;; the endpoints). To try your procedures, you’ll need a way to print points:

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


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
  (make-point (average (x-point (start-segment s))
                       (x-point (end-segment s)))
              (average (y-point (start-segment s))
                       (y-point (end-segment s)))))

;; -----------------------------------------------------------------------
;; Exercise 2.4: Here is an alternative procedural representation of pairs. For
;; this representation, verify that (car (cons x y)) yields x for any objects x
;; and y.

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

;; What is the corresponding definition of cdr? (Hint: To verify that this works, make use of the substitution model of 1.1.5.)


;; Using the substitution model

;; (car (cons x y))
;; (car (lambda (m) (m x y)))
;; ((lambda (m) (m x y)) (lambda (p q) p))
;; ((lambda (p q) p) x y)
;; x

(define (cdr z)
  (z (lambda (p q) q)))

;; Using the substitution model

;; (cdr (cons x y))
;; (cdr (lambda (m) (m x y)))
;; ((lambda (m) (m x y)) (lambda (p q) q))
;; ((lambda (p q) q) x y)
;; y


;; Exercise 2.3: Implement a representation for rectangles in a plane. (Hint:
;; You may want to make use of Exercise 2.2.) In terms of your constructors and
;; selectors, create procedures that compute the perimeter and the area of a
;; given rectangle. Now implement a different representation for rectangles. Can
;; you design your system with suitable abstraction barriers, so that the same
;; perimeter and area procedures will work using either representation?

;; Representation 1, Rectangle is represented as bottom left and top right
;; corner. We're going to make the assumption that the first point has a lower x
;; and y coordinate than the second to make things easier

(define (make-rect p1 p2)
  (cons p1 p2))

(define (width-rect r)
  (- (x-point (cdr r)) (x-point (car r))))

(define (height-rect r)
  (- (y-point (cdr r)) (y-point (car r))))

(define (perim-rect r)
  (+ (* 2 (width-rect r))
     (* 2 (height-rect r))))

(define (area-rect r)
  (* (width-rect r)
     (height-rect r)))


(area-rect (make-rect (make-point 1 2) (make-point 5 7)))
;; => 20
(perim-rect (make-rect (make-point 1 2) (make-point 5 7)))
;; 18

;; Representation 2, Rectangle is the bottom left corner, a width and a height:

(define (make-rect p w h)
  (cons p (cons w h)))

(define (width-rect r)
  (car (cdr r)))

(define (height-rect r)
  (cdr (cdr r)))

(area-rect (make-rect (make-point 1 2) 4 5))
;; => 20

(perim-rect (make-rect (make-point 1 2) 4 5))
;; => 18


;; --------------------------------------------------------------------------------

;; Exercise 2.5: Show that we can represent pairs of nonnegative integers using
;; only numbers and arithmetic operations if we represent the pair a and b as
;; the integer that is the product 2^a*3^b Give the corresponding definitions
;; of the procedures cons, car, and cdr.


;; Since 2 and 3 are relatively prime, we can just find how many factors of two and three go into 2^a*3^b to find a and b

(define (cons2 a b)
  (* (expt 2 a) (expt 3 b)))

;; Compute number of times e divides n
(define (factor n e)
  (define (iter acc n)
    (if (= (remainder n e) 0)
        (iter (+ acc 1) (/ n e))
        acc))
  (iter 0 n))

(define (car n)
  (factor n 2))

(define (cdr n)
  (factor n 3))

;; ----------------------------------------------------------------------------------

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

;; Using substitution

;; (add-1 zero)
;; (add-1 (lambda (f) (lambda (x) x)))
;; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))))
;; (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
;; (lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))

;; (add-1 one)
;; (add-1 (lambda (f) (lambda (x) (f x))))
;; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
;; (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
;; (lambda (f) (lambda (x) (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (church->int n)
  ((n inc) 0))

;; ------------------------------------------------------------------------------

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

;; bug 
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

(define (upper-bound i)
  (cdr i))

(define (lower-bound i)
  (car i))

;; ------------------------------------------------------------------------------

;; Exercise 2.8: Using reasoning analogous to Alyssa’s, describe how the
;; difference of two intervals may be computed. Define a corresponding
;; subtraction procedure, called sub-interval.

;; The minimal value of subtraction is the lower bound of the first - the upper
;; bound of the second. The maximum is the upper bound of the first - the lower
;; bound of the second

(define (sub-interval x y)
  (make-interval (- (lower-bound x) 
                    (upper-bound y))
                 (- (upper-bound x) 
                    (lower-bound y))))


;; -------------------------------------------------------------------------------

;; Exercise 2.9: The width of an interval is half of the difference between its
;; upper and lower bounds. The width is a measure of the uncertainty of the
;; number specified by the interval. For some arithmetic operations the width of
;; the result of combining two intervals is a function only of the widths of the
;; argument intervals, whereas for others the width of the combination is not a
;; function of the widths of the argument intervals. Show that the width of the
;; sum (or difference) of two intervals is a function only of the widths of the
;; intervals being added (or subtracted). Give examples to show that this is not
;; true for multiplication or division.

;; For addition: if interval A has lower bound A_l and upper bound A_b, and B
;; likewise is B_l, B_u, and the widths A_w, B_w then the sum is (A_l+B+l, A_u + B_u) and thus the width
;; is (A_u+B_u-A_l-B_l)/2 = (A_u-A-l)/2 + (B_u - B_l)/2 = A_w+B_w

;; For subtraction: A-B = (A_l - B_u, A_u - B_l), so the width is
;; (A_u - B_l - A_l + B_u)/2 = (A_u - A_l)/2 + (B_u - B_l)/2
;; = A_w + B_w


;; For multiplication, consider the most trivial example: multiplying an interval by itself.
;; A*A = (A_l^2, A_u^2), so the width is (A_u^2-A_l^2)/2 = (A_u-A_l)(A_u+A_l)/2 = A_w(A_u+A_l)/2
;; The extra term, A_u+A_l, can't be computed from just the width.

;; Likewise, for division (assuming that 1 < A_l < A_u < B_l < B_u)

;; A / B = (A_l,A_u) * (1/B_l,1/B_u) = (A_l/B_u, A_u/B_l)
;; The width is (A_u/B-l - A_l/B_u)/2 = (A_u*B_u - A_l*B_l)/2(B_u*B_l)
;; Which is not a function of the widths.

;; ---------------------------------------------------------------------------------

;; Exercise 2.10: Ben Bitdiddle, an expert systems programmer, looks over
;; Alyssa’s shoulder and comments that it is not clear what it means to divide
;; by an interval that spans zero. Modify Alyssa’s code to check for this
;; condition and to signal an error if it occurs.


(define (div-interval x y)
  (if (or (= 0 (upper-bound y))
          (= 0 (lowe-bound y)))
      (error "Argument spans 0")
      (mul-interval x 
                    (make-interval 
                     (/ 1.0 (upper-bound y)) 
                     (/ 1.0 (lower-bound y))))))



;; --------------------------------------------------------------------------

;; Exercise 2.11: In passing, Ben also cryptically comments: “By testing the
;; signs of the endpoints of the intervals, it is possible to break mul-interval
;; into nine cases, only one of which requires more than two multiplications.”
;; Rewrite this procedure using Ben’s suggestion.

(define (mul-interval x y)
  ;; An interval's lower bound is positive, thus it's entirely positive
  (define (positive-interval? n)
    (positive? (lower-bound x)))

  ;; An interval's upper bound is negative, thus it's entirely negative
  (define (negative-interval? n)
    (negative? (upper-bound n)))

  (let ((x-l (lower-bound x))
        (x-u (upper-bound x))
        (y-l (lower-bound y))
        (y-u (upper-bound y)))


    (cond ((and (positive-interval? x) ;; Both are positive
                (positive-interval? y))
           (make-interval (* x-l y-l)
                          (* x-u y-u)))

          ((and (negative-interval? x) ;; Both are negative
                (negative-interval? y))
           (make-interval (* x-u y-u)
                          (* x-l y-l)))

          ((and (positive-interval? x) ;; x positive, y negative
                (negative-interval? y))
           (make-interval (* x-u y-l)
                          (* x-l y-u)))

          ((and (negative-interval? x) ;; x negative, y positive
                (positive-interval? y))
           (make-interval (* x-l y-u)
                          (* x-u y-l)))

          ((positive-interval? x) ;; x positive, y spans 0
           (make-interval (* x-u y-l)
                          (* x-u y-u)))

          ((negative-interval? x) ;; x negative, y spans 0
           (make-interval (* x-l y-u)
                          (* x-u y-l)))

          ((positive-interval? y) ;; x spans 0, y positive
           (make-interval (* x-l y-u)
                          (* x-u y-u)))

          ((negative-interval? y) ;; x spans 0, y negative
           (make-interval (* x-u y-l)
                          (* x-l y-u)))

          (else ;; both span 0
           (let ((p1 (* x-l
                        y-l))
                 (p2 (* x-l
                        y-l))
                 (p3 (* x-u
                        y-l))
                 (p4 (* x-u
                        y-u)))
             (make-interval (min p1 p2 p3 p4)
                            (max p1 p2 p3 p4)))))))

;; -----------------------------------------------------------------

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


;; Exercise 2.12: Define a constructor make-center-percent that takes a center
;; and a percentage tolerance and produces the desired interval. You must also
;; define a selector percent that produces the percentage tolerance for a given
;; interval. The center selector is the same as the one shown above.

(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100))))

(define (percent i)
  (* 100 (/ (width i) (center i))))

;; Exercise 2.13: Show that under the assumption of small percentage tolerances
;; there is a simple formula for the approximate percentage tolerance of the
;; product of two intervals in terms of the tolerances of the factors. You may
;; simplify the problem by assuming that all numbers are positive.

;; Assuming all numbers are positive, we have A*B = (A_l * B_l, A_u * B_u)

;; If A_l is A_c-A_w and A_u is A_c+A_w we have

;; the new lower bound as (A_c-A_w)*(B_c-B_w) = A_c*B_c - A_w*B_c - B_w*A_c + A_w*B_w
;; The new upper bound is (A_c+A_w)*(B_c+B_w) = A_c*B_c + A_w*B_c + B_w*A_c + A_w*B_w

;; If we assume the percentages are small, we can ignore A_w*B_w, and thus the width of the new interval becomes A_w*B-c+B_w+A_c

;; A_w and B_w are widths, in percentages they become A_c*A_p and B_c*B_p, so we have A_p*A_c*B_c+B_p*B_c*A_c.

;; A_c*B_c is the new center, which means the new tolerance is just the sum of the old tolerences, A_p+B_p.


;; ----------------------------------------

(define (par1 r1 r2)
  (div-interval 
   (mul-interval r1 r2)
   (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval 
     one
     (add-interval 
      (div-interval one r1) 
      (div-interval one r2)))))

;; Exercise 2.14: Demonstrate that Lem is right. Investigate the behavior of the
;; system on a variety of arithmetic expressions. Make some intervals A and B,
;; and use them in computing the expressions A/A and A/B. You will get the
;; most insight by using intervals whose width is a small percentage of the
;; center value. Examine the results of the computation in center-percent form
;; (see Exercise 2.12).


(define a (make-center-percent 20 1))
(define b (make-center-perfecent 20 5))

(center (par1 a b))
;;=> 10.0320288259433
(center (par2 a b))
;;=> 9.99599639675708

(percent (par1 a b))
;;=> 8.98084405866507
(percent (par2 a b))
;;=> 3.00240312406128

;; It looks like we're messing up the percent part.

(percent (div-interval a b))
;; => 5.99700149925037
(percent (div-interval a a))
;; => 1.99980001999799

;; My expectation is that A/A should be a point, with 0 percent tolerence?

;; --------------------------------------------------------------

;; Exercise 2.15: Eva Lu Ator, another user, has also noticed the different
;; intervals computed by different but algebraically equivalent expressions. She
;; says that a formula to compute with intervals using Alyssa’s system will
;; produce tighter error bounds if it can be written in such a form that no
;; variable that represents an uncertain number is repeated. Thus, she says,
;; par2 is a “better” program for parallel resistances than par1. Is she right?
;; Why?

;; Yes I think so? Something to do with how the "real" value of uncertain
;; variables can only be "realized" once -- it has some true resistance and when
;; we use it in a computation, the value of A can't take on two different
;; meanings in the same computation.

;; --------------------------------------------------------------------------

;; Exercise 2.16: Explain, in general, why equivalent algebraic expressions may
;; lead to different answers. Can you devise an interval-arithmetic package that
;; does not have this shortcoming, or is this task impossible? (Warning: This
;; problem is very difficult.)

;; In general this has to do with intervals being a poor language for
;; uncertainty. A variable X may take on some value in a range, but it has to
;; fix to a given value in an entire expression. I have no idea how to do this
;; better but my guess it to look at symbolic programming or attend this meetup
;; http://www.meetup.com/Toronto-Probabilistic-Programming-Meetup/
