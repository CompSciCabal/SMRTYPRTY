;#lang planet neil/sicp
#lang racket

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
;; I like this problem, but now how the book poses it. It was very
;; confusing and I had to look up stuff in external resources.

(define zero (lambda (f) (lambda (x) x)))

;(add-1 zero)
;(lambda (f) (lambda (x) (f ((zero f) x))))
;(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
(define one (lambda (f) (lambda (x) (f x))))

;; (add-1 one) =>
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

;; The new addition function just composes the two numbers it receives. Let f
;; be a function of x. The n-th number is f^n(x) and the m-th number is f^m(x).
;; m + n => f^(m+n)(x) => f^m(f^n(x)) == f^n(f^m(x)).

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; building upon add-1:
(define (new+ a b) 
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

;; for this problem, I used: 
;; http://www.billthelizard.com/2010/10/sicp-26-church-numerals.html
(define (inc n) (+ 1 n))

;; (((new+ two one) inc) 0) -> 3
;; (define four (new+ two two))
;; ((four inc) 0)

;; 2.7
(define (make-interval a b) (cons a b))
(define upper-bound cdr)
(define lower-bound car)

(define (add-interval  x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

;; 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; 2.9
;; width: half the difference bw its upper bound and lower bound

;; Let (a,b), (c,d) be ordered intervals, where the LHS is the lowest
;; (a,b) + (c,d)
;; => (a+c, b+d)
;; width(a+c, b+d)
;; => 1/2(b+d-(a+c))
;; => 1/2(b+d-a-c)
;; => 1/2((b-a)+(d-c))

;; (a,b) - (c,d)
;; => (a-d, b-c)
;; width(a-d, b-c)
;; => 1/2(b-c-(a-d))
;; => 1/2(b-c-a+d)
;; => 1/2(b-a+(d-c))

;; Thus, width((a,b) + (c,d)) = width((a,b) - (c,d)).
;; I hope that's what this question was asking ...

;; Give examples to show this is not true for multiplication and division ...
;; ... skipped

;; 2.10
(define (mul-interval x y)
  (let ((p1 (+ (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (and (>= (lower-bound y) 0) (<= (upper-bound y) 0))
      (display 'error)
      (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y))))))
                      
;; 2.11
;; I suspect that Ben Bitdiddle, the expert systems programmer, was probably
;; smoking crack with Rob Ford just before he made this "suggestion".

;; This is almost completely wrong.
(define (mul-interval-crack x y)
  (define lb lower-bound)
  (define ub upper-bound)
  (cond
    ;; [a,b]*[c,d]
    ;; [+ +]x[+ +]
    [(andmap positive? (list (lb x) (ub x) (lb y) (ub y)))
     (make-interval (* (lb x) (lb y)) (* (ub x) (ub y)))]
    ;; [+ +]x[+ -] (not possible)
    ;; [+ +]x[- +] 
    [(and (negative? (lb y)) (andmap positive? (list (lb x) (ub x) (lb y))))
     (make-interval (* (lb x) (lb y)) (* (ub x) (ub y)))]
    ;; [+ +]x[- -] => (ad, bc)
    [(andmap positive? (list (lb x) (ub x)))
     (make-interval (* (lb x) (ub y)) (* (ub x) (lb y)))]
    ;; [- +]x[+ +] normal case
    ;; [- +]x[- +] the special case ... todo
    ;; [- +]x[+ -] (not possible)
    ;; [- +]x[- -] => (bd, ac)
    [(and (andmap negative? (list (lb x) (lb y) (ub y))) (positive? (ub x)))
     (make-interval (* (ub x) (ub y)) (* (lb x) (lb y)))]
    ;; the other two [- +] cases from aboveS
    [(and (negative? (lb x)) (positive? (ub x)))
     (make-interval (* (lb x) (lb y)) (* (ub x) (ub y)))]
    ;; [- -]x[+ +] => (bd,ac)
    [(and (andmap negative? (list (lb x) (ub x)))
          (andmap positive? (list (lb y) (ub y))))
     (make-interval (* (ub x) (ub y)) (* (lb x) (lb y)))]
    ;; [- -]x[+ -] (not possible)
    ;; [- -]x[- +] 
    [(and (andmap negative? (list (lb x) (ub x) (lb y))) (positive? (ub y)))
     (make-interval (* (ub x) (ub y)) (* (lb x) (lb y)))]
    ;; [- -]x[- -] 
    [else (make-interval (* (ub x) (ub y)) (* (lb x) (lb y)))]))

;; Tests for 2.11 stolen from Bill the Lizard:
;(define a (make-interval 2 4))
;(define b (make-interval -2 4))
;(define c (make-interval -4 -2))
;> (mul-interval a a)
;(4 . 16)
;> (mul-interval a b)
;(-8 . 16)
;> (mul-interval a c)
;(-16 . -4)
;> (mul-interval b a)
;(-8 . 16)
;> (mul-interval b b)
;(-8 . 16)
;> (mul-interval b c)
;(-16 . 8)
;> (mul-interval c a)
;(-16 . -4)
;> (mul-interval c b)
;(-16 . 8)
;> (mul-interval c c)
;(4 . 16)
  
;; 2.12
(define (make-center-percent center pt)
  (make-interval (* (- 1 pt) center)
                 (* (+ 1 pt) center)))

(define (percent t)
  (/ (- (cdr t) (car t)) 20))

;; 2.13
;; Assume low percentage tolerances. Show there's a simple formula for the
;; "approximate percentage tolerance" of the product of two intervals in terms
;; of the tolerances of the factors. Assume all numbers are positive.

;; In general, (a,b)x(c,d) => (ac,bd).
;; We want to solve for ?? in this:
;; (x +- y)*(u +- v) = (xu +- ??)
;;
;; The midpoint between ac and bd is (ac + bd)/2. Thus, we could express
;; (a,b)x(c,d)'s solution in the center/pt form as 
;; (ac + bd)/2 +- (bd - ac)/2.
;;
;; We must work backwards to find the values of x,y,u, and v in this form:
;; (x +- y)*(u +- v) = (xu +- ??)
;;
;; ... not interested in finishing this problem.


;; 2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define a (make-interval 300 2))
(define b (make-interval 100 3))
(define ab (div-interval a b))
(define aa (div-interval a a))

;> ab
;'(0.02 . 300.3333333333333)
;> aa
;'(0.006666666666666667 . 300.5)

;; 2.15

;; I suspect she is right, because the less intervals one
;; introduces, the less error there is to propagate. I
;; haven't a proof, though, just this intuition based
;; on many, many physics labs.


;; 2.16 - Attempt #2
;;
;; This is impossible.
;;
;; The first solution which came to my mind was kind of science fiction-y
;; and involved transforming all algebraic expressions (expressed as functions)
;; into some "normal" form via symbolic computing. This is already a very sketchy
;; solution because of the giant number of ways an algebraic expression could be
;; implemented. Then, all expressions would be evaluated after being converted 
;; to some "normal" form. But after you computed a value and wanted to use it
;; in another expression, that value would be "impure", because it came from
;; some other expression and by giving it to another function to use in a 
;; computation, you'd be contaminating it. Then you'd have to keep track of where
;; which numbers came from and blah. This particular problem was pointed out by
;; Dan, Andrey, and Leo at the November 10th meeting. Clearly, this is a ridiculous
;; solution.
;;
;; Above, I wasn't really thinking about interval arithmatic in particular. There
;; isn't a concept of an "indentity" (as demonstrated by exercise 2.14) the system
;; we've developed so far. Wikipedia says this about the dependency problem (which
;; is what this is): 
;;
;; "In general, it can be shown that the exact range of values can be achieved,
;; if each variable appears only once and if f is continuous inside the box. 
;; However, not every function can be rewritten this way."
;;
;; So, as problem 2.15 demonstrated, being able to reduce the number of variables
;; (especially down to once) is really powerful. Since not every function can be
;; written this way, then it is impossible.


;; original incoherent text is below and a copy should be sent to
;; http://shitmystudentswrite.tumblr.com/

;; off-topic: what is the "recursive poisoning clause"?
;; i shall ramble now, but there are no proofs
;; i don't know if this makes any senses
;; when we say two algebraic expressions are "equivalent",
;; we only mean they are "equivalent" in perfect math land*
;; where there is no error which can be propagated when evaluating
;; these expressions. ofc this is not the case irl, because there's
;; no perfect precision system in computing (not yet? (I guess that's the point of this question)).
;; but can we determine if two algebraic expressions are equivalent?
;; wolframalpha certainly can't, but it can do a pretty decent job.
;; can humans determine if two algebraic expressions are the same?
;; (is this what symbolic computation is?)
;; we cannot yet determine if any two programs are the same (is this true?)
;; 
;; and even if we could determine that the two were "equivalent",
;; after you performed your computation with both and got different
;; answers, would you just average them? (I don't think you should
;; average them, if that was the case, but take the one which involved
;; the least number of error-propagating variables/calculations as being
;; close to the "true" value)
;;
;; tl;dr: cannot implement the package because precision problems + 
;; symbolic computing isn't caught up yet (I just made that last part up)

;; * ignoring calculus
