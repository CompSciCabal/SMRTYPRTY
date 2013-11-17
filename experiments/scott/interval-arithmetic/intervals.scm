;;; Interval Arithmetic

;; Originally from the exercises of sicp chapter 2.1

;; Definitions modified to follow those given in

;; http://pages.cpsc.ucalgary.ca/~rokne/CPSC491/interval_art.pdf

;; main part is the newton interval method (in progress) but already working

;; on one example

;; Constructor and Selectors

(define (make-interval a b)
  (cons a b))
(define (upper-bound i)
  (cdr i))
(define (lower-bound i)
  (car i))
;; Alternate Constructors and selectors

(define (make-midpoint-percent c p)
  (let ((w (* c p 1e-2)))
    (make-interval (- c w) (+ c w))))
(define (make-midpoint-halfwidth c hw)
  (make-interval (- c hw) (+ c hw)))
(define (midpoint i)
  (/ (+ (upper-bound i) (lower-bound i)) 2))
(define (width i)
  (- (upper-bound i) (lower-bound i)))
(define (percent i)
  (* (/ (/ (width i) 2) (midpoint i)) 100))
;; utility functions

(define (equals-zero? x)
  (let ((tolerance 1e-6))
    (< (abs x) tolerance)))
(define +inf
  (* 1.7976931348623157 (expt 10 308)))
(define -inf
  (* -1.7976931348623157 (expt 10 308)))
(define (contains-zero? x)
  (and
    (<= (lower-bound x) 0)
    (>= (upper-bound x) 0)))
(define zero-interval
  (make-interval 0 0))
(define (zero-interval? x)
  (and
    (equals-zero? (lower-bound x))
    (equals-zero? (upper-bound x))))
(define (absmax x)
  (max (abs (lower-bound x)) (abs (upper-bound x))))
(define (absmin x)
  (if (contains-zero? x)
    0
    (min (abs (lower-bound x)) (abs (upper-bound x)))))
;; Addition

(define (add-interval x y)
  (make-interval
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))
;; Multiplication

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
  (p2 (* (lower-bound x) (upper-bound y)))
  (p3 (* (upper-bound x) (lower-bound y)))
  (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
;; Division

;; not defined for intervals containg zero

(define (div-interval x y)
  (if (equals-zero? (width y))
    (error "Dividing by zero span interval" y)
    (if (contains-zero? y)
      (error "Dividing by an interval containg zero" y)
      (mul-interval x (make-interval
        (/ 1.0 (upper-bound y))
        (/ 1.0 (lower-bound y)))))))
;; Extended Division, use this to divide by an interval containing zero

;; it returns a list containing 1 or 2 intervals

;; cases for interval 1/[a, b]:

;;   a=0 < b -> [1/b, +inf]

;;   a < 0 < b -> [-inf, 1/a] union [1/b, +inf]

;;   a < 0 = b. -> [-inf, 1/a]

(define (reciprocal-interval x)
  (let ((a (lower-bound x))
  (b (upper-bound x)))
    (cond
      ((not (contains-zero? x))
        (list (make-interval (/ 1.0 b) (/ 1.0 a))))
      ((and (equals-zero? a) (equals-zero? b)) (error "Dividing by zero interval" x))
      ((equals-zero? a) (list (make-interval (/ 1.0 b) +inf)))
      ((equals-zero? b) (list (make-interval -inf (/ 1.0 a))))
      (else (list
        (make-interval -inf (/ 1.0 a))
        (make-interval (/ 1.0 b) +inf))))))
(define (test-recip)
  (define (display-interval i)
    (display "[ ")
    (display (lower-bound i))
    (display ", ")
    (display (upper-bound i))
    (display " ]"))
  (define (display-list L)
    (if (null? (cdr L))
      (let ()
        (display-interval (car L))
        (newline))
      (let ()
        (display-interval (car L))
        (display ", ")
        (display-list (cdr L)))))
  (define (test-element ia)
    (make-interval 2 5)
    (display "x: ")
    (display-interval ia)
    (newline)
    (display "1/x: ")
    (display-list (reciprocal-interval ia)))
  (define (test-elements L)
    (if (null? (cdr L))
      (test-element (car L))
      (let ()
        (test-element (car L))
        (test-elements (cdr L)))))
  (define intervals
    (list
      (make-interval 2 5)
      (make-interval -5 -2)
      (make-interval -3 5)
      (make-interval 0 5)
      (make-interval -5 0)))
  (test-elements intervals))
; (test-recip)

;; Subtraction

(define (sub-interval x y)
  (make-interval
    (- (lower-bound x) (upper-bound y))
    (- (upper-bound x) (lower-bound y))))
;; power evaluation

;; It seems like you could reduce the dependency problem for natural number powers

;; although this only helps for even powers over domains that contain zero

;; otherwise the results are the same as using regular interval multiplication

(define (pow-interval x n)
  (if (even? n)
    (make-interval
      (expt (absmin x) n)
      (expt (absmax x) n))
    (make-interval
      (expt (lower-bound x) n)
      (expt (upper-bound x) n))))
;; Comparisons

;; Note that not all cases are covered by this

(define (lt-interval? x y)
  (< (upper-bound x) (lower-bound y)))
(define (equal-interval? x y)
  (and
    (equals-zero? (abs (- (lower-bound x) (lower-bound y))))
    (equals-zero? (abs (- (upper-bound x) (upper-bound y))))))
(define (gt-interval? x y)
  (> (lower-bound x) (upper-bound y)))
;; Set Operations

(define (intersect-interval x y)
  (let ((max-low (max (lower-bound x) (lower-bound y)))
  (min-high (min (upper-bound x) (upper-bound y))))
    (if (> max-low min-high)
      (make-interval 0 0)
      (make-interval max-low min-high))))
;; Intersect lists of intervals

;; not a good implementation, could be greatly improved

(define (merge-interval-lists X Y)
  (define (iter-y x L)
    (if (null? L)
      (list)
      (let ((merged-interval (intersect-interval x (car L))))
        (if (not (zero-interval? merged-interval))
          (cons merged-interval (iter-y x (cdr L)))
          (iter-y x (cdr L))))))
  (define (iter-x L)
    (if (null? L)
      (list)
      (let ((result (iter-y (car L) Y)))
        (append result (iter-x (cdr L))))))
  (iter-x X))
(define (test-merge)
  (define (display-interval i)
    (display "[ ")
    (display (lower-bound i))
    (display ", ")
    (display (upper-bound i))
    (display " ]"))
  (define (display-list L)
    (if (null? L)
      (let ()
        (display "nil")
        (newline))
      (if (null? (cdr L))
        (let ()
          (display-interval (car L))
          (newline))
        (let ()
          (display-interval (car L))
          (display ", ")
          (display-list (cdr L))))))
  (define X1
    (list (make-interval 2 5) (make-interval -5 -2)))
  (define X2
    (list (make-interval -10 10)))
  (define X3
    (list (make-interval -1 1)))
  (define X4
    (list (make-interval -4 -1) (make-interval 1 4)))
  (define (test-pair X1 X2)
    (display "L1 and L2: ")
    (newline)
    (display-list X1)
    (display-list X2)
    (display "Merged -> ")
    (display-list (merge-interval-lists X1 X2)))
  (test-pair X1 X2)
  (test-pair X1 X3)
  (test-pair X1 X4))
; (test-merge)

;; Interval Newtons method

;; Section 1.6 of the linked paper

;; using intervals, if xi is the interval at iteration i, and 

;; ci is the center point of that interval, then we have the newton iteration

;; x{i+1} = xi intersected with { ci - f(ci)/fprime(xi) }

(define (interval-newtons-method f fprime x))
(define (f x)
  (* (- x 1) (+ x 1)))
(define (fprime X)
  (make-interval
    (* 2 (lower-bound X))
    (* 2 (upper-bound X))))
;; Newton eval accepts and returns a list of intervals

;; since if the interval contains zero it could be disjoint

(define (newton-eval-calc c fc rfpX)
  (sub-interval
    (make-interval c c)
    (mul-interval (make-interval fc fc) rfpX)))
(define (newton-eval-interval X)
  (let ((c (midpoint X))
  (fc (f (midpoint X)))
  (rfpX (reciprocal-interval (fprime X))))
    (define (iter L)
      (if (null? L)
        L
        (cons (newton-eval-calc c fc (car L)) (iter (cdr L)))))
    (iter rfpX)))
(define (newton-eval X)
  (define (iter L)
    (if (null? L)
      (list)
      (append
        (merge-interval-lists (list (car L)) (newton-eval-interval (car L)))
        (newton-eval (cdr L)))))
  (iter X))
(define X0
  (list (make-interval -4 4)))
(define X1 (newton-eval X0))
X1
(define X2 (newton-eval X1))
X2
(define X3 (newton-eval X2))
X3
(define X4 (newton-eval X3))
X4
(define X5 (newton-eval X4))
X5
(define X6 (newton-eval X5))
X6
(intersect-interval (make-interval -2 4) (make-interval 0 1))
(intersect-interval (make-interval -2 4) (make-interval 5 6))
(intersect-interval (make-interval -2 4) (make-interval 0 6))
(intersect-interval (make-interval -2 4) (make-interval -4 6))
;; 2.9 Interval width

; given intervals 

; a from al to au and 

; b from bl to bu

; a+b = (al+bl, au+bu)

; so width(a+b) = au+bu-al-bl

;                       = width(a) + width(b)

;

; width (a-b) = au-bl-al+bu

;                  = width(a) + width(b)

;

;; 2.13 Approximate product tolerance

; let i = ci +/- ci*pi and j = cj +/- cj*pj

; and assuming ci, cj > 0

; and 0 < pi, pj < 1

; i*j = ci*cj ( 1 - pi - pj + pipj,  1 + pi + pj + pipj)

; so the new center is

; center(i*j) = cicj(1+pipj)

; width(i*j) = ci*cj (pi+pj)

; and the new tolerance is

; tolerance(i*j) = (pi+pj)/(1+pipj)

; note: this isnt approximate.  Maybe they were looking for something else?

; maybe the linearization by dropping the quadratic term?

;; 2.14 Different results from equivalent expressions

; par1 = r1r2/(r1+r2)

; par2 = 1/(1/r1+1/r2)

(define (par1 x y)
  (div-interval (mul-interval x y) (add-interval x y)))
(define (par2 x y)
  (let ((one (make-interval 1 1)))
    (div-interval
      one
      (add-interval (div-interval one x) (div-interval one y)))))
(define (test-par)
  (define ia
    (make-midpoint-percent 5 10))
  (define ib
    (make-midpoint-percent 2 15))
  (define ic
    (make-interval -1.5 1.5))
  (define id
    (make-midpoint-percent -2 15))
  (define (display-interval i)
    (display (midpoint i))
    (display " +/- ")
    (display (percent i))
    (newline))
  (define (display-interval-bounds i)
    (display "[ ")
    (display (lower-bound i))
    (display ", ")
    (display (upper-bound i))
    (display " ]")
    (newline))
  (define (display-value v)
    (display v)
    (newline))
  (display "ia: ")
  (display-interval ia)
  (display "ib: ")
  (display-interval ib)
  (display "ic: ")
  (display-interval-bounds ic)
  (display "id: ")
  (display-interval id)
  (display "ia=ia?: ")
  (display-value (equal-interval? ia ia))
  (display "ia=ib?: ")
  (display-value (equal-interval? ia ib))
  (display "ia<ib?: ")
  (display-value (lt-interval? ia ib))
  (display "ib<ia?: ")
  (display-value (lt-interval? ib ia))
  (display "ia>ib?: ")
  (display-value (gt-interval? ia ib))
  (display "ib>ia?: ")
  (display-value (gt-interval? ib ia))
  (display "absmin ic: ")
  (display-value (absmin ic))
  (display "absmin ia: ")
  (display-value (absmin ia))
  (display "absmax ia: ")
  (display-value (absmax ia))
  (display "contains-zero? ia: ")
  (display-value (contains-zero? ia))
  (display "contains-zero? ic: ")
  (display-value (contains-zero? ic))
  (display "par1(ia,ia): ")
  (display-interval (par1 ia ia))
  (display "par2(ia,ia): ")
  (display-interval (par2 ia ia))
  (display "par1(ia,ib): ")
  (display-interval (par1 ia ib))
  (display "par2(ia,ib): ")
  (display-interval (par2 ia ib))
  (display "ia*ia: ")
  (display-interval (mul-interval ia ia))
  (display "ia^2: ")
  (display-interval (pow-interval ia 2))
  (display "ia*ia*ia: ")
  (display-interval (mul-interval (mul-interval ia ia) ia))
  (display "ia^3: ")
  (display-interval (pow-interval ia 3))
  (display "ic*ic: ")
  (display-interval-bounds (mul-interval ic ic))
  (display "ic^2: ")
  (display-interval-bounds (pow-interval ic 2))
  (display "id*id: ")
  (display-interval-bounds (mul-interval id id))
  (display "id^2: ")
  (display-interval-bounds (pow-interval id 2))
  (display "ia+ia-ia: ")
  (display-interval (sub-interval (add-interval ia ia) ia)))
; (test-par)

;;;

;;;

;;;

;;;

;;;

;;;
