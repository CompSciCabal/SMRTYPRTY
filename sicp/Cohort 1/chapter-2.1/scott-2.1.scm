;; Definitions from the text

(define (make-rat n d)
  (cons n d))
(define (numer q)
  (car q))
(define (denom q)
  (cdr q))
(define (display-rat q)
  (display (numer q))
  (display "/")
  (display (denom q))
  (newline))
; (display-rat (make-rat 5 8))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))
(gcd 60 33)
(gcd (- 33) (- 21))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(make-rat 50 80)
;; 2.1 better make-rat

(define (make-rat n d)
  (define (sign a)
    (if (positive? a)
      1
      -1))
  (let ((g (gcd (abs n) (abs d)))
  (s (* (sign n) (sign d)))
  (n (abs n))
  (d (abs d)))
    (cons (* s (/ n g)) (/ d g))))
(make-rat -5 -6)
(make-rat -5 6)
(make-rat 5 -6)
(make-rat 17 -8)
(make-rat -17 8)
;; 2.2 line segments

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (display-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")")
  (newline))
(define (combine-points binary-op p q)
  (make-point
    (binary-op (x-point p) (x-point q))
    (binary-op (y-point p) (y-point q))))
(define (scale-point s p)
  (make-point (* s (x-point p)) (* s (y-point p))))
; (display-point (make-point 1.4 4.5))

(define (make-segment x y)
  (cons x y))
(define (start-segment p)
  (car p))
(define (end-segment p)
  (cdr p))
(define (midpoint-segment s)
  (scale-point
    0.5
    (combine-points + (start-segment s) (end-segment s))))
(define seg-1
  (make-segment (make-point 5 7) (make-point -2 9)))
seg-1
(midpoint-segment seg-1)
;; 2.3 rectangles

;; first representation, bottom-left -top-right

(define (make-rect bottom-left top-right)
  (cons bottom-left top-right))
(define (bottom-left-rect r)
  (car r))
(define (top-right-rect r)
  (cdr r))
(define (width-rect r)
  (x-point
    (combine-points - (top-right-rect r) (bottom-left-rect r))))
(define (height-rect r)
  (y-point
    (combine-points - (top-right-rect r) (bottom-left-rect r))))
(define (perimeter-rect r)
  (* 2 (+ (width-rect r) (height-rect r))))
(define (area-rect r)
  (* (width-rect r) (height-rect r)))
(define rect-1
  (make-rect (make-point 3 4) (make-point 5 9)))
rect-1
(area-rect rect-1)
(perimeter-rect rect-1)
;; second representation bottom-left - width-height

(define (make-rect bottom-left top-right)
  (cons bottom-left (combine-points - top-right bottom-left)))
(define (bottom-left-rect r)
  (car r))
(define (top-right-rect r)
  (combine-points + (car r) (cdr r)))
(define (width-rect r)
  (x-point (cdr r)))
(define (height-rect r)
  (y-point (cdr r)))
;; 2.4 alternative pair implementation

(define (cons-alt x y)
  (lambda (m)
    (m x y)))
(define (car-alt p)
  (p (lambda (x y)
    x)))
(define (cdr-alt p)
  (p (lambda (x y)
    y)))
(define sm
  (cons-alt "super" "man"))
sm
(car-alt sm)
(cdr-alt sm)
;; 2.5 integer pairs representing a pair a,b as 2^a3^b

(define (divide-int a b)
  (define (iter acc i)
    (let ((r (remainder acc b)))
      (if (> r 0)
        i
        (iter (/ acc b) (+ i 1)))))
  (iter a 0))
(divide-int 12 3)
(define (expt-int a b)
  (define (iter i r)
    (if (= i b)
      r
      (iter (+ i 1) (* r a))))
  (iter 0 1))
(expt-int 2 5)
(integer? (expt-int 2 32))
(define (cons-int a b)
  (* (expt-int 2 a) (expt-int 3 b)))
(define (car-int p)
  (divide-int p 2))
(define (cdr-int p)
  (divide-int p 3))
(define int-pair-1
  (cons-int 5 4))
(integer? int-pair-1)
(car-int int-pair-1)
(cdr-int int-pair-1)
;; 2.6 Church Numerals

(define zero
  (lambda (f)
    (lambda (x)
      x)))
(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))
(define one
  (lambda (f)
    (lambda (x)
      (f x))))
(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))
;; church numerals answer the question, can we use numbers without having 

;; numbers in our system?

;; defining addition and multiplication using church numerals

; for addition you replace the zero of a with b

(define (add-church a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))
(define (inc a)
  (+ 1 a))
((one inc) 0)
(define three
  (add-church one two))
((three inc) 0)
;; multiplication

; for multiplication you replace the function of a with the function of b

(define (mul-church a b)
  (lambda (f)
    (lambda (x)
      ((a (b f)) x))))
(define six
  (mul-church two three))
((six inc) 0)

;;; Extended Example 2.1.4 

(define (add-interval x y)
  (make-interval
    (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
  (p2 (* (lower-bound x) (upper-bound y)))
  (p3 (* (upper-bound x) (lower-bound y)))
  (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x (make-interval
    (/ 1.0 (upper-bound y))
    (/ 1.0 (lower-bound y)))))
;;; Interval Arithmetic

;; 2.7 Defining the constructor and selectors

(define (make-interval a b)
  (cons a b))
(define (upper-bound i)
  (cdr i))
(define (lower-bound i)
  (car i))
(define i-1
  (make-interval 1.0 2.0))
(define i-2
  (make-interval 3.0 5.0))
i-1
i-2
(add-interval i-1 i-2)
(mul-interval i-1 i-2)
(div-interval i-1 i-2)
;; 2.8 interval subtraction

(define (sub-interval x y)
  (make-interval
    (- (lower-bound x) (upper-bound y))
    (- (upper-bound x) (lower-bound y))))
(sub-interval i-1 i-2)
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
; a*b = (min(albl, albu, aubl, aubu), max(albl, albu, aubl, aubu))
; does function f exist, s.t. width(a*b) = f(width(a), width(b))?
; given intervals with the same width,
; c = (-2, 2), d = (0,4)
; and another interval e = (1,2)
; width(c*e) = 16
; width(d*e) = 8
; but c and d have the same width, so if the function f existed it 
; would map 1 value to 2, which violates the definition of a function
; ? look this up later i believe a function needs 1 output per unique input values

(define (width-interval x)
  (- (upper-bound x) (lower-bound x)))
(define (equals-zero? x)
  (let ((tolerance 1e-6))
    (< (abs x) tolerance)))
(equals-zero? 1)
(equals-zero? 1e-7)
(equals-zero? -1e-7)
;; 2.10 divide by zero

(define (div-interval x y)
  (if (equals-zero? (width-interval y))
    (error "Dividing by zero span interval" y)
    (mul-interval x (make-interval
      (/ 1.0 (upper-bound y))
      (/ 1.0 (lower-bound y))))))
;(div-interval (make-interval 1 2) (make-interval 1 1))

;(div-interval (make-interval 1 2) (make-interval 0 1))

(div-interval (make-interval 1 2) (make-interval 1e-5 2e-5))
(div-interval (make-interval 1 2) (make-interval 1e-6 2e-6))
;(div-interval (make-interval 1 2) (make-interval 1.5e-6 2e-6))

;; 2.11 Multiplication Optimization

; im not convinced this is an optimization
; its more complex, harder to maintain, harder to test,
; and you only get a gain in performance if mults are truly the bottleneck

(define (mul-interval x y)
  (let ((xl (lower-bound x))
  (xu (upper-bound x))
  (yl (lower-bound y))
  (yu (upper-bound y)))
    (define (mul-internal l1 l2 u1 u2)
      (make-interval (* l1 l2) (* u1 u2)))
    (define (case-pppp)
      (mul-internal xl yl xu yu))
    (define (case-ppnp)
      (mul-internal xu yl xu yu))
    (define (case-ppnn)
      (mul-internal xu yl xl yu))
    (define (case-nppp)
      (mul-internal xl yu xu yu))
    (define (case-npnp)
      (make-interval
        (min (* xl yu) (* yl xu))
        (max (* xl yl) (* xu yu))))
    (define (case-npnn)
      (mul-internal xu yl xl yl))
    (define (case-nnpp)
      (mul-internal xl yu xu yl))
    (define (case-nnnp)
      (mul-internal xl yu xl yl))
    (define (case-nnnn)
      (mul-internal xu yu xl yl))
    ; note, this approach makes zero negative
    
    (let ((pxl (positive? xl))
    (pxu (positive? xu))
    (pyl (positive? yl))
    (pyu (positive? yu)))
      (cond
        ((and pxl pxu pyl pyu) (case-pppp))
        ((and pxl pxu (not pyl) pyu) (case-ppnp))
        ((and pxl pxu (not pyl) (not pyu)) (case-ppnn))
        ((and (not pxl) pxu pyl pyu) (case-nppp))
        ((and (not pxl) pxu (not pyl) pyu) (case-npnp))
        ((and (not pxl) pxu (not pyl) (not pyu)) (case-npnn))
        ((and (not pxl) (not pxu) pyl pyu) (case-nnpp))
        ((and (not pxl) (not pxu) (not pyl) pyu) (case-nnnp))
        ((and
          (not pxl)
          (not pxu)
          (not pyl)
          (not pyu))
         (case-nnnn))))))
(define nn
  (make-interval -3 -2))
(define np
  (make-interval -3 2))
(define pp
  (make-interval 2 3))
(mul-interval pp pp)
(mul-interval pp np)
(mul-interval pp nn)
(mul-interval np pp)
(mul-interval np np)
(mul-interval np nn)
(mul-interval nn pp)
(mul-interval nn np)
(mul-interval nn nn)
; the above testing found 3 bugs in the new implementation after i first wrote it

; zero in the earlier simple one...

;; 2.12 centre width by percent

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (upper-bound i) (lower-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))
(center i-2)
(width i-2)
(define i-3
  (make-center-width 5.0 0.25))
i-3
(define (make-center-percent c p)
  (let ((w (* c p 1e-2)))
    (make-interval (- c w) (+ c w))))
(define (percent i)
  (* (/ (width i) (center i)) 100))
(define i-4
  (make-center-percent 2 10))
i-4
(percent i-4)
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
    (make-center-percent 5 10))
  (define ib
    (make-center-percent 2 15))
  (define (display-interval i)
    (display (center i))
    (display " +/- ")
    (display (percent i))
    (newline))
  (display "ia: ")
  (display-interval ia)
  (display "ib: ")
  (display-interval ib)
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
  (display "ia+ia-ia: ")
  (display-interval (sub-interval (add-interval ia ia) ia)))
(test-par)
;; 2.15 good interval programs
; she is right that par2 is a better program than par1 for
; computing resistances.  the issue is that each interval is treated
; independently, even if they are the same variable.
; for instance above i did ia*ia, as expected the two 10%
; uncertainties become close to 20%, but it shouldn't, since the value
; will be identical.
; For squaring, the tolerance should remain at exactly 10%, since only the magnitude 
; of the value and not the uncertainty is modified.
; Though she is right, it seems like maybe the interval system should have some
; mechanism for detecting this and at least issuing a warning.

;; 2.16
; Adding and subtracting intervals has the same problem. 
; In 2.14 I tried ia+ia-ia.  This gives back the same center value,
; with tripled uncertainty.
; If each interval is treated as a random variable, we could solve the problem in a 
; different way.
; I know that this is exactly what is done in when computing the timing of a clock cycle signal through a chip in semi-conductor design.
; Lightning talk maybe ;)
; Essentially, each component adds a variable amount to the delay of the signal, 
; the standard static timing analysis (STA) uses the best-case/worst-case approach to combine values like in Alyssa's system.
; Newer statistical approaches instead store random distributions for each contribution.
; But combining random variables is much more complex,  http://en.wikipedia.org/wiki/Relationships_among_probability_distributions
; At this point, i first tried to implement intervals, using random variables.
; I didn't succeed... 
; Then I researched interval arithmetic more.
; I hadn't understood the problem originally, its used when you
; don't know anything about the distributions but you still want to do
; computations that give correct, if not precise results.
; intlab is matlab software to do lots of stuff with intervals, http://www.ti3.tuhh.de/rump/intlab/ 

