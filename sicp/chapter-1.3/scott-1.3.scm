;;;; exercises from section 1.3

;;; Basic Functions

(define (inc i)
  (+ 1 i))
(define (identity x) x)
(define (square x)
  (* x x))
(define (cube x)
  (* x x x))
(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))
(sum identity 1 inc 10)
(sum square 1 inc 10)
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))
;;; 1.29 Simpson's Rule: Integral of f from a to b

;;; original Integral example

(integral cube 0 1 0.01)
; result 0.2499875

(integral cube 0 1 0.001)
; result 0.249999875

;;; Simpson's Rule

(define (integral-simpson f a b n)
  (define (y k)
    (f (+ a (* k h))))
  (define (v k)
    (if (= (remainder k 2) 0)
      (* 2 (y k))
      (* 4 (y k))))
  (define h
    (/ (- b a) n))
  (*
    (/ h 3)
    (+ (y 0) (sum v 1 inc (- n 1)) (y n))))
(integral-simpson cube 0 1 10)
; result 0.25

(integral-simpson cube 0 1 100)
; result 0.25

(integral-simpson cube 0 1 1000)
; result 0.25

; Simpsons seems exact for this example.  

; Wikipedia says this should be h^4 so 10 should only give 10^-4 

; im suspecting if you do the math maybe its better than that for low order polynomials.

;;; 1.30 Iterative sum implementation

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))
(sum identity 1 inc 10)
;;; 1.32 a

;;; Implement product, factorial and approximate pi using the formula.

;;; pi/4 = (2*4*4*6*6*8...)/(3*3*5*5*7...)

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))
;;; this is a factorial

(product identity 1 inc 4)
(define (factorial n)
  (product identity 1 inc n))
(factorial 4)
;;; as an aside, i wanted to avoid dividing 2 big numbers, so i tried to use rational numbers

;;; first i found the cons type which is a dotted pair

(define pair (cons 2 3))
(/ (car pair) (cdr pair))
;;; then i saw you could implement the pair using functions. 

;;; ive done it by copying this is still a bit confusing to me.

(define (rational-number p q)
  (lambda (f)
    (f p q)))
(rational-number 1 2)
(define (numerator r)
  (r (lambda (p q)
    p)))
(numerator (rational-number 3 5))
(define (denominator r)
  (r (lambda (p q)
    q)))
(denominator (rational-number 3 5))
;;; but now syntax highlighting shows that lisp already supports rational numbers :)

;;; oh well ill use mine for now

(define (pi-wallis n)
  (define a
    (rational-number 2 3))
  (define (term a)
    (/ (numerator a) (denominator a)))
  (define (next a)
    (define p (numerator a))
    (define q (denominator a))
    (if (< p q)
      (rational-number (+ p 2) q)
      (rational-number p (+ q 2))))
  (define (iter a r i)
    (if (> i n)
      r
      (iter (next a) (* r (term a)) (+ i 1))))
  (* 4 (iter a 1 1))
  ;; (product term a next b).
  
  ;; how to compare rational numbers?  i.e can i add an overload for less than?
  
  ;; that is the only stumbling block to using the generic product
  )
(pi-wallis 1)
(pi-wallis 2)
(pi-wallis 3)
(pi-wallis 50)
(pi-wallis 500)
(pi-wallis 5000)
;; Seems like it is very slowly converging... 5000 only gave 4 digits

;;; 1.31b recursive product

(define (product-rec term a next b)
  (if (> a b)
    1
    (* (term a) (product-rec term (next a) next b))))
(product-rec identity 1 inc 6)
;;; 1.32 generic accumulate

;;; a) recursive

;;; b) iterative

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner
      (term a)
      (accumulate-rec combiner null-value term (next a) next b))))
(accumulate-rec + 0 identity 1 inc 10)
(define (accumulate combiner null-value term a next b)
  (define (iter a r)
    (if (<= a b)
      (iter (next a) (combiner (term a) r))
      r))
  (iter a null-value))
(accumulate * 1 identity 1 inc 6)
(define (product term a next b)
  (accumulate * 1 term a next b))
(product identity 1 inc 6)
(define (sum term a next b)
  (accumulate + 0 term a next b))
(sum identity 1 inc 10)
;;; 1.33

(define (filtered-accumulate combiner null-value pred term a next b)
  (define (iter a r)
    (cond
      ((> a b) r)
      ((pred a) (iter (next a) (combiner (term a) r)))
      (else (iter (next a) r))))
  (iter a null-value))
(filtered-accumulate + 0 even? identity 1 inc 10)
;;; a) Finding primes, copying vlads prime finder (also in the text)

; Deterministic primality test

; Time complexity: Θ(√n)

(define (smallest-divisor n)
  (find-divisor n 2))
(define (square x)
  (* x x))
(define (divides? a b)
  (= (remainder b a) 0))
(define (find-divisor n test-divisor)
  (define (next td)
    (+ td 1))
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (next test-divisor)))))
(define (prime? n)
  (and (> n 1) (= (smallest-divisor n) n)))
;;; prime squares sum

(filtered-accumulate + 0 prime? square 1 inc 10)
;;; b) product of all relatively prime integers

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))
(define (relatively-prime? a b)
  (= (gcd a b) 1))
(define (func-1-33-b n)
  (define (pred i)
    (relatively-prime? i n))
  (filtered-accumulate * 1 pred identity 1 inc (- n 1)))
(func-1-33-b 6)
(func-1-33-b 7)
;;; 1.34

(define (f g)
  (g 2))
(f square)
;;; (f f)

;;; This doesn't work since when you evaluate it,

;;; you get (2 2) and 2 is not a valid operator name
;;; 1.36 Display the guesses for finding a fixed point of x^x = 1000

(display "s1 :")
(display 4)
(newline)
(define (display-guess i guess)
  (display "Guess ")
  (display i)
  (display ": ")
  (display guess)
  (newline))
; (message "guess: %d" 127)

(define (fixed-point f first-guess)
  (define (close-enough? x y)
    (< (abs (- x y)) tolerance))
  (define (try i guess)
    (let ((next (f guess)))
      (display-guess i next)
      (if (close-enough? guess next)
        next
        (try (+ i 1) next))))
  (display-guess 0 first-guess)
  (try 1 first-guess))
(fixed-point cube 0.95)
; using it with the transformation x <- log(1000)/log(x)

(fixed-point (lambda (x)
  (/ (log 1000) (log x)))
             3)
(define (average a b)
  (/ (+ a b) 2))
; 38 iterations without averaging

(fixed-point (lambda (x)
  (average x (/ (log 1000) (log x))))
             3)
; 9 iterations with averaging

;;; 1.37 Continued fractions

; a) recursive formulation

(define (cont-frac-rec n d k)
  (define (iter i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))
(define (golden-apx cont-frac k)
  (/ 1 (cont-frac (lambda (i)
    1.0)
             (lambda (i)
               1.0)
             k)))
(define (test-golden cont-frac k)
  (define (iter i)
    (if (> i k)
      #t
      (let ((result (golden-apx cont-frac i)))
        (display i)
        (display ": ")
        (display result)
        (newline)
        (iter (+ i 1)))))
  (iter 1))
(test-golden cont-frac-rec 20)
; it took 12 iterations to get 4 digits accuracy of phi

;;; 1.37b an iterative approach, simple one is to start at the bottom and iterate up

(define (cont-frac-iter n d k)
  (define (iter i r)
    (if (= 0 i)
      r
      (iter (- i 1) (/ (n i) (+ (d i) r)))))
  (iter k 0))
(test-golden cont-frac-iter 20)
;;; the downside to both of these approaches, as andrey brought up 

; is that if you compute the kth term and it isnt precise enough

; you have to recompute all of it in order to get better accuracy.

; Alternatively, the recursive approach could memoize everything.

; Another approach, is to use rational functions to express each level of nesting.

; A rational function is any function F(x) = (ax+b)/(cx+d)

; a nice property of them is that when you compose them

; you always get back a rational function

; so for a linear continued fraction, you can express any depth of fraction as a single rational function

; to this we work out an expression for computing the new coefficient values

; after a function composition

; given F(x) = (ax+b)/(cx+d) and G(x) = (ex+f)/(gx+h), then 

; H(x) = F(G(x)) = (ix+j)/(kx+l) has coefficients:

; i = ae+bg

; j = af+bh

; k = ce+dg

; l = cf+dh

(define (rational-function a b c d)
  (lambda (f)
    (f a b c d)))
(define (rf-display r)
  (r (lambda (a b c d)
    (display "( ")
    (display a)
    (display ", ")
    (display b)
    (display ", ")
    (display c)
    (display ", ")
    (display d)
    (display " )")
    (newline))))
(define (rf-eval r x)
  (r
    (lambda (a b c d)
      (/ (+ (* a x) b) (+ (* c x) d)))))
;;; This composition function doesnt normalize the coefficients  for clarity

; usually you would keep their values within a range for accuracy

(define (rf-compose F G)
  (F
    (lambda (a b c d)
      (G (lambda (e f g h)
        (rational-function
          (+ (* a e) (* b g))
          (+ (* a f) (* b h))
          (+ (* c e) (* d g))
          (+ (* c f) (* d h))))))))
(define (rf-normalize r)
  (r
    (lambda (a b c d)
      (let ((s (sqrt
        (+ (* a a) (* b b) (* c c) (* d d)))))
        (rational-function (/ a s) (/ b s) (/ c s) (/ d s))))))
(define f1
  (rational-function 1 2 3 4))
(define f2
  (rational-function 5 6 7 8))
(rf-display f1)
(rf-display f2)
(define f12
  (rf-compose f1 f2))
(rf-display f12)
(define rf1 (rf-normalize f1))
(rf-display rf1)
;;; This function gives you a rational function back that is equivalent to the first k

; nestings of the fraction

(define (cont-frac-rf n d k)
  (define (next i)
    (rational-function 0.0 (n i) 1.0 (d i)))
  (define (iter i F)
    (if (> i k)
      F
      (iter (+ i 1) (rf-compose F (next i)))))
  (iter 1 (rational-function 1.0 0.0 0.0 1.0)))
;;; uses the function to compute the kth truncation

(define (cont-frac-rf-eval n d k)
  (let ((F (cont-frac-rf n d k)))
    (rf-eval F 0.0)))
(cont-frac-rf-eval (lambda (i)
  1.0)
                   (lambda (i)
                     1.0)
                   5)
(test-golden cont-frac-rf-eval 20)
;;; what if we have the 11th and we want the twelfth?

(define cf11
  (cont-frac-rf (lambda (i)
    1.0)
                (lambda (i)
                  1.0)
                11))
; in our case n and d are constant, otherwise they need to be offset

(define cf12
  (rf-compose cf11 (cont-frac-rf (lambda (i)
    1.0)
                (lambda (i)
                  1.0)
                1)))
(/ 1 (rf-eval cf11 0.0))
(/ 1 (rf-eval cf12 0.0))
;;; or maybe more useful for this constant case, if we want 22 terms accuracy

; just compose cf11 with cf11

(define cf22
  (rf-compose cf11 cf11))
(/ 1 (rf-eval cf22 0.0))
;;; 1.38 approximating e

(define (one i) 1.0)
(define (euler-seq i)
  (let ((j (+ i 1)))
    (if (= 0 (remainder j 3))
      (* 2 (/ j 3))
      1.0)))
(define (display-seq f i k)
  (display (f i))
  (display ", ")
  (if (= i k)
    #t
    (display-seq f (+ i 1) k)))
(display-seq one 1 20)
(display-seq euler-seq 1 20)
(define (e2-apx-rf k)
  (cont-frac-rf one euler-seq k))
(define e2-apx (e2-apx-rf 20))
(define (e-apx)
  (+ (rf-eval e2-apx 0.0) 2))
(e-apx)
;;; 1.39 approximating tan

; cant use the rational functions any more, since it isnt linear

(define (tan-apx x k)
  (define (n i)
    (* x x))
  (define (d i)
    (- (* 2 i) 1))
  (define (iter i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (- (d i) (iter (+ i 1))))))
  (/ (iter 1) x))
(tan-apx 1 10)
(tan 1)
;;; actually thats wrong we can, just let y=x^2 and then divide by -x at the end

(define (tan-apx-rf x k)
  (define (n i)
    (- (* x x)))
  (define (d i)
    (- (* 2 i) 1))
  (/ (rf-eval (cont-frac-rf n d k) 0.0) (- x)))
(tan-apx-rf 1 10)
;;;

;;;

;;;

;;;

;;;

;;;

;;;

;;;

;;; these are pushing the last command up to the middle of the screen...




