(require-extension sicp)
(require-extension numbers)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (cube x)
  (* x x x))

;; Exercise 1.29: Simpson’s Rule is a more accurate method of
;; numerical integration than the method illustrated above. Using
;; Simpson’s Rule, the integral of a function f between a and b is
;; approximated as

;; h/3(y0+4y1+2y2+4y3+2y4+⋯+2yn−2+4yn−1+yn)

;; where h=(b−a)/n, for some even integer n, and yk=f(a+kh).
;; (Increasing n increases the accuracy of the approximation.) Define
;; a procedure that takes as arguments f, a, b, and n and returns the
;; value of the integral, computed using Simpson’s Rule. Use your
;; procedure to integrate cube between 0 and 1 (with n=100 and
;; n=1000), and compare the results to those of the integral procedure
;; shown above.

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (yk k)
    (f (+ a (* h k))))
  (define (term k)
    (cond ((or (= k 0) (= k n)) (yk k))
          ((even? k) (* 2 (yk k)))
          (else (* 4 (yk k)))))
  (/ (* h (sum term 0 inc n))
     3.0))

;; Exercise 1.30: The sum procedure above generates a linear
;; recursion. The procedure can be rewritten so that the sum is
;; performed iteratively. Show how to do this by filling in the
;; missing expressions in the following definition:

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;; Exercise 1.31:

;; 1. The sum procedure is only the simplest of a vast number of
;; similar abstractions that can be captured as higher-order
;; procedures. Write an analogous procedure called product that
;; returns the product of the values of a function at points over a
;; given range. Show how to define factorial in terms of product. Also
;; use product to compute approximations to π using the formula

;; π/4 = 2⋅4⋅4⋅6⋅6⋅8/3⋅3⋅5⋅5⋅7⋅7.

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (pi-val n)
  (define (even-term n)
    (if (odd? n)
        (+ n 1)
        (+ n 2)))
  (define (odd-term n)
    (if (odd? n)
        (+ n 2)
        (+ n 1)))
  (define (term a)
    (/ (even-term a) (odd-term a)))
  (* 4.0 (product term 1 inc n)))

;; 2. If your product procedure generates a recursive process, write
;; one that generates an iterative process. If it generates an
;; iterative process, write one that generates a recursive process.

(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursive term (next a) next b))))

(define (factorial-recursive n)
  (product-recursive identity 1 inc n))

;; Exercise 1.32:

;; Show that sum and product (Exercise 1.31) are both special cases of
;; a still more general notion called accumulate that combines a
;; collection of terms, using some general accumulation function:

;; Accumulate takes as arguments the same term and range
;; specifications as sum and product, together with a combiner
;; procedure (of two arguments) that specifies how the current term is
;; to be combined with the accumulation of the preceding terms and a
;; null-value that specifies what base value to use when the terms run
;; out. Write accumulate and show how sum and product can both be
;; defined as simple calls to accumulate.

(define (accumulate-recur combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (accumulate-recur combiner null-value term (next a) next b)
                (term a))))

;; If your accumulate procedure generates a recursive process, write
;; one that generates an iterative process. If it generates an
;; iterative process, write one that generates a recursive process.

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

;; Exercise 1.33: You can obtain an even more general version of
;; accumulate (Exercise 1.32) by introducing the notion of a filter on
;; the terms to be combined. That is, combine only those terms derived
;; from values in the range that satisfy a specified condition. The
;; resulting filtered-accumulate abstraction takes the same arguments
;; as accumulate, together with an additional predicate of one
;; argument that specifies the filter. Write filtered-accumulate as a
;; procedure. Show how to express the following using
;; filtered-accumulate:

(define (filtered-accumulate pred combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((pred a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

;; 1. the sum of the squares of the prime numbers in the interval a to
;; b (assuming that you have a prime? predicate already written)

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum-squares-of-primes a b)
  (define inc2 (compose inc inc))
  (define (next a)
    (if  (or (= a 1) (even? a))
         (inc a)
         (inc2 a)))
  (filtered-accumulate prime? + 0 square a next b))

;; 2. the product of all the positive integers less than n that are
;; relatively prime to n (i.e., all positive integers i<n such that
;; GCD(i,n)=1).

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (sum-of-relative-primes n)
  (define (relatively-prime? i)
    (= 1 (gcd i n)))
  (filtered-accumulate relatively-prime? * 1 identity 1 inc n))

;; Exercise 1.34: Suppose we define the procedure

(define (f g) (g 2))

;; Then we have

;; (f square)
;; 4

;; (f (lambda (z) (* z (+ z 1))))
;; 6

;; What happens if we (perversely) ask the interpreter to evaluate the
;; combination (f f)? Explain.

;; (f f) ;; f is substituted for g in (g 2):
;; (f 2) ;; 2 is substituted for g in (g 2):
;; (2 2) ;; 2 is not a function so error:
;; Error: call of non-procedure: 2

    ;; Call history:

    ;; <syntax>	  (f f)
    ;; <eval>	  (f f)
    ;; <eval>	  [f] (g 2)
;; <eval>	  [f] (g 2)	<--

(define (search f neg-point pos-point)
  (let ((midpoint
         (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond
           ((positive? test-value)
            (search f neg-point midpoint))
           ((negative? test-value)
            (search f midpoint pos-point))
           (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value)
                (positive? b-value))
           (search f a b))
          ((and (negative? b-value)
                (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt-fp x)
  (fixed-point (lambda (y) (/ x y))
               1.0))

(define (sqrt-fp2 x)
  (fixed-point
   (lambda (y) (average y (/ x y)))
   1.0))

;; Exercise 1.35: Show that the golden ratio φ (1.2.2) is a fixed
;; point of the transformation x↦1+1/x, and use this fact to compute φ
;; by means of the fixed-point procedure.

(define (golden-ratio n)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
               1.0))

;; Exercise 1.36: Modify fixed-point so that it prints the sequence of
;; approximations it generates, using the newline and display
;; primitives shown in Exercise 1.22. Then find a solution to x^x=1000
;; by finding a fixed point of x↦log(1000)/log(x). (Use Scheme’s
;; primitive log procedure, which computes natural logarithms.)
;; Compare the number of steps this takes with and without average
;; damping. (Note that you cannot start fixed-point with a guess of 1,
;; as this would cause division by log⁡(1)=0.)

(define (noisy-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (print "trying " guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (noisy-golden-ratio)
  (noisy-fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(define (noisy-golden-ratio-dampened)
  (noisy-fixed-point (lambda (x) (average x (+ 1 (/ 1 x)))) 1.0))

;; #;359> (noisy-golden-ratio)
;; trying 1.0
;; trying 2.0
;; trying 1.5
;; trying 1.66666666666667
;; trying 1.6
;; trying 1.625
;; trying 1.61538461538462
;; trying 1.61904761904762
;; trying 1.61764705882353
;; trying 1.61818181818182
;; trying 1.61797752808989
;; trying 1.61805555555556
;; trying 1.61802575107296
;; trying 1.61803713527851
;; 1.61803278688525

;; #;365> (noisy-golden-ratio-dampened)
;; trying 1.0
;; trying 1.5
;; trying 1.58333333333333
;; trying 1.60745614035088
;; trying 1.61477854766521
;; trying 1.61702925556443
;; trying 1.6177236283488
;; trying 1.61793809348321
;; trying 1.6180043565683
;; trying 1.61802483200585
;; 1.61803115917027

(define (ex-136a n)
  (noisy-fixed-point (lambda (x) (/ (log n) (log x)))
                     2))

(define (ex-136b n)
  (noisy-fixed-point (lambda (x) (average x (/ (log n) (log x))))
                     2))

;; #;398> (ex-136a 1000)
;; trying 2
;; trying 9.96578428466209
;; trying 3.00447220984121
;; trying 6.27919575750716
;; trying 3.75985070240154
;; trying 5.2158437849259
;; trying 4.1822071924014
;; trying 4.82776509834459
;; trying 4.38759338466268
;; trying 4.6712500857639
;; trying 4.48140361689505
;; trying 4.6053657460929
;; trying 4.52308496787189
;; trying 4.57711468204734
;; trying 4.54138248015145
;; trying 4.56490324523083
;; trying 4.54937267930334
;; trying 4.55960649191329
;; trying 4.55285387578827
;; trying 4.55730552974826
;; trying 4.55436906443618
;; trying 4.556305311533
;; trying 4.55502826357355
;; trying 4.55587039670285
;; trying 4.55531500119208
;; trying 4.55568126354333
;; trying 4.55543971573685
;; trying 4.55559900999829
;; trying 4.55549395753139
;; trying 4.55556323729288
;; trying 4.55551754841765
;; trying 4.5555476793064
;; trying 4.55552780851625
;; trying 4.55554091291796
;; 4.55553227080365
;; #;436> (ex-136b 1000)
;; trying 2
;; trying 5.98289214233104
;; trying 4.92216872130834
;; trying 4.62822431819546
;; trying 4.56834651313624
;; trying 4.5577305909237
;; trying 4.55590980904513
;; trying 4.55559941161062
;; trying 4.55554655214737
;; 4.55553755199982
;; #;443>

;; Exercise 1.37:

;; An infinite continued fraction is an expression of the form

;; f=N1D1+N2D2+N3D3+….

;; As an example, one can show that the infinite continued fraction
;; expansion with the Ni and the Di all equal to 1 produces 1/φ1/φ,
;; where φ is the golden ratio (described in 1.2.2). One way to
;; approximate an infinite continued fraction is to truncate the
;; expansion after a given number of terms. Such a truncation—a
;; so-called finite continued fraction k-term finite continued
;; fraction—has the form N1D1+N2⋱+NkDk. Suppose that n and d are
;; procedures of one argument (the term index i) that return the Ni
;; and Di of the terms of the continued fraction. Define a procedure
;; cont-frac such that evaluating (cont-frac n d k) computes the value
;; of the k-term finite continued fraction. Check your procedure by
;; approximating 1/φ using

(define (cont-frac n d k)
  (define (recur x)
    (if (< x 0)
        0
        (/ (n k) (+ (d k) (recur (- x 1))))))
  (recur k))

(define (cont-frac-test k)
  (define (test n)
    (when (< n k)
      (print "cont-frac " (inc n) " " (cont-frac
                                       (lambda (i) 1.0)
                                       (lambda (i) 1.0)
                                       n))
      (test (inc n))))
  (test 0))

;; for successive values of k. How large must you make k in order to
;; get an approximation that is accurate to 4 decimal places?

;; 11

;; Also, maximum accuracy reached at step 37 on my computer with
;; chicken:

;; cont-frac 1 1.0
;; cont-frac 2 0.5
;; cont-frac 3 0.666666666666667
;; cont-frac 4 0.6
;; cont-frac 5 0.625
;; cont-frac 6 0.615384615384615
;; cont-frac 7 0.619047619047619
;; cont-frac 8 0.617647058823529
;; cont-frac 9 0.618181818181818
;; cont-frac 10 0.617977528089888
;; cont-frac 11 0.618055555555556
;; cont-frac 12 0.618025751072961
;; cont-frac 13 0.618037135278515
;; cont-frac 14 0.618032786885246
;; cont-frac 15 0.618034447821682
;; cont-frac 16 0.618033813400125
;; cont-frac 17 0.618034055727554
;; cont-frac 18 0.618033963166706
;; cont-frac 19 0.618033998521803
;; cont-frac 20 0.618033985017358
;; cont-frac 21 0.618033990175597
;; cont-frac 22 0.618033988205325
;; cont-frac 23 0.618033988957902
;; cont-frac 24 0.618033988670443
;; cont-frac 25 0.618033988780243
;; cont-frac 26 0.618033988738303
;; cont-frac 27 0.618033988754322
;; cont-frac 28 0.618033988748204
;; cont-frac 29 0.618033988750541
;; cont-frac 30 0.618033988749648
;; cont-frac 31 0.618033988749989
;; cont-frac 32 0.618033988749859
;; cont-frac 33 0.618033988749909
;; cont-frac 34 0.61803398874989
;; cont-frac 35 0.618033988749897
;; cont-frac 36 0.618033988749894
;; cont-frac 37 0.618033988749895
;; cont-frac 38 0.618033988749895


;; If your cont-frac procedure generates a recursive process, write
;; one that generates an iterative process. If it generates an
;; iterative process, write one that generates a recursive process.

(define (cont-frac-iter n d k)
  (define (iter acc i)
    (if (< i 0)
        acc
        (iter (/ (n k)
                 (+ (d k) acc))
              (- i 1))))
  (iter 0 k))

(define (cont-frac-iter-test k)
  (define (test n)
    (when (< n k)
      (print "cont-frac-iter " (inc n) " " (cont-frac-iter
                                            (lambda (i) 1.0)
                                            (lambda (i) 1.0)
                                            n))
      (test (inc n))))
  (test 0))

;; Exercise 1.38: In 1737, the Swiss mathematician Leonhard Euler
;; published a memoir De Fractionibus Continuis, which included a
;; continued fraction expansion for e−2, where e is the base of the
;; natural logarithms. In this fraction, the Ni are all 1, and the Di
;; are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, …. Write a
;; program that uses your cont-frac procedure from Exercise 1.37 to
;; approximate e, based on Euler’s expansion.

(define (euler k)
  (define (d i)
    (cond ((< i 1) 0)
          ((= 2 (remainder i 3)) (+ 2 (d (- i 3))))
          (else 1)))
  (define (n i) 1)
  (+ 2.0 (cont-frac-iter n d k)))

(define (euler-test k)
  (define (test n)
    (when (<= n k)
      (print "euler " n " " (euler n))
      (test (inc n))))
  (test 1))

;; This is weird. It doesn't seem right, but I can't see a problem
;; with the code. Maybe later...

;; #;739> (euler-test 30)
;; euler 1 2.5
;; euler 2 2.41666666666667
;; euler 3 2.6
;; euler 4 2.625
;; euler 5 2.23606797003472
;; euler 6 2.61904761904762
;; euler 7 2.61764705882353
;; euler 8 2.16227766016838
;; euler 9 2.61797752808989
;; euler 10 2.61805555555556
;; euler 11 2.12310562561766
;; euler 12 2.61803713527851
;; euler 13 2.61803278688525
;; euler 14 2.09901951359278
;; euler 15 2.61803381340013
;; euler 16 2.61803405572755
;; euler 17 2.08276253029822
;; euler 18 2.6180339985218
;; euler 19 2.61803398501736
;; euler 20 2.07106781186548

;; Exercise 1.39: A continued fraction representation of the tangent
;; function was published in 1770 by the German mathematician J.H.
;; Lambert:

;; tan⁡x=x1−x23−x25−…,

;; where xx is in radians. Define a procedure (tan-cf x k) that
;; computes an approximation to the tangent function based on
;; Lambert’s formula. k specifies the number of terms to compute, as
;; in Exercise 1.37.

(define (tan-cf x k)
  (define (n i)
    (if (= 1 i)
        x
        (- 0 (square x))))
  (define (d i)
    (- (* 2 i) 1))
  (cont-frac-iter n d k))

(define (tan-cf-test x))


(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (cube x) (* x x x))

;; ((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))

(define (sqrt x)
  (newtons-method
   (lambda (y)
     (- (square y) x))
   1.0))

(define (fixed-point-of-transform
         g transform guess)
  (fixed-point (transform g) guess))

(define (average-damp f)
  (lambda (x)
    (average x (f x))))

(define (ex-136c n)
  (fixed-point-of-transform
   (lambda (x) (/ (log n) (log x)))
   average-damp
   2))

(define (repeated-iter fn n)
  (define (iter x res-fn)
    (if (= x 0)
        res-fn
        (iter (dec x) (compose fn res-fn))))
  (iter n fn))

(define (iterative-improve good-enough? improve-guess)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve-guess guess))))
  iter)

(define (sqrt3 v)
  ((iterative-improve
    (lambda (x) (< (abs (- (square x) v)) 0.001))
    (lambda (x) (average x (/ v x))))
   1.0))

(define (double fn)
  (compose fn fn))

(define (range x)
  (define (iter x y)
    (if (= x 0)
        y
        (iter (- x 1) (cons x y))))
  (iter x '()))

(define (repeat x y)
  (define (iter x z)
    (print "x=" x " y=" y " z=" z)
    (if (= x 0)
        z
        (iter (- x 1) (cons y z))))
  (iter x '()))

(define (pow x y)
  (apply * (repeat y x)))

(define (noisy-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (print "trying " guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (nth-root v n)
  (define (f y)
    (/ v (pow y (- n 1))))
  (define damp (repeated-iter average-damp 4))
  (define damped-fn (lambda (x) (damp (f x))))
  (noisy-fixed-point
   damped-fn
   1.0))
