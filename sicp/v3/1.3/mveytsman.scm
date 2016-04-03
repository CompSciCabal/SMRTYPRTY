(require-extension sicp)

(define (cube x) (* x x x))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) 
     dx))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; Exercise 1.29: Simpson’s Rule is a more accurate method of numerical
;; integration than the method illustrated above. Using Simpson’s Rule, the
;; integral of a function f between a and b is approximated as

;; h/3(y_0+4y_1+2y_2+4y_3+2y_4+..+2y_{n-2}+4y_{n-1}+y_n)

;; where h=(b−a)/n, for some even integer n, and y_k=f(a+kh). (Increasing n
;; increases the accuracy of the approximation.) Define a procedure that takes
;; as arguments f, a, b, and n and returns the value of the integral, computed
;; using Simpson’s Rule. Use your procedure to integrate cube between 0 and 1
;; (with n=100 and n=1000), and compare the results to those of the integral
;; procedure shown above.

(define (simpsons-integral f a b n)
  (define h (/ (- b a) n)) ;; haven't gotten to let yet
  (define (simpsons-rule x0)
    (+ (f x0) (* 4 (f (+ x0 h))) (f (+ x0 (* 2 h)))))
  (define (next x)
    (+ x (* 2 h)))
  (* (/ h 3) (sum simpsons-rule a next (- b h))))


(simpsons-integral cube 0 1 100)
;;=> 0.25

(simpsons-integral cube 0 1 1000)
;;=> 0.25

(integral cube 0 1 0.01)
;;=> .2499875

(integral cube 0 1 0.001)
;;=> .249999875000001

;; The simpsons rule procedure is more accurate.

;; Interesting fact: "Since it uses quadratic polynomials to approximate functions, Simpson's rule actually gives exact results when approximating integrals of polynomials up to cubic degree." - http://mathworld.wolfram.com/SimpsonsRule.html

;; This means we can get accurate results for cube with an n of 2

(simpsons-integral cube 0 1 2)
;;=> 0.25

;; ----------------------------------

;;Exercise 1.30: The sum procedure above generates a linear recursion. The
;;procedure can be rewritten so that the sum is performed iteratively. Show how
;;to do this by filling in the missing expressions in the following definition:

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))


;;----------------------------


;;Exercise 1.31:

;; 1) The sum procedure is only the simplest of a vast number of similar
;;    abstractions that can be captured as higher-order procedures.51 Write an
;;    analogous procedure called product that returns the product of the values
;;    of a function at points over a given range. Show how to define factorial
;;    in terms of product. Also use product to compute approximations to π using
;;    the formula52 π/4=(2*4*4*6*6*8...) / (3*3*5*5*7*7...)


(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))


;; We can rewrite the pi formula as
;; pi/2 = (2*2*4*4*6*6*8...)/(3*3*5*5*7*7...)
;; we have an extra 8 up top so we get
;; pi/2 = 8*(2*2*4*4*6*6...)/(3*3*5*5*7*7...)
;; pi/2 = 2(n+1)* \product_0^n (2n)^2/(2n+1)^2
;; pi = 4 * n+1 * \product_0^n (2n)^2/(2n+1)^2
(define (pi n)
  (define (inc2 x)
    (+ x 2))
  (define (term x)
    (/ (square (* 2 x)) (square (+ 1 (* 2 x)))))
  (* 4 (+ n 1) (product term 1 inc n)))


(pi 100)
;;=> 3.1493784731686
(pi 1000)
;;=> 3.14237736509388

;; 2) If your product procedure generates a recursive process, write one that
;;    generates an iterative process. If it generates an iterative process,
;;    write one that generates a recursive process.

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

;; ----------------------------------

;;Exercise 1.32:

;; 1) Show that sum and product (Exercise 1.31) are both special cases of a
;;    still more general notion called accumulate that combines a collection of
;;    terms, using some general accumulation function:


;;    (accumulate
;;      combiner null-value term a next b)


;;    Accumulate takes as arguments the same term and range specifications as
;;    sum and product, together with a combiner procedure (of two arguments)
;;    that specifies how the current term is to be combined with the
;;    accumulation of the preceding terms and a null-value that specifies what
;;    base value to use when the terms run out. Write accumulate and show how
;;    sum and product can both be defined as simple calls to accumulate.

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (accumulate combiner null-value term (next a) next b) (term a) )))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;; 2) If your accumulate procedure generates a recursive process, write one that
;;    generates an iterative process. If it generates an iterative process,
;;    write one that generates a recursive process.

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))


;; -------------------------

;; Exercise 1.33: You can obtain an even more general version of accumulate
;; (Exercise 1.32) by introducing the notion of a filter on the terms to be
;; combined. That is, combine only those terms derived from values in the range
;; that satisfy a specified condition. The resulting filtered-accumulate
;; abstraction takes the same arguments as accumulate, together with an
;; additional predicate of one argument that specifies the filter. Write
;; filtered-accumulate as a procedure.


(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (filter a)
            (iter (next a) (combiner (term a) result))
            (iter (next a) result))))
  (iter a null-value))


;; Show how to express the following using filtered-accumulate:

;; 1) the sum of the squares of the prime numbers in the interval a to b
;;    (assuming that you have a prime? predicate already written)

(define (sum-of-prime-squares a b)
  (filtered-accumulate prime? + 0 square a inc b))

;; 2) the product of all the positive integers less than n that are relatively
;;    prime to n (i.e., all positive integers i<n such that GCD(i,n)=1.

(define (product-relatively-prime n)
  (define (filter i)
    (= 1 (gcd i n)))
  (filtered-accumulate filter * 1 identity a inc b))


;; ------------------------

;; Exercise 1.34: Suppose we define the procedure

(define (f g) (g 2))

;; Then we have

(f square)
;;=> 4

(f (lambda (z) (* z (+ z 1))))
;;=? 6

;; What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? Explain.

;; (f f) expands to
;; (f 2) expands to
;; (2 2)

;; 2 is not a procedure and can't be called so we fail with an error.

;; ------------------------

;; Exercise 1.35: Show that the golden ratio φ (1.2.2) is a fixed point of the
;; transformation x↦1+1/x, and use this fact to compute φ by means of the
;; fixed-point procedure.

;; We defined \phi as \phi^2=\phi + 1
;; Dividing by \phi we get \phi = 1 + 1/\phi, meaning that \phi is in fact a fixed point of (lambda (x) (+ 1 (/ 1 x)))

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

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1)
;;=> 1.61803278688525


;; ----------------------------------

;; Exercise 1.36: Modify fixed-point so that it prints the sequence of
;; approximations it generates, using the newline and display primitives shown
;; in Exercise 1.22. Then find a solution to x^x=1000 by finding a fixed
;; point of x↦log(1000)/log(x). (Use Scheme’s primitive log
;; procedure, which computes natural logarithms.) Compare the number of steps
;; this takes with and without average damping. (Note that you cannot start
;; fixed-point with a guess of 1, as this would cause division by
;; log(1)=0.)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))



(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)
;; 2
;; 9.96578428466209
;; 3.00447220984121
;; 6.27919575750716
;; 3.75985070240154
;; 5.2158437849259
;; 4.1822071924014
;; 4.82776509834459
;; 4.38759338466268
;; 4.6712500857639
;; 4.48140361689505
;; 4.6053657460929
;; 4.52308496787189
;; 4.57711468204734
;; 4.54138248015145
;; 4.56490324523083
;; 4.54937267930334
;; 4.55960649191329
;; 4.55285387578827
;; 4.55730552974826
;; 4.55436906443618
;; 4.556305311533
;; 4.55502826357355
;; 4.55587039670285
;; 4.55531500119208
;; 4.55568126354333
;; 4.55543971573685
;; 4.55559900999829
;; 4.55549395753139
;; 4.55556323729288
;; 4.55551754841765
;; 4.5555476793064
;; 4.55552780851625
;; 4.55554091291796
;; 4.55553227080365

(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2)
;; 2
;; 5.98289214233104
;; 4.92216872130834
;; 4.62822431819546
;; 4.56834651313624
;; 4.5577305909237
;; 4.55590980904513
;; 4.55559941161062
;; 4.55554655214737
;; 4.55553755199982


;; Without average dampening, we took 34 guesses, with it we only took 9.

;; -----------------------


;; Exercise 1.37:

;; 1)  An infinite continued fraction is an expression of the form
;; f=N1D1+N2D2+N3D3+….
;; f=N1D1+N2D2+N3D3+….
;; As an example, one can show that the infinite continued fraction expansion with the NiNi and the DiDi all equal to 1 produces 1/φ1/φ, where φφ is the golden ratio (described in 1.2.2). One way to approximate an infinite continued fraction is to truncate the expansion after a given number of terms. Such a truncation—a so-called finite continued fraction k-term finite continued fraction—has the form
;; N1D1+N2⋱+NkDk.
;; N1D1+N2⋱+NkDk.
;; Suppose that n and d are procedures of one argument (the term index ii) that return the NiNi and DiDi of the terms of the continued fraction. Define a procedure cont-frac such that evaluating (cont-frac n d k) computes the value of the kk-term finite continued fraction. Check your procedure by approximating 1/φ1/φ using
;; (cont-frac (lambda (i) 1.0)
;;            (lambda (i) 1.0)
;;           k)

;;How large must you make k in order to get an approximation that is accurate to 4 decimal places?

(define (cont-frac n d k)
  ;; assume l >= 1
  (if (= k 1)
      (/ (n 1) (d 1))
      (/ (n 1) (+ (d 1) (cont-frac (lambda (x) (n (+ x 1)))
                                   (lambda (x) (d (+ x 1)))
                                   (- k 1))))))

(/ 1  (cont-frac (lambda (i) 1.0)
                 (lambda (i) 1.0)
                 12))
;;=> 1.61805555555556


;; We get within 4 decimal places using k=12

;; 2) If your cont-frac procedure generates a recursive process, write one that
;; generates an iterative process. If it generates an iterative process, write
;; one that generates a recursive process.


(define (cont-frac n d k)
  ;; assume l >= 1
  (define (iter result i)
    (if (= i 0)
        result
        (iter (/ (n i) (+ (d i) result)) (- i 1))))
  (iter 0 k))


;; ---------------------------------

;; Exercise 1.38: In 1737, the Swiss mathematician Leonhard Euler published a
;; memoir De Fractionibus Continuis, which included a continued fraction
;; expansion for e−2, where e is the base of the natural logarithms. In this
;; fraction, the Ni are all 1, and the Di are successively 1, 2, 1, 1, 4, 1,
;; 1, 6, 1, 1, 8, …. Write a program that uses your cont-frac procedure from
;; Exercise 1.37 to approximate ee, based on Euler’s expansion.



(define (compute-e k)
  (define (d i)
    (if (= 0 (remainder (- i 2) 3))
        (* 2 (+ 1 (/ (- i 2) 3)))
        1))
  (+ 2 (cont-frac (lambda (x) 1) d k)))

(compute-e 100)
;;=> 2.71828182845905


;; --------------------------

;; Exercise 1.39: A continued fraction representation of the tangent function
;; was published in 1770 by the German mathematician J.H. Lambert:
;;

;; tan(x) = x/(1 - (x^2/(3-(x^2/5-...))))

;;where x is in radians. Define a procedure (tan-cf x k) that computes an
;;approximation to the tangent function based on Lambert’s formula. k specifies
;;the number of terms to compute, as in Exercise 1.37.

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (square x)))
  (define (d i)
    (- (* 2 i) 1))
  (define (iter result i)
    (if (= i 0)
        result
        (iter (/ (n i) (- (d i) result)) (- i 1))))
  (iter 0 k))


(tan 2)
;;=> -2.18503986326152

(tan-cf 2 100)
;;=> -2.18503986326152

;; ------------------------


;; Exercise 1.40: Define a procedure cubic that can be used together with the
;; newtons-method procedure in expressions of the form

;; (newtons-method (cubic a b c) 1)

;; to approximate zeros of the cubic x^3+ax^2+bx+c

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) 
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) 
               guess))


(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x) c)))

(newtons-method (cubic 3 2 7) 1)
;;=> -3.08674533988258

;; --------------------------

;; Exercise 1.41: Define a procedure double that takes a procedure of one
;; argument as argument and returns a procedure that applies the original
;; procedure twice. For example, if inc is a procedure that adds 1 to its
;; argument, then (double inc) should be a procedure that adds 2.

(define (double f)
  (lambda (x)
    (f (f x))))

;; What value is returned by

(((double (double double)) inc) 5)
;;=> 21

;; -----------------------------

;; Exercise 1.42: Let f and g be two one-argument functions. The composition
;; f after g is defined to be the function x↦f(g(x)). Define a
;; procedure compose that implements composition. For example, if inc is a
;; procedure that adds 1 to its argument,

(define (compose f g)
  (lambda (x)
    (f (g x))))

((compose square inc) 6)
;;=> 49

;; ------------------------------

;; Exercise 1.43: If f is a numerical function and n is a positive integer,
;; then we can form the nth repeated application of f, which is defined to
;; be the function whose value at x is f(f(…(f(x))…)). For
;; example, if f is the function x↦x+1, then the nth repeated
;; application of f is the function x↦x+n. If f is the operation of
;; squaring a number, then the nth repeated application of f is the function
;; that raises its argument to the 2n-th power. Write a procedure that
;; takes as inputs a procedure that computes f and a positive integer n and
;; returns the procedure that computes the nth repeated application of f.
;; Your procedure should be able to be used as follows:

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))


((repeated square 2) 5)
;;=> 625

;; -----------------------------------

;; Exercise 1.44: The idea of smoothing a function is an important concept in
;; signal processing. If f is a function and dx is some small number, then
;; the smoothed version of f is the function whose value at a point x is the
;; average of f(x−dx), f(x), and f(x+dx). Write a procedure
;; smooth that takes as input a procedure that computes f and returns a
;; procedure that computes the smoothed f.

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx)))
       3)))


;; It is sometimes valuable to repeatedly smooth a function (that is, smooth the
;; smoothed function, and so on) to obtain the n-fold smoothed function. Show
;; how to generate the n-fold smoothed function of any given function using
;; smooth and repeated from Exercise 1.43.

(define (repeated-smooth f n)
  ((repeated smooth n) f))

;; ----------------------------------------

;; Exercise 1.45: We saw in 1.3.3 that attempting to compute square roots by
;; naively finding a fixed point of y↦x/yy↦x/y does not converge, and that this
;; can be fixed by average damping. The same method works for finding cube roots
;; as fixed points of the average-damped y↦x/y2y↦x/y2. Unfortunately, the
;; process does not work for fourth roots—a single average damp is not enough to
;; make a fixed-point search for y↦x/y3y↦x/y3 converge. On the other hand, if we
;; average damp twice (i.e., use the average damp of the average damp of
;; y↦x/y3y↦x/y3) the fixed-point search does converge. Do some experiments to
;; determine how many average damps are required to compute nthnth roots as a
;; fixed-point search based upon repeated average damping of y↦x/yn−1y↦x/yn−1.
;; Use this to implement a simple procedure for computing nthnth roots using
;; fixed-point, average-damp, and the repeated procedure of Exercise 1.43.
;; Assume that any arithmetic operations you need are available as primitives.

(define (average-damp f)
  (lambda (x) 
    (average x (f x))))

;; I don't know how to show this analytically, but I found that you need to average-damp floor(log2(n)), i.e. every power of 2 your need to average damp another time


(define (log2 n)
  (/ (log n) (log 2)))

(define (nth-root x n)
  (fixed-point 
   ((repeated average-damp (floor (log2 n))) 
    (lambda (y) (/ x (expt y (- n 1)))))
   1.0))


;; -------------------

;; Exercise 1.46: Several of the numerical methods described in this chapter are
;; instances of an extremely general computational strategy known as iterative
;; improvement. Iterative improvement says that, to compute something, we start
;; with an initial guess for the answer, test if the guess is good enough, and
;; otherwise improve the guess and continue the process using the improved guess
;; as the new guess. Write a procedure iterative-improve that takes two
;; procedures as arguments: a method for telling whether a guess is good enough
;; and a method for improving a guess. Iterative-improve should return as its
;; value a procedure that takes a guess as argument and keeps improving the
;; guess until it is good enough. Rewrite the sqrt procedure of 1.1.7 and the
;; fixed-point procedure of 1.3.3 in terms of iterative-improve.

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  iter)

(define (sqrt-ii x)
  ((iterative-improve
    (lambda (guess)
      (< (abs (- (square guess) x)) 0.001))
    (lambda (guess)
      (average guess (/ x guess))))
   1.0))



(define (fixed-point-ii f first-guess)
  ((iterative-improve
    (lambda (guess)
      (< (abs (- guess (f guess)))
         tolerance))
    f)
   first-guess))

;; Note that this version gives a different answer than by the fixed-point defined in 1.3.3.

(fixed-point (lambda (y) (+ (sin y) (cos y)))
             1.0)
;;=> 1.25873159629712
(fixed-point-ii (lambda (y) (+ (sin y) (cos y)))
                1.0)
;;=> 1.25872287430527

;; The reason for it is that fixed-point-ii does one less iteration than fixed-point:

((lambda (y) (+ (sin y) (cos y)))  1.25872287430527)
;;=> 1.25873159629712 - the result of the fixed-point call

;; This is because in the definition of fixed-point in the book

(define (try guess)
  (let ((next (f guess)))
    (if (close-enough? guess next)
        next
        (try next))))

;; If the guess is close-enough, we return (f guess), not guess. This doesn't
;; fit into how I wrote iterative-improve. When the guess is good enough, you
;; return the guess, not call improve an extra time.

