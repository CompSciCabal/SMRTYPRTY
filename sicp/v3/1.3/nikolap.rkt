(define (sum term a next b)
	(if (> a b)
		0
		(+ (term a)
			(sum term (next a) next b))))

;; ----------------------------------------------
;; exercise 1.29


(define (cube x) (* x x x))

(define (integral f a b n)
	(define h (/ (- b a) n))
	(define (y k)
		(f (+ a (* k h))))
	(define (term k)
		(* (y k)
			(cond
				((= 0 k) 0)
				((= n k) 1)
				((odd? k) 4)
				((even? k) 2))))
	(sum term 0 inc n))

;; ----------------------------------------------
;; exercise 1.30

(define (sum2 term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (+ result (term a)))))
	(iter a 0))

;; ----------------------------------------------
;; exercise 1.31

;; thank goodness for google https://en.wikipedia.org/wiki/Wallis_product
;; expansion of (2n / (2n - 1)) * (2n / (2n - 1)) => (4n^2 / (4n^2 - 1))

(define (square x) (* x x))

(define (product term a next b)
	(if (> a b)
		0
		(* (term a)
		   (product term (next a) next b))))

(define (factorial n)
	(product identity 1 inc n))

(define (pi-aprx n)
	(define (term x)
		(/ (* 4 (square n))
		   (- (* 4 (square n)) 1)))
	(* 2 (product term 1 inc n)))

(define (product2 term a next b)
	(define (iter a result)
		(if (> a b)
			result
			(iter (next a) (* result (term a)))))
	(iter a 0))

;; ----------------------------------------------
;; exercise 1.32

(define (accumulate combiner null-value term a next b)
	(if (> a b)
		null-value
		(combiner (term a)
			      (product term (next a) next b))))

(define (accumulate combiner null-value term a next b)
	(define (iter a result)
		(if (> a b)
			null-value
			(iter (next a) (combiner result (term a)))))
	(iter a 0))

;; ----------------------------------------------
;; exercise 1.33
(define (filtered-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a)
                    (filtered-accum filter combiner null-value term (next a) next b))
          (filtered-accum filter combiner null-value term (next a) next b))))

;; assume prime? fn

(define (sum-square-primes a b)
   (filtered-accumulate prime? + 0 square a inc b))

;; assume gcd fn

(define (gcd-prod n)
	(define (filterer x)
		(= (1 (gcd x n))))
	(filtered-acum filterer * 1 identity inc (- n 1)))

;; ----------------------------------------------
;; exercise 1.34

(define (f g)
	(g 2))

;; (f f)
;; (f (f 2))
;; (f (f 2))
;; (f 2) --> uh oh

;; skipping some of the mathy ones :D

;; ----------------------------------------------
;; exercise 1.40

;; f(x) = x^3 + ax^2 + bx + c

(define (cubic a b c)
	(lambda (x)
		(+  (* x x x)
			(* a x x)
			(* b x)
			c)))

;; ----------------------------------------------
;; exercise 1.41

(define (double fn)
	(lambda (x) (fn (fn x))))

;; ----------------------------------------------
;; exercise 1.42

(define (compose fn1 fn2) ; yay comp!
	(lambda (x) (fn1 (fn2 x)))) 

;; I wonder if you can use an undefined number of args in fn params of a define. In which case can abstract compose for any number of fns, not just 2
;; if so, then 1.43 becomes pretty easy, assuming you write an apply fn

;; ----------------------------------------------
;; exercise 1.43

(define (repeated fn n)
	(if (= n 1)
		fn
		(compose fn 
				 (repeated fn (dec n)))))

;; ----------------------------------------------
;; exercise 1.44

;; pretty simple -- return fn that averages up fn of x, x + dx, and x - dx

(define (smooth fn dx)
	(labmda (x)
		(/	(+  (fn x)
				(fn (+ x dx))
				(fn (- x dx)))
			3)))

;; ----------------------------------------------
;; exercise 1.45

(define (nth-roots x y n)
	(labmda (x)
		)) ;; herp derp? No time left, but don't think I'm going down the right path. Will try again later.

;; ----------------------------------------------
;; exercise 1.46

(define (iterative-improve good-enough? improve-guess)
	(define (iterate guess)
		(if (good-enough? guess)
			guess
			(iterate (improve-guess guess))))
	iterate)