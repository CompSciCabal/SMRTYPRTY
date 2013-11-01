#lang racket

;;; EX 1.29

(define (cube n) (* n n n))

(define (inc n) (+ n 1))

(define (id n) n)

(define (even? n) (= (remainder n 2) 0))

(define (sum-from a b term next) 
  (if (> a b)
      0
      (+ (term a)
         (sum-from (next a) b term next))))

(define (integral f a b dx) 
  (define (add-dx x)
    (+ x dx)) 
  (* (sum-from (+ a (/ dx 2.0)) b f add-dx)
     dx))

(define (simpsons fun a b n)
  (define h (/ (- b a) n))
  (define (coeff k)
    (cond ((= k 0) 1)
          ((= k n) 1)
          ((even? k) 2)
          (else 4)))
  (define (term k)
    (* (coeff k) (fun (+ a (* k h)))))
  (* (/ h 3) (sum-from 0 n term inc)))

; (integral cube 0 1 0.001)
;   0.249999875000001
; (simpsons cube 0 1 1000.0)
;   0.2496671666666661
; --- that's unexpected...

; (simpsons cube 10 11 1000.0)
;   1159.473010499999
; (integral cube 10 11 0.001)
;   1160.2499973749036

; and that's even worse (should be 1160.25)

; note: i'd reversed 4 and 2 in the definition -- all better now.

;;; EX 1.30

(define (sum term a next b) 
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a))))) 
  (iter a 0))

;;; EX 1.31a

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product id 1 inc n))

(define (pi-apr n)
  (define (term n)
    (/ (if (even? n)
           (+ n 2)
           (+ n 1))
       (if (even? n)
           (+ n 1)
           (+ n 2))))
  (* 4.0 (product term 1 inc n)))

;;; EX 1.31b

(define (product-rec term a next b) 
  (if (> a b)
      1
      (* (term a)
         (product-rec term (next a) next b))))


;;; EX 1.32a

(define (accumulate combiner null-value term a next b) 
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) next b))))

(define (sum-a term a next b)
  (accumulate + 0 term a next b))

(define (product-a term a next b)
  (accumulate * 1 term a next b))

;;; EX 1.32b

(define (accumulate-inc combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))


;;; EX 1.33

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (if (filter a)
                           (combiner result (term a))
                           result))))
  (iter a null-value))

(define (square n) (* n n))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor) 
  (cond ((> (square test-divisor) n) n) 
        ((divides? test-divisor n) test-divisor) 
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;;; 1.33a
(define (sum-of-prime-squares a b)  
  (filtered-accumulate prime? + 0 square a inc b))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;; 1.33b
(define (gcd-prod n)
  (define (gcd-one? x)
    (= 1 (gcd x n)))
  (filtered-accumulate gcd-one? * 1 id 2 inc n))


;;; EX 1.34

(define (f g) (g 2))
; (f f)
; (f 2)
; (2 2) <--- error


;;; EX 1.35

(define phi (/ (+ 1 (sqrt 5)) 2))

; 1 + 1/phi = 1 + 2/(1+r5) = (3 + r5)/(1 + r5) =
; = (3 + r5 - 3r5 - 5) / (1 + r5 - r5 -5) = 
; [i.e. * (1 - r5)/(1 - r5) ]
; = (-2 - 2r5) / -4 = (1 + r5) / 2

;(= (+ 1 (/ 1 phi)) phi)
;  #t

(define tolerance 0.00001) 
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) 
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next)))) 
  (try first-guess))

; (fixed-point (lambda (x) (+ 1 (/ 1 x))) 2.0)
;   1.6180327868852458


;;; EX 1.36

; (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
;   35 steps
; (expt 4.555532270803653 4.555532270803653)
;   999.9913579312362

; (fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2.0)
;   10 steps
; (expt 4.555537551999825 4.555537551999825)
;   1000.0046472054871


;;; EX 1.37

(define (cont-frac n d k)
  (define (cont-frac-iter n d k acc)
    (if (= k 0)
        acc
        (cont-frac-iter n d (- k 1) (/ (n k) (+ (d k) acc)))))
  (cont-frac-iter n d k 0))

; (/ 1 phi)
;   0.6180339887498948
; (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)
;   0.6179775280898876
; (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)
;   0.6180555555555556

(define (cont-frac-rec n d k)
  (if (= k 0)
      0
      (/ (n k) (+ (d k) (cont-frac-rec n d (- k 1))))))


;;; EX 1.38

(define (e-approx n)
  (+ 2
     (cont-frac (lambda (i) 1.0) 
                (lambda (x) 
                  (if (= 2 (remainder x 3))
                      (* 2 (+ 1 (quotient x 3)))
                      1))
                n)))

(define e 2.718281828459045)

; e
;   2.718281828459045
; (e-approx 10)
;   2.7182817182817183
; (e-approx 15)
;   2.718281828470584
; (e-approx 20)
;   2.718281828459045


;;; EX 1.39

(define (tan-cf x k)
  (let ((sx (- 0 (square x))))
    (cont-frac (lambda (n) (if (= n 1) x sx))
               (lambda (d) (- (* 2 d) 1.0))
               k)))

; (tan 1)
;   1.557407724654902
; (tan-cf 1 5)
;   1.5574074074074076
; (tan-cf 1 10)
;   1.557407724654902


;;;;;;;;;;; START 1.3.4 ;;;;;;;;;;;;;;

;;; EX 1.40

(define (deriv g) 
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (newton-transform g) 
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess) 
  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess) 
  (fixed-point (transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

;;; EX 1.41

(define (double f)
  (lambda (x) (f (f x))))

; (((double (double double)) inc) 5)
;   21

; ((double (double (double inc))) 5)
;   13

; (((double (double (double double))) inc) 0)
;   256
; ((double (double (double (double inc)))) 0)
;   16

;;; EX 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

;;; EX 1.43

(define (repeated f n)
  (define (repeat-iter f n acc)
    (if (= n 0) 
        acc
        (repeat-iter f (- n 1) (compose f acc))))
  (repeat-iter f n (lambda (x) x)))

(define (repeated-rec f n)
  (if (= n 0)
      (lambda (x) x)
      (compose f (repeated-rec f (- n 1)))))

;;; EX 1.44

(define (smooth f)
  (lambda (x) 
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-smooth f n)
  ((repeated smooth n) f))

; (sin (/ 1 0.0001))
;   -0.30561438888825215
; ((smooth (lambda (x) (sin (/ 1 x)))) 0.0001)
;   -0.13922089082976782
; ((n-smooth (lambda (x) (sin (/ 1 x))) 2) 0.0001)
;   0.023617992290436273
; ((n-smooth (lambda (x) (sin (/ 1 x))) 3) 0.0001)
;   0.05419684260015855
; ((n-smooth (lambda (x) (sin (/ 1 x))) 4) 0.0001)
;   0.0666205827502035
; ((n-smooth (lambda (x) (sin (/ 1 x))) 5) 0.0001)
;   0.06994069568929728
; ((n-smooth (lambda (x) (sin (/ 1 x))) 6) 0.0001)
;   0.06724567385347607
; ((n-smooth (lambda (x) (sin (/ 1 x))) 7) 0.0001)
;   0.06351046183967048

;;; EX 1.45

(define (average j k)
  (/ (+ j k) 2))

(define (average-damp f) 
  (lambda (x) (average x (f x))))

(define (ssqrt x) 
  (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))

(define (cbrt x)
  (fixed-point-of-transform (lambda (y) (/ x (* y y))) average-damp 1.0))

(define (4rt x)
  (fixed-point-of-transform (lambda (y) (/ x (* y y y))) (double average-damp) 1.0))

(define (5rt x)
  (fixed-point-of-transform (lambda (y) (/ x (* y y y y))) (double average-damp) 1.0))

(define (6rt x)
  (fixed-point-of-transform (lambda (y) (/ x (* y y y y y))) (double average-damp) 1.0))

(define (7rt x)
  (fixed-point-of-transform (lambda (y) (/ x (* y y y y y y))) (double average-damp) 1.0))

(define (nrt x n)
  (fixed-point-of-transform 
   (lambda (y) (/ x (expt y (- n 1)))) 
   (repeated average-damp (floor (/ (log n) (log 2)))) 
   1.0))

; the 10,000th root converges, but it takes a looooooong time...


;;; EX 1.46

(define (iterative-improve improve-guess good-enough)
  (define (iterate guess)
    (if (good-enough guess) 
        guess
        (iterate (improve-guess guess))))
  (lambda (guess) (iterate guess)))

; lambda, let, etc

(define (iisqrt n)
  ((iterative-improve
    (lambda (x) (/ (+ x (/ n x)) 2))
    (lambda (x) (< (abs (- n (* x x))) dx)))
   1.0))

(define (iifp f first-guess)
  ((iterative-improve
    f
    (lambda (x) (< (abs (- x (f x))) dx)))
   first-guess))

; this is totally cheating...
    

