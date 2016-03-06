;; 1.29

;; y0 = f(a + 0h) = f(a)

(define (cube x)
  (* x x x))

(define (simpsons f a b n)
  (define h (/ (- b a) n))
  (define (multiplier k)
    (cond ((or (= k 0) (= k n)) 1)
          ((odd? k) 4)
          (else 2)))
  (define (yk k)
    (* (multiplier k) (f (+ a (* k h)))))
  (define (sum k)
    (if (> k n)
        0
        (+ (yk k) (sum (+ 1 k)))))
  (* (/ h 3.0) (sum 0)))

;; 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (a > b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;; 1.31.1

(define (product-recur term a next b)
  (if (> a b)
      1
      (* (term a) (product-recur term (next a) next b))))

(define (factorial n)
  (define (inc x)
    (+ x 1))
  (define (identity x)
    x)
  (product-recur identity 1 inc n))

(define (pi n)
  (define (numerator counter)
    (if (even? counter)
        (+ 2 counter)
        (+ 1 counter)))
  (define (denominator counter)
    (if (even? counter)
        (+ 1 counter)
        (+ 2 counter)))
  (define (term counter)
    (/ (numerator counter) (denominator counter)))
  (define (inc counter)
    (+ 1 counter))
  (* 4.0 (product-recur term 1 inc n)))

;; 1.31.2

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

;; 1.32

(define (accumulate-recur combiner null-value term a next b)
  (define (recur a)
    (if (> a b)
        null-value
        (combiner (term a) (recur (next a)))))
  (recur a))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate-iter + 0 term a next b))

(define (product term a next b)
  (accumulate-iter * 1 term a next b))

1.33

(define (filtered-accumulate-iter pred combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((pred a) (iter (next a) (combiner result (term a))))
          ;; combiner optional here
          (else (iter (next a) result))))
  (iter a null-value))

(define (filtered-accumulate-recur pred combiner null-value term a next b)
  (define (recur a)
    (cond ((a > b) null-value)
          ((pred a) (combiner (term a) (recur (next a))))
          ;; combiner is optional here
          (else (recur (next a)))))
  (recur a))

(define (sum-prime-range a b)
  (define (next x)
    (if (= 2 x)
        (+ x 1)
        (+ x 2)))
  (filtered-accumulate-recur prime? + 0 square a next b))

(define (prod-rel-prime n)
  (define (inc x)
    (+ 1 x))
  (define (identity x)
    x)
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
  (define (rel-prime? x)
    (= (gcd x n) 1))
  (filtered-accumulate-iter rel-prime? * 1 identity 1 inc (- n 1)))

