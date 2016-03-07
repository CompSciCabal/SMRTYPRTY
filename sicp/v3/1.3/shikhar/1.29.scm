(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (inc i) (+ i 1))
  (define (term k) (* (y k)
                      (cond 
                        ((= k 0) 1)
                        ((= k n) 1)
                        ((even? k) 2)
                        (else 4))))
  (* (/ h 3) (sum term 0 inc n)))

(integral cube 0 1 10)
