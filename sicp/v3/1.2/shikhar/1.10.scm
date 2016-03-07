(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n))
;; f(n) = 2n

(define (g n) (A 1 n))
;; g(n) = 2^n

(define (h n) (A 2 n))
;; h(1) = 2
;; h(2) = 4
;; h(n) = 2^h(n-1)
