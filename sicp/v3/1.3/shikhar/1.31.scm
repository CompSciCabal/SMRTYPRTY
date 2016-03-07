; recursive

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

; iterative

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

; factorial

(define (identity x) x)

(define (inc x) (+ x 1))

(define (factorial n) (product identity 1 inc n))

(factorial 5)
