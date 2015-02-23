; helpers
(define (square x)
  (* x x))

; ex 1.3

(define (biggest-square-sum x y z)
  (cond
    ((and (>= x y) (>= z y))
      (+ (square x) (square z)))
    ((and (>= x z) (>= y z))
      (+ (square x) (square y)))
    (else (+ (square y) (square z)))))
