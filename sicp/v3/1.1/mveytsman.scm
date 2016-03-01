;; Exercise 1.2

;; Translate the following expression into prefix form
;; (5 + 1/2 + (2 - (6 + 1/3))) / (3 * (6 - 2) * (2 -7))

(/ (+ 5 1/2 (- 2 (- 3 (+ 6 1/3)))) (* 3 (- 6 2) (- 2 7)))

;; Exercise 1.3
;; Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

;; First a helper
(define (sum-of-squares x y)
  (+ (* x x) (* y y)))

(define (sum-of-squares-of-largest x y z)
  (cond ((and (<= x y) (<= x z)) (sum-of-squares y z))
        ((and (<= y x) (<= y z)) (sum-of-squares x z))
        (else (sum-of-squares x y))))