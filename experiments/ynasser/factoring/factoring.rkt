#lang racket
;; Pollard's rho factoring algorithm

;; the transformation function from the original paper
;; (there are better ones depending on your n) is
;; f = (x^2 - 1) mod n

(define (f) 
  (lambda (x) (lambda (n) (modulo (- (* x x) 1) n))))

(define (f-expanded x n)
  (modulo (+ (* x x) 1) n))

(define (pollard n func)
  (define (while-d-equals-1 x y d)
    (display "x: ")(display x)(display "\n")
    (display "y: ")(display y)(display "\n")
    (display "d: ")(display d)(display "\n")
    (set! x (((func)x)n))
    (set! y (((func)(((func)y)n))n))
    (cond
      [(not (equal? d 1)) d]
      [else (while-d-equals-1 x y (gcd (abs (- x y)) n))]))
  (while-d-equals-1 2 2 1)) ;; not magic numbers!
 
;; returns all factors, hopefully
(define (factor-pollard n)
  (define (helper n lst)
    (cond
      [(equal? 1 n) (cons 1 lst)]
      [else (define new-number (pollard n f))
            (helper (/ n new-number) (cons new-number lst))]))
  (helper n '()))

;; a very naive factoring algorithm
(define (factoring num lst c)
  (cond
    [(> c num) lst]
    [(eq? 0 (modulo num c)) (factoring num (cons c lst) (+ 1 c))]
    [else (factoring num lst (+ 1 c))]))

;; Fermat's factoring algorithm
(define (is-square? n)
  (integer? (sqrt n)))

;; this is slower than python's default math library (and less precise?)
(define (fermat-factor n)
  (define a (ceiling (sqrt n)))
  (define b2 (- (* a a) n))
  (define (while-b2-not-sqr x y)
    (cond
      [(is-square? y)
       (display "First factor: ")(display (+ x (sqrt y)))(display "\n")
       (display "Second factor: ")(display (- x (sqrt y)))(display "\n")]
      [else (while-b2-not-sqr (+ x 1) (- (* x x) n))]))
  (while-b2-not-sqr a b2))
  
 (factor-pollard 45893845)

