;; Still plugging away with Chicken. Other than the +inf problem from last time,
;; things seem ok. I'll evaluate a few others if anything else comes up
;; (I've got my eye on gambit/termite and chez)

;;; 1.29

;; o_o'

;;; 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;;; 1.31
(define (sequence combine term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combine (term a) result))))
  (iter a 0))

(define (sequence combine term a next b)
  (if (> a b)
      0
      (combine (term a) (sequence combine term (next a) next b))))

(define (product term a next b)
  (sequence * term a next b))

;;;1.32
;; Oops. This question asks me to define `accumulate`,
;; which I defined in 1.31, but named `sequence`.

(define (accumulate combine term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combine (term a) result))))
  (iter a 0))

(define (accumulate combine term a next b)
  (if (> a b)
      0
      (combine (term a) (accumulate combine term (next a) next b))))

;;; 1.33
;; Just as a sidenote, we've officially passed the point where
;; any sane person could claim that adding generality is worth the
;; complexity trade-off we're incurring.

;; Also, this is an insane way to de-compose the problem. The natural
;; decomposition (for me, natuarlly, YMMV) for something like this
;; would be in terms of `filter` and `fold*`/`reduce`.
;; See the Haskell notes for details.

(define (filtered-accumulate filter combine term a next b)
  (define (iter a result)
    (cond ((> a b)
           result)
          ((filter a)
           (iter (next a) (combine (term a) result)))
          (else
           (iter (next a) result))))
  (iter a 0))

;; a)
(define (inc a) (+ a 1))
(define (sum-prime-squares a b)
  (filtered-accumulate prime? + square a inc b))

;; b)
(define (abelsons-sum n)
  (filetered-accumulate
   (lambda (a) (= 1 (gcd a n)))
   * id 1 inc (inc n)))

;;; 1.34
(define (f g) (g 2))

; An evaluation error, I think. (f f) reduces to (f 2), which reduces to (2 2), at which point we get an error unless we've defined "the function '2'", and we can't.

;;; 1.35
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (displayln guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;; (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) => .6180327868852458

;;; 1.36
;; Done. The function you're looking for is `displayln`

(fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1)

;; 34 steps

(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 1.1)

;; 13 steps.

;; Note that these do produce different outputs. The first one returns 4.555538934848503, while the second returns 4.555536364911781. Granted, this satisfies close-enough?, but still.

;;; 1.37

;;; 1.38

;;; 1.39
