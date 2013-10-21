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