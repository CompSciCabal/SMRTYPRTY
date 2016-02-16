(require-extension sicp)

;; SICP Chapter 1, Section 1.1, The Elements of Programming

;; Exercise 1.2

;;  5 + 4 + (2 - (3 - (6 + 4/5)))
;;  -----------------------------
;;        3(6 - 2)(2 - 7)

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; Exercise 1.3

(define (sum-of-squares x y)
  (+ (* x x) (* y y)))

(define (sum-of-larger-squares x y z)
  (cond ((and (>= x z) (>= y z)) (sum-of-squares x y))
        ((and (>= x y) (>= z y)) (sum-of-squares x z))
        (else (sum-of-squares y z))))

;; Exercise 1.5

;; Ben Bitdiddle has invented a test to determine whether the
;; interpreter he is faced with is using applicative-order evaluation
;; or normal-order evaluation. He defines the following two
;; procedures:

;; (define (p) (p))

;; (define (test x y)
;;   (if (= x 0)
;;       0
;;       y))

;; Then he evaluates the expression

;; (test 0 (p))

;; What behavior will Ben observe with an interpreter that uses
;; applicative-order evaluation? What behavior will he observe with an
;; interpreter that uses normal-order evaluation? Explain your answer.
;; (Assume that the evaluation rule for the special form if is the
;; same whether the interpreter is using normal or applicative order:
;; The predicate expression is evaluated first, and the result
;; determines whether to evaluate the consequent or the alternative
;; expression.)

;; Answer:

;; If the interpreter uses applicative-order evaluation, it will hang
;; as it recurses infinitely on the procedure `p`. This is because
;; both of the operands in the combination `(test 0 (p))` will be
;; evaluated and substituted before being passed to the definition of
;; `test`.

;; If the interpreter uses normal-order evaluation, the expression
;; will evaluate to 0, since neither expression will be evaluated
;; until they are required - in this case, the 0 will be evaluated
;; when the comparison is made in the first `if` clause, and since it
;; will return true, only the second clause will need to be evaluated
;; and returned. Thus, the invocation of the `p` procedure will never
;; occur.

;; Exercise 1.6

;; Alyssa P. Hacker doesn't see why if needs to be provided as a
;; special form. ``Why can't I just define it as an ordinary procedure
;; in terms of cond?'' she asks. Alyssa's friend Eva Lu Ator claims
;; this can indeed be done, and she defines a new version of if:

;; (define (new-if predicate then-clause else-clause)
;;   (cond (predicate then-clause)
;;         (else else-clause)))

;; Eva demonstrates the program for Alyssa:

;; (new-if (= 2 3) 0 5)
;; 5

;; (new-if (= 1 1) 0 5)
;; 0

;; Delighted, Alyssa uses new-if to rewrite the square-root program:

;; (define (sqrt-iter guess x)
;;   (new-if (good-enough? guess x)
;;           guess
;;           (sqrt-iter (improve guess x)
;;                      x)))

;; What happens when Alyssa attempts to use this to compute square
;; roots? Explain.

;; Answer:

;; The call stack will overflow. This is because, whereas the `if`
;; special form is written such that the third clause is never
;; evaluated if the first clause evaluates to true, scheme is
;; otherwise an applicative-order interpreted language. So, the
;; `new-if` procedure does not benefit from this special handling, and
;; `sqrt-iter` will be called every time `new-if` is called.

;; Exercise 1.7

;; The good-enough? test used in computing square roots will not be
;; very effective for finding the square roots of very small numbers.
;; Also, in real computers, arithmetic operations are almost always
;; performed with limited precision. This makes our test inadequate
;; for very large numbers. Explain these statements, with examples
;; showing how the test fails for small and large numbers. An
;; alternative strategy for implementing good-enough? is to watch how
;; guess changes from one iteration to the next and to stop when the
;; change is a very small fraction of the guess. Design a square-root
;; procedure that uses this kind of end test. Does this work better
;; for small and large numbers?

;; Answer:

;; The margin of error in the provided `good-enough?` predicate is an
;; absolute value, so attempts to calculate square roots of values
;; which approach this margin in size will have relatively much less
;; accurate results.

;; OTOH for extremely large numbers where our calculations will be
;; performed with limited precision, we may well never reach the
;; termination case.

(define (square x)
  (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (/ (- (improve guess x) guess)
             guess))
     0.001))

(define (squart x)
  (sqrt-iter 1.0 x))

;; Exercise 1.8

;; Newton's method for cube roots is based on the fact that if y is an
;; approximation to the cube root of x, then a better approximation is
;; given by the value

;; x/9^2 + 2y
;; ----------
;;     3

;; Use this formula to implement a cube-root procedure analogous to
;; the square-root procedure. (In section 1.3.4 we will see how to
;; implement Newton's method in general as an abstraction of these
;; square-root and cube-root procedures.)

(define (cube-root x)
  (cube-root-iter x 1.0 0.0))

(define (cube-root-iter x guess last-guess)
  (if (good-enough1? guess last-guess)
      guess
      (cube-root-iter x (improve-guess x guess) guess)))

(define (good-enough1? guess last-guess)
  (let* ((diff (abs (- guess last-guess)))
         (p-age (* 100 (/ diff guess))))
    (< p-age 0.00001)))

(define (improve-guess x y)
  (/ (+ (/ x (* y y)) (* 2 y)) 3))
