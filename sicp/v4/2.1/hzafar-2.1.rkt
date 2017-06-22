#lang racket

(require rackunit
         rackunit/log)

;; 2.4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

; (car (cons 1 2))
; => (car (λ (m) (m 1 2)))
; => ((λ (m) (m 1 2)) (λ (p q) p))
; => ((λ (p q) p) 1 2)
; => 1

; Correspondingly,
(define (cdr z)
  (z (lambda (p q) q)))

(check-equal? (car (cons 1 2)) 1)
(check-equal? (cdr (cons 1 2)) 2)

;; 2.5
(define (ncons a b)
  (* (expt 2 a) (expt 3 b)))

(define (multiplicity n d)
  (define (iter n result)
    (if (> (remainder n d) 0)
        result
        (iter (/ n d) (+ 1 result))))
  (iter n 0))

(check-equal? (multiplicity 24 2) 3)

(define (ncar npair)
  (multiplicity npair 2))

(define (ncdr npair)
  (multiplicity npair 3))

(check-equal? (ncar (ncons 1 2)) 1)
(check-equal? (ncdr (ncons 1 2)) 2)

;; 2.6
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; (add-1 zero)
; => (λ (f) (λ (x) (f ((zero f) x))))
; => (λ (f) (λ (x) (f (((λ (f) (λ (x) x)) f) x))))
; => (λ (f) (λ (x) (f ((λ (x) x) x))))
; => (λ (f) (λ (x) (f x)))

; (add-1 (add-1 zero))
; (add-1 (λ (f) (λ (x) (f x))))
; => (λ (f) (λ (x) (f (((λ (f) (λ (x) (f x))) f) x))))
; => (λ (f) (λ (x) (f ((λ (x) (f x)) x))))
; => (λ (f) (λ (x) (f (f x))))

(define (plus a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

;; Handwaving ahead: If n is a Church numeral, think of the fragment (n f) as sort of
;; representing how many "f"s are in n. So, for 0, this would be nothing; for 1, this
;; would be f; for 2, this would be f·f (where "·" means function composition).
;;
;; Consider this in terms of the add-1 definition given in the book. The fragment
;; (f ((n f) x))
;; roughly means "take the number of fs in n, then apply one more f to the outside", so
;; we get from x to (f x) to (f (f x)), etc.
;;
;; When we add two Church numerals, we have some expressions like (f (f ... (f x)))),
;; where let's say the first numeral has _a_ compositions of f and the second has _b_ compositions
;; of f, and what we want is an expression that has _a+b_ compositions of f. Thus, we first
;; generate an expression containining _b_ compositions by pulling out those _b_ "f"s and
;; applying it to x: ((b f) x) => (f (f (f ...[b times]... x))). Then we apply _a_ more
;; "f"s to the outside of that to get the final expression that we want:
;; ((a f) ((b f) x)) => (f (f (f ...[a times]... (f (f (f ...[b times]... x))))))

;; Some tests
(define (eval-church-num n)
  ((n add1) 0))

(check-equal? (eval-church-num zero) 0)
(check-equal? (eval-church-num (add-1 zero)) 1)
(check-equal? (eval-church-num (plus (add-1 (add-1 zero)) (add-1 (add-1 (add-1 zero))))) 5)

(define three (add-1 (add-1 (add-1 zero))))
(check-equal? (eval-church-num (plus three (plus three three))) 9)

(test-log #:display? #t #:exit? #t)