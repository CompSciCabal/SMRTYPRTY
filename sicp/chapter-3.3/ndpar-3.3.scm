#lang planet neil/sicp

;; -------------------------------------------------------------------
;; Mutable Lists, p.252
;; -------------------------------------------------------------------

;; Exrcise 3.16, p.259

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; Tests

(= 3 (count-pairs (cons 1 (cons 2 (cons 3 nil)))))

(define x (cons 1 nil))
(= 4 (count-pairs (cons (cons 2 x) x)))
(= 5 (count-pairs (cons (cons x x) x)))

(define y (cons x x))
(= 7 (count-pairs (cons y y)))

(define z (cons 3 4))
(define cycle (cons 1 (cons 2 z)))
(set-cdr! z cycle)
;(= ∞ (count-pairs cycle)) ; Infinite loop

;; Exrcise 3.17, p.259

(define (cp x)
  (let ((visited nil))
    (define (iter y)
      (if (memq y visited)
          0
          (begin (set! visited (cons y visited))
                 (if (not (pair? y))
                     0
                     (+ (iter (car y))
                        (iter (cdr y))
                        1)))))
    (iter x)))

;; Tests

(= 3 (cp (cons 1 (cons 2 (cons 3 nil)))))
(= 3 (cp (cons (cons 2 x) x)))
(= 3 (cp (cons (cons x x) x)))
(= 3 (cp (cons y y)))
(= 3 (cp cycle))

;; Exercise 3.18, p.260

(define (cycle? ls)
  (define (iter y visited)
    (cond ((memq y visited) #t)
          ((null? y) #f)
          (else (iter (cdr y) (cons y visited)))))
  (iter ls nil))

;; Tests

(not (cycle? '(1 2 3)))
(cycle? cycle)

;; Exercise 3.19, p.260

(define (floyd-cycle? ls)
  (define (iter tortoise hare)
    (cond ((eq? tortoise hare) #t)
          ((null? hare) #f)
          ((null? (cdr hare)) #f)
          (else (iter (cdr tortoise) (cddr hare)))))
  (cond ((null? ls) #f)
        ((null? (cdr ls)) #f)
        (else (iter (cdr ls) (cddr ls)))))

;; Tests

(not (floyd-cycle? nil))
(not (floyd-cycle? '(1)))
(not (floyd-cycle? '(1 2)))
(not (floyd-cycle? '(1 2 3)))
(floyd-cycle? cycle)
