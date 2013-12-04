#lang racket

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; Exercise 2.59
(define (union-set-2.59 s0 s1)
  (cond
    [(empty? s0) s1]
    [(element-of-set? (car s0) s1) (union-set-2.59 (cdr s0) s1)]
    [else (union-set-2.59 (cdr s0) (cons (car s0) s1))]))

;; Exercise 2.60
(define (elem-of-set? x s)
  (display "s is ")(display s)(newline)
  (cond
    [(or (empty? s)(> (car s) x)) #f]
    [(equal? x (car s)) #t]
    [else (elem-of-set? x (cdr s))]))

(define (insert x s s0)
  ;(display "s ")(display s)(newline)
  ;(display "s0 ")(display s0)(newline)
  (cond
    [(empty? s) (append s0 (list x))]
    [(< x (car s)) (append (append s0 (list x)) s)]
    [else (insert x (cdr s) (append s0 (list (car s))))]))

(define (adjoin-set x s)
  (if (elem-of-set? x s)
      s
      (insert x s null)))

(define (intersect-set s0 s1)
  (define (put-together set0 set1 carry)
        (cond
          [(ormap empty? (list set0 set1)) carry]
          [(elem-of-set? (car set0) set1) 
           (put-together (cdr set0) set1 (append carry (list (car set0))))]
          [else (put-together (cdr set0) set1 carry)]))
    (put-together s0 s1 null))

(define (union-set s0 s1)
  (define (integrate set0 set1)
    (cond
      [(empty? set0) set1]
      [else (integrate (cdr set0) (adjoin-set (car set0) set1))]))
  (define (put-together set0 set1 carry)
    (cond
      [(ormap empty? (list set0 set1)) (integrate set1 carry)]
      [(elem-of-set? (car set0) set1) (put-together (cdr set0) set1 carry)]
      [else (put-together (cdr set0) set1 (append carry (list (car set0))))]))
  (put-together s0 s1 null))
      

;; Efficiency? Which is better when?

;; Exercise 2.61
;; I did this above by accident.
;; Todo: talk about efficiency ... 

;; Exercise 2.62
;; Give an O(n) implementation of union-set for sets as ordered lists.
;; I guess n = length(lst0) + length(lst1)
;; I think my solution above is O(n), so I won't do this.

;; Exercise 2.63
;; blah

;; Exercise 2.64

;; Exercise 2.65