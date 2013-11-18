#lang racket

;; Example functions
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) 
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ 1 low) high))))

(define (enumerate-tree tree)
  (cond
    [(null? tree) tree]
    [(not (pair? tree)) (list tree)]
    [else (append (enumerate-tree (car tree))
                  (enumerate-tree (cdr tree)))]))

(define (sum-off-squares tree)
  (accumulate +
              0
              (map sqr (filter odd?
                                  (enumerate-tree tree)))))

;; "Modular construction is a powerful strategy for controlling complexity in
;; enigineering design. In real signal-processing applications, for example.
;; designers regularly build systems by cascading elements selected from stand-
;; ardized families of filters and transducers."

(define (list-fib-squares n sequence)
  (accumulate *
              1
              (map sqr
                   (filter odd? sequence))))

;; Exercise 2.33
(define (map-new p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(define (append-new seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-new sequence)
  (accumulate (lambda (x y) (+ 1 (length-new (cdr sequence)))) 0 sequence))

;; Exercise 2.34
;; original attempt, which returns the correct value sometimes, but sometimes
;; it's doubled ...
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-term)
                (+ (* this-coeff x)
                   (* x (horner-eval x (cdr coefficient-sequence)))))
              0
              coefficient-sequence))

\
(define (horner-eval-2 x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-term)
 ;               (display "this-coeff is ")(display this-coeff)(newline)
  ;              (display "higher-term is ")(display higher-term)(newline)
                (+ (* x higher-term)
                   this-coeff))
              0
              coefficient-sequence))

(define (pow base exp)
  (if (eq? 0 exp)
      1
      (* base (pow base (- exp 1)))))

;; (horner-eval 2 (list 1 3 0 5 0 1))
;; (+ 1 6 (* 5 (pow 2 3)) (pow 2 5)) => 79

;; 2.35
(define (fringe lst)
  (cond
    [(empty? lst) empty]
    [(integer? lst) (list lst)]
    [(list? (car lst)) (fringe (append (car lst) (cdr lst)))]
    [else (cons (first lst) (fringe (cdr lst)))]))

;; I think this is correct: 
(define (new-count-leaves t)
  (accumulate (lambda (x y) (+ y 1)) 0 (fringe t)))

;; Here is a *very* silly version with map, as the exercise wants it done:
(define (map-count-leaves t)
  (accumulate (lambda (x y) (+ y 1)) 0 (map (lambda (x) x) (fringe t))))

;(define (count-leaves x)
;  (cond ((null? x) 0)
;        ((not (pair? x)) 1)
;        (else (+ (count-leaves (car x))
;                 (count-leaves (cdr x))))))

;; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define (transpose m)
  (accumulate-n cons null m))
  
;(define (matrix-*-matrix m n)
 ; (let ((cols (transpose n)))
  ;  (map <> m)))
