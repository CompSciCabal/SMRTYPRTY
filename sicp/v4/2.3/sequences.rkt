#lang racket

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else  (filter predicate
                       (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low
            (enumerate-interval
              (+ low 1)
              high))))

;; 2.33
;; Later we need to use the more general system map function.
;; Uncomment if you want to test below.
#| (define (map p sequence) |#
#|   (accumulate (lambda (x y) |#
#|                 (cons (p x) y)) |#
#|               null sequence)) |#
;; Later we need to use a different append.
#| (define (append seq1 seq2) |#
#|   (accumulate cons seq2 seq1)) |#

(define (length sequence)
  (accumulate (lambda (el counter) (+ counter 1)) 0 sequence))

#| (map (lambda (x) (* x x)) (list 1 2 3 4 5)) |#
#| (append (list 1 2 3 4 5) (list 6 7 8 9 10)) |#
#| (length (list 1 2 3 4)) |#


;; 2.34
(define
  (horner-eval x coeff-seq)
  (accumulate
    (lambda (coeff higher-terms)
      (+ coeff (* higher-terms x)))
    0
    coeff-seq))

#| (horner-eval 2 (list 1 3 0 5 0 1)) ; should be 79 |#


;; 2.35
;; from 2.2
(define (join lst1 lst2)
  (cond [(null? lst1) lst2]
        [else (cons (car lst1)
                    (append (cdr lst1) lst2))]))
(define (fringe tree)
  (cond [(empty? tree) empty]
        [(pair? tree) (join (fringe (car tree))
                            (fringe (cadr tree)))]
        [else (list tree)]))

#| (define (count-leaves t) |#
#|   (accumulate (lambda (x y) (+ 1 y)) ; the increment function |#
#|               0 |#
#|               (fringe t))) |#
;; hmm, or...
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x) 1)
                   (fringe t))))

(define simple-tree (list (list 2 5)
                          (list 1 3)))
(define my-tree (list
                  (list
                    (list 4
                          (list 2 9))
                    2)
                  (list 6
                        (list
                          (list 5
                            (list 2 4))
                          2))))
#| (count-leaves simple-tree) |#
#| (count-leaves my-tree) |#


;; 2.36
(define (accumulate-n op init seqs)
  (if (empty? (car seqs))
      empty
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

#| (define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) |#
#| (accumulate-n + 0 s) |#


;; 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row)
         (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons empty mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col)
                  (dot-product row col))
                cols)) m)))

#| (define m '((1 2 3 4) (4 5 6 6) (6 7 8 9))) |#
#| (define i '((1 0 0 0) (0 1 0 0) (0 0 1 0) (0 0 0 1))) |#
#| (define v '(2 1 10 20)) |#
#| (define r1 '(1 2 3 4)) |#
#| (define r2 '(4 5 6 6)) |#
#| (define r3 '(6 7 8 9)) |#
#| (dot-product v r1) ;; should be 2+2+30+80 = 114 |#
#| (dot-product v r2) ;; should be 193 |#
#| (dot-product v r3) ;; should be 279 |#
#| (matrix-*-vector m v) ;; should be '(114 193 279) |#
#| (transpose m) ;; should be '((1 4 6) (2 5 7) (3 6 8) (4 6 9)) |#
#| (matrix-*-matrix m i) ;; should be m |#


;; 2.38
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op
                      initial
                      (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; the operation needs to be associative (grouping does not matter)
;; Note that I did not say commutative; matrix multiplication for example will work
;; fine with either fold-right or fold-left.

#| (fold-right / 1 (list 1 2 3)) ; should give 1/(2/3) = 3/2 |#
#| (fold-left  / 1 (list 1 2 3)) ; should give (1/2)/3 = 1/6 |#
#| (fold-right list empty (list 1 2 3)) ; should be (list 1 (list 2 (list 3 (list empty)))) |#
#|                                      ; or '(1 (2 (3 ()))) |#
#| (fold-left  list empty (list 1 2 3)) ; (list (list (list (list empty) 1) 2) 3) |#
#|                                      ; or '(((() 1) 2) (3)) |#


;; 2.39
; redefine append as before so second arg is an element
(define (append lst el)
  (cond [(empty? lst) (cons el empty)]
        [else (cons (car lst) 
                    (append (cdr lst) el))]))
(define (reverse sequence)
  (fold-right 
   (lambda (x y) (append y x)) null sequence))

#| (define (reverse sequence) |#
#|   (fold-left |# 
#|    (lambda (x y) (cons y x)) null sequence)) |#

(reverse (list 1 2 3 4))
