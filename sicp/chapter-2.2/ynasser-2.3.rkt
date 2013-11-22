#lang racket

;; Things to make sicp scheme compatible with racket
(define first car)
(define rest cdr)
(define null '())
(define nil null)
(define empty? null?)
(define sqr (lambda (x) (* x x)))
(define (filter op lst)
  (cond 
    ((empty? lst) null)
    ((op (car lst)) (cons (car lst) (filter op (cdr lst))))
    (else (filter op (cdr lst)))))

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

;; Exercise 2.33
(define (map-new p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))

(define (append-new seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-new sequence)
  (accumulate (lambda (x y) (+ 1 (length-new (cdr sequence)))) 0 sequence))

;; Exercise 2.34
(define (horner-eval-2 x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-term) (+ (* x higher-term)
                                                  this-coeff))
              0
              coefficient-sequence))

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

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(define m '((1 2 3) (4 5 6) (7 8 9)))

;; exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
(define (fold-right op initial sequence)
  (fold-left op initial (reverse sequence)))

(define foldl fold-left)
(define foldr fold-right)

;(foldr / 1 (list 1 2 3)) => 1.5
;(foldl / 1 (list 1 2 3)) => 1.5
;(foldr list null (list 1 2 3)) => '(1 (2 (3 ())))
;(foldl list null (list 1 2 3)) => '(3 (2 (1 ())))

;; Q: What property should op satisfy for foldr and foldl to have the same
;; results?
;; A: (op A B) == (op B A)

;; Exercise 2.39
(define (reverse-right sequence)
  (foldr (lambda (x y) (append y (list x))) null sequence))

(define (reverse-left sequence)
  (foldl (lambda (x y) (cons x y)) null sequence))

;; Exercise 2.40
;; define unique-pairs:
;; given n, it generates all pairs (i,j) st 1<=j<i<=n
;; example: n = 4
;; (4,3) (4,2) (4,1)
;; (3,2) (3,1)
;; (2,1)
(define (unique-pairs n)
  (cond
    [(>= 1 n) null]
    [else (append 
          (map (lambda (x) (list n x)) (build-list (- n 1) (lambda (x) (+ 1 x))))
          (unique-pairs (- n 1)))]))

;; Use unique-pairs to simplify the definition of prime-sum-pairs.

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime? n)
  (empty? (cdr (filter (lambda (x) (and (not (equal? n x))
                           (equal? 0 (remainder n x))))
          (build-list n (lambda (y) (+ 1 y)))))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (make-pair-sum-new pair)
  (list (car pair) (cdr pair) (+ (car pair) (cdr pair))))

(define (prime-sum-pairs-original n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))


;; simplified version of prime-sum-pairs
(define (prime-sum-pairs n)
  (map make-pair-sum-new (filter (lambda (x) (prime? (+ (car x) (cdr x))))
                                 (unique-pairs n))))

;; Exercise 2.41
;; Write a procedure to find all ordered triples of distinct positive integers
;; i, j, and k less than or equal to a given integer n that sums to a given 
;; integer s.
;; constraints: i + j + k = s and

(define (happy-sum? sum nums)
  (equal? (accumulate + 0 nums) sum))

(define (fragment orig lst)
  (cond
    [(empty? orig) lst]
    [else (fragment (rest (rest (rest orig))) (cons (list (car orig) (first (rest orig)) (first (rest (rest orig)))) lst))]))

(define (generate-filling-old item)
  (cond
    [(< (first item) (second item)) #f]
    [else
     (define lst (rest (build-list (- (car item) (car (cdr item))) (lambda (x) (+ x (car (cdr item)))))))
     (display "list is ")(display lst)(newline)
     (define clump (accumulate (lambda (x y) (cons (first item) (cons x (cons (car (cdr item)) y)))) null lst))
     (fragment clump empty)]))

(define (generate-filling item)
  (range (+ 1 (first item)) (second item)))

(define f (unique-pairs 5))

(define (unique-triplets n)
  (accumulate (lambda (x y) (append (generate-filling-old x) y)) null (unique-pairs n)))

(define (exercise-2.41 n sum)
  (filter (lambda (x) (happy-sum? sum x)) (unique-triplets n)))

;; There must be a more succinct solution!

;; Exercise 2.42
;; The Eight Queens Puzzle
(define (safe? k position) 'dog)
(define (adjoin-position new-row k rest-of-queens) 'dog)

(define (queens board-size)
  (define empty-board 
    (build-list board-size (lambda (y) (build-list board-size (lambda (x) (list))))))
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; todo
;;(define (adjoin-position))

;; returns bool
;;(define (safe? k position))
