#lang racket

(define (inc n) (+ n 1))
(define (dec n) (- n 1))

(define id identity)
(define (apply1 p a) (p a))

(define (not-in item seq)
  (not (member item seq)))

(define (square x) (* x x))

;; Exercise 2.17, p.103
;; Last element of the list
(define (last-pair lst)
  (cond ((null? lst) null)
        ((null? (cdr lst)) (car lst))
        (else (last-pair (cdr lst)))))

;(= 34 (last-pair (list 23 72 149 34)))
;(= 34 (last-pair (list 34)))
;(null? (last-pair null))

;; Exercise 2.18, p.103
;; Reversing list
(define (my-reverse lst)
  (define (iter li acc)
    (if (null? li)
        acc
        (iter (cdr li) (cons (car li) acc))))
  (iter lst null))

;; Tests
;(let ((lst (list 25 16 9 4 1)))
;  (equal? (reverse lst) (my-reverse lst)))

;; Exercise 2.19, p.103
;; Counting change (see also ndpar-1.2.tex)
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define no-more? null?)
(define except-first-denomination cdr)
(define first-denomination car)

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;; Order of the coins does NOT affect the answer.
;(= 292 (cc 100 us-coins) (cc 100 (reverse us-coins)))

;; Exercise 2.20, p.104
;; Varargs: dotted-tail notation
(define (same-parity x . y)
  (define (filter li acc)
    (cond ((null? li) (reverse acc))
          ((even? (- x (car li))) (filter (cdr li)
                                          (cons (car li) acc)))
          (else (filter (cdr li) acc))))
  (filter y (list x)))

;(equal? (same-parity 1 2 3 4 5 6 7) (list 1 3 5 7))
;(equal? (same-parity 2 3 4 5 6 7) (list 2 4 6))

;; Exercise 2.21, p.106
;; Map function
(define (square-list items)
  (map square items))

(define (square-list-2 items)
  (if (null? items)
      null
      (cons (square (car items))
            (square-list-2 (cdr items)))))

;(equal? (list 1 4 9 16)
;        (square-list (list 1 2 3 4)))
;(equal? (list 1 4 9 16)
;        (square-list-2 (list 1 2 3 4)))

;; Exercise 2.22, p.106
;; Iterative map
(define (square-list-3 items)
  (define (iter li acc)
    (if (null? li)
        (flatten acc) ; otherwise it's deeply unproper list
        (iter (cdr li)
              (cons acc (square (car li))))))
  (iter items null))

;(equal? (list 1 4 9 16)
;        (square-list-3 (list 1 2 3 4)))

;; Exercise 2.23, p.107
;; Iterating for side effects
(define (for-each p items)
  (cond ((null? items) #t)
        (else
         (p (car items))
         (for-each p (cdr items)))))

;(for-each (lambda (x) (newline) (display x))
;          (list 57 321 88))

;; Exercise 2.25, p.110
(define (cadaddr x)
  (car (cdaddr x)))

(define (cadadadadadadr x)
  (cadadr (cadadr (cadadr x))))

;(cadaddr '(1 3 (5 7) 9))
;(caar '((7)))
;(cadadadadadadr '(1 (2 (3 (4 (5 (6 7)))))))

;; Exercise 2.26, p.110
;(define x (list 1 2 3))
;(define y (list 4 5 6))

;(append x y)
;(cons x y)
;(list x y)

;; Exercise 2.27, p.110
;; Reversing trees
(define (deep-reverse-2 tree)
  (define (iter li acc)
    (cond ((null? li) acc)
          ((pair? (car li))
           (iter (cdr li)
                 (cons (deep-reverse-2 (car li)) acc)))
          (else (iter (cdr li)
                      (cons (car li) acc)))))
  (iter tree null))

(define (deep-reverse tree)
  (define (iter li acc)
    (if (null? li)
        acc
        (iter (cdr li)
              (cons (deep-reverse (car li))
                    acc))))
  (if (pair? tree)
      (iter tree null)
      tree))

;(equal? '((4 3) (2 1))
;        (deep-reverse '((1 2) (3 4))))

;; Exercise 2.28, p.111
(define (fringe tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

;(define x '((1 2) (3 4)))
;(equal? '(1 2 3 4) (fringe x))
;(equal? '(1 2 3 4 1 2 3 4)
;        (fringe (list x x)))
;(equal? '(1 2 3 4 1 2 3 4)
;        (fringe '((1 ((2 ((3 (4 1)) 2)) 3)) 4)))

;; Exercise 2.29, p.111
(define (make-mobile left right)
  (list left right))

(define left-branch car)
(define right-branch cadr)

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

;; structure is either number (weight)
;; or another mobile
(define (make-branch length structure)
  (list length structure))

(define mobile-structure? pair?)

(define branch-length car)
(define branch-structure cadr)

(define (branch-weight branch)
  (let ((struct (branch-structure branch)))
    (if (mobile-structure? struct)
        (total-weight struct)
        struct)))

(define (torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (branch-balanced? branch)
  (let ((struct (branch-structure branch)))
    (if (mobile-structure? struct)
        (balanced? struct)
        #t)))

;; Mobile is balanced iff
;; 1) left and right torques are equal
;; 2) all sbumobiles are balanced
(define (balanced? mobile)
  (let ((lbranch (left-branch mobile))
        (rbranch (right-branch mobile)))
    (and (= (torque lbranch) (torque rbranch))
         (branch-balanced? lbranch)
         (branch-balanced? rbranch))))

;; Alternative representation
;(define (make-mobile left right)
;  (cons left right))
;(define right-branch cdr) ; cadr -> cdr

;(define (make-branch length structure)
;  (cons length structure))
;(define branch-structure cdr) ; cadr -> cdr

;; Tests
;(define mobile (make-mobile (make-branch 10 3)
;                            (make-branch 5 (make-mobile (make-branch 1 4)
;                                                        (make-branch 2 2)))))

;(= 9 (total-weight mobile))
;(balanced? mobile)

;; Exercise 2.30, p.112
;; Squaring trees
(define (square-tree-1 tree)
  (map (lambda (x)
         (if (pair? x)
             (square-tree-1 x)
             (square x)))
       tree))

(define (square-tree-2 tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-2 (car tree))
                    (square-tree-2 (cdr tree))))))

;; Exercise 2.31, p.113
;; Mapping over trees
(define (tree-map f tree)
  (map (lambda (x)
         (if (pair? x)
             (tree-map f x)
             (f x)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

;; Tests
(let ((tree '(1 (2 (3 4) 5) (6 7)))
      (result '(1 (4 (9 16) 25) (36 49))))
  (and (equal? result (square-tree   tree))
       (equal? result (square-tree-1 tree))
       (equal? result (square-tree-2 tree))))

;; Exercise 2.32, p.113
;; Set of all subsets
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rst (subsets (cdr s))))
        (append rst (map (lambda (x) (cons (car s) x))
                         rst)))))

;; Tests
(equal? '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
        (subsets '(1 2 3)))

;; Exercise 2.33, p.119
;; Accumulate (right fold) as a fundametal operation of FP
(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define fold-right accumulate)

(define (map-2 p seq)
  (accumulate (lambda (x y) (cons (p x) y)) null seq))

(define (append-2 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-2 seq)
  (accumulate (lambda (_ n) (inc n)) 0 seq))

;; Tests
(equal? '(1 4 9 16) (map-2 square '(1 2 3 4)))
(equal? '(1 2 3 4) (append-2 '(1 2) '(3 4)))
(= 4 (length-2 '(1 2 3 4)))

;; Exercise 2.34, p.119
;; Horner's rule
(define (horner-eval x coefficients)
  (accumulate (lambda (coeff higher-terms)
                (+ (* higher-terms x) coeff))
              0
              coefficients))

;; Tests
(= 79 (horner-eval 2 '(1 3 0 5 0 1)))

;; Exercise 2.35, p.120
(define (count-leaves tree)
  (accumulate + 0 (map (lambda (t)
                         (if (pair? t)
                             (count-leaves t)
                             1))
                       tree)))

;; Tests
(let ((x '((1 2) (3 4))))
  (= 8 (count-leaves (list x x))))

;; Exercise 2.36, p.120
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; Tests
(equal? (accumulate-n + 0 '(( 1  2  3)
                            ( 4  5  6)
                            ( 7  8  9)
                            (10 11 12)))
        '(22 26 30))

;; Exercise 2.37, p.120
;; Vector algebra
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define (transpose m)
  (accumulate-n cons null m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

;; Tests
(define a '((1 2 3 4)
            (4 5 6 6)
            (6 7 8 9)))

(define b '(( 1  2  0)
            ( 2  1 -1)
            (-1  1  2)
            ( 0 -1  1)))

(define v '(1 0 -1 2))

(equal? (matrix-*-vector a v)
        '(6 10 16))

(equal? (transpose a)
        '((1 4 6)
          (2 5 7)
          (3 6 8)
          (4 6 9)))

(equal? (matrix-*-matrix a b)
        '(( 2  3  8)
          ( 8 13 13)
          (12 18 18)))

;; Exercise 2.38, p.121
(define (fold-left op initial seq)
  (define (iter acc rest)
    (if (null? rest)
        acc
        (iter (op acc (car rest))
              (cdr rest))))
  (iter initial seq))

;; Tests
(= 3/2 (fold-right / 1 '(1 2 3)))
(= 1/6 (fold-left  / 1 '(1 2 3)))

(equal? (fold-right list null '(1 2 3))
        '(1 (2 (3 ()))))
(equal? (fold-left list  null '(1 2 3))
        '(((() 1) 2) 3))

;; For commutative operations left fold
;; yields the same result as right fold
(equal? (fold-right * 1 '(1 2 3))
        (fold-left  * 1 '(1 2 3)))

;; Exercise 2.39, p.122
;; Reverse through fold
(define (reverse-right seq)
  (fold-right (lambda (x acc) (append acc (list x)))
              null
              seq))

(define (reverse-left seq)
  (fold-left (lambda (acc x) (cons x acc))
             null
             seq))

;; Tests
(let ((lst (list 25 16 9 4 1)))
  (and (equal? (reverse lst) (reverse-right lst))
       (equal? (reverse lst) (reverse-left  lst))))

;; Flatmap
(define (flatmap f seq)
  (fold-right append null (map f seq)))

;; Exercise 2.40, p.124
;; { (i,j) : 1 ≤ j < i ≤ n }
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (range 1 i)))
           (range 2 (inc n))))

;; Tests
(equal? '((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4))
        (unique-pairs 5))

;; Exercise 2.41, p.124
(define (exercise-2.41 s n)
  (filter-sum s (unique-triples n)))

(define (filter-sum s seq)
  (filter (lambda (c) (= s (sum c)))
          seq))

(define (sum seq) (fold-left + 0 seq))

(define (unique-triples n)
  (flatmap (lambda (pair)
             (map (lambda (k) (cons k pair))
                  (range (inc (car pair)) (inc n))))
           (unique-pairs (dec n))))

;; Tests
(equal? '((5 2 1) (4 3 1))
        (exercise-2.41 8 5))

;; Exercise 2.42, p.124
;; Eight-queens puzzle
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (range 1 (inc board-size))))
          (queen-cols (dec k))))))
  (queen-cols board-size))

;; NB: In the following procedures K is not used!
;; What data structure requires K?
(define empty-board null)

(define (adjoin-position new-row _ rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? _ positions)
  ; queen checks horizontally and diagonally
  (define (next-checked-positions positions)
    (map apply1 (list dec id inc) positions))
  (define (safe-positions? checked-positions positions)
    (if (null? positions)
        #t
        (and (not-in (car positions) checked-positions)
             (safe-positions? (next-checked-positions checked-positions)
                              (cdr positions)))))
  (let ((pos (car positions)))
    (safe-positions? (next-checked-positions (list pos pos pos))
                     (cdr positions))))

;; Tests
(= (length (queens 0)) 1)
(= (length (queens 1)) 1)
(= (length (queens 2)) 0)
(= (length (queens 3)) 0)
(= (length (queens 4)) 2)
(= (length (queens 5)) 10)
(= (length (queens 6)) 4)
(= (length (queens 7)) 40)
(= (length (queens 8)) 92)
;(member '(6 2 7 1 4 8 5 3) (queens 8))
(equal? '((5 3 1 6 4 2) (4 1 5 2 6 3) (3 6 2 5 1 4) (2 4 6 1 3 5))
        (queens 6))

;; Exercise 2.43, p.126
;; It seems that Luois' time is O(n^n),
;; while the original time is O(n^2).
;; I don't have formal proof yet.
