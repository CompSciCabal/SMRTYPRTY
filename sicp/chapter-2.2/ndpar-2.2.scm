#lang racket

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

(equal? '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
        (subsets '(1 2 3)))
