;; 2.17 last-pair of list

(define (last-pair l)
  (if (null? (cdr l))
    l
    (last-pair (cdr l))))
(last-pair (list 1 4 "reggie" 7))
;; 2.18 reverse list

(define (reverse-list l)
  (define (iter r l)
    (if (null? l)
      r
      (iter (cons (car l) r) (cdr l))))
  (iter (list) l))
(reverse-list (list 3 1 99 3 7))
;; 2.19 Coin Changing

(define us-coins
  (list 50 25 10 5 1))
(define can-coins
  (list 50 25 10 5))
(define uk-coins
  (list 100 50 20 10 5 2 1 0.5))
(define (cc amount coin-values)
  (define (first-denomination l)
    (car l))
  (define (except-first-denomination l)
    (cdr l))
  (define (no-more? l)
    (null? l))
  (cond
    ((= amount 0) 1)
    ((or (< amount 0) (no-more? coin-values)) 0)
    (else
      (+
        (cc amount (except-first-denomination coin-values))
        (cc
          (- amount (first-denomination coin-values))
          coin-values)))))
;(cc 100 us-coins)

;(cc 100 (reverse-list us-coins))

;(cc 100 can-coins)

;(cc 100 (reverse-list can-coins))

; no the order shouldnt affect the answer since we exhaustively try all possibilities

;; 2.20 dotted tail notation

(define (semi-parity i . l)
  (let ((parity? (if (even? i)
    even?
    odd?)))
    (define (next-cons l)
      (cond
        ((null? l) l)
        ((parity? (car l)) (cons (car l) (next-cons (cdr l))))
        (else (next-cons (cdr l)))))
    (cons i (next-cons l))))
;(semi-parity)

(semi-parity 1 2 3 6 6 4 7)
;; 2.21 square-list with map

(define (map-list proc items)
  (if (null? items)
    items
    (cons (proc (car items)) (map-list proc (cdr items)))))
(map-list abs (list -1 2 -4 -5 3.4 5.5 -10.6))
(define (square-list l)
  (map-list (lambda (x)
    (* x x))
            l))
(square-list (list 1 2 2.2 6 9 -2.3))
(define (square-list l)
  (if (null? l)
    l
    (cons (* (car l) (car l)) (square-list (cdr l)))))
(square-list (list 1 2 2.2 6 9 -2.3))
;; 2.22 Reversed List

; Louis first approach is appending to the front of the list so it gets reversed.

; His second answer will return the pair (nil, square(last-element)).

; It doesn't build a list.

;; 2.23 for-each

(define (for-each-list proc items)
  (if (null? items)
    #t
    ((lambda ()
      (proc (car items))
      (for-each-list proc (cdr items))))))
;(for-each-list (lambda (x)

;  (display x)

;  (newline))

;               (list "hey!" "Ho!"))

;; 2.24 its lists all the way down

(list 1 (list 2 (list 3 4)))
(cons 1 (cons
  (cons 2 (cons (cons 3 (cons 4 (list))) (list)))
  (list)))
; L1: (1, ->) (-> L2, nil)

; L2: (2, -> ) (-> L3, nil)

; L3: (3, ->) (4, nil)

; L1

; 1 , L2

;    , 2 , L3

;    ,    ,  3 , 4  

;; 2.25 Find the 7!

(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))
(car (car (list (list 7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (list
  1
  (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))
;; 2.26 

(define x
  (list 1 2 3))
(define y
  (list 4 5 6))
(append x y)
; (1 2 3 4 5 6)

(cons x y)
; ((1 2 3) 4 5 6)

(list x y)
; ((1 2 3) (4 5 6))

;; 2.27 Deep Reverse

(define (deep-reverse-list l)
  (define (iter r l)
    (cond
      ((null? l) r)
      ((pair? (car l)) (iter (cons (iter (list) (car l)) r) (cdr l)))
      (else (iter (cons (car l) r) (cdr l)))))
  (iter (list) l))
(deep-reverse-list
  (list
    1
    (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(deep-reverse-list (list 1 2 (list 3 4) (list 5 6)))
;; 2.28 tree fringe

(define (fringe l)
  (cond
    ((null? l) l)
    ((pair? (car l)) (append (fringe (car l)) (fringe (cdr l))))
    (else (cons (car l) (fringe (cdr l))))))
(fringe (list (list 1 2) (list 3 4)))
(fringe
  (list
    1
    (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
;; 2.29 Binary Mobile

; with lists

(define (make-mobile left right)
  (list left right))
(define (mobile? m)
  (pair? m))
(define (make-branch length structure)
  (list length structure))
; 2.29 a) selectors with list implementation

(define (left-branch m)
  (car m))
(define (right-branch m)
  (car (cdr m)))
(define (branch-length b)
  (car b))
(define (branch-structure b)
  (car (cdr b)))
;; 2.29 b) Computing the total weight

(define (total-weight m)
  (define (branch-weight b)
    (let ((m (branch-structure b)))
      (if (mobile? m)
        (total-weight m)
        m)))
  (+
    (branch-weight (left-branch m))
    (branch-weight (right-branch m))))
(define x
  (make-mobile
    (make-branch
      3
      (make-mobile (make-branch 2 4) (make-branch 3 3)))
    (make-branch 10 1)))
x
(total-weight x)
;; 2.29 c) Balanced Mobile

(define (balanced? m)
  (define (balanced-branch b)
    (let ((m (branch-structure b)))
      (if (mobile? m)
        (balanced-weight m)
        (cons #t m))))
  (define (balanced-weight m)
    (let ((bwl (balanced-branch (left-branch m)))
    (bwr (balanced-branch (right-branch m))))
      (let ((bl (car bwl))
      (wl (cdr bwl))
      (br (car bwr))
      (wr (cdr bwr)))
        (cons
          (and
            bl
            br
            (=
              (* (branch-length (left-branch m)) wl)
              (* (branch-length (right-branch m)) wr)))
          (+ wl wr)))))
  (car (balanced-weight m)))
(balanced? x)
(define bx
  (make-mobile
    (make-branch
      3
      (make-mobile (make-branch 1 0.5) (make-branch 1 0.5)))
    (make-branch 3 1)))
(balanced? bx)
;;;

;;;

;;;

;;;

;;;

;;;

;;;
