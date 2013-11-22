#lang racket

;;; Reading Notes

; 133.6: "closure" (losing battle) 
;        [can everything in a language be closed?]
; 137.10: nil wars
; 142.11: It seems a little strange that (define g (lambda w w))
;         captures its arguments as a list...
; 144: what are some other common abstraction barriers like map? 
;      how do the different ways of decomposing abstraction effect
;      our ability to effectively reason about different problem types?



;;; PRELUDE

(define nil '())
(define (filter predicate sequence) 
  (cond ((null? sequence) '())
        ((predicate (car sequence)) 
         (cons (car sequence)
               (filter predicate (cdr sequence)))) 
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence) 
  (if (null? sequence)
      initial 
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high) 
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree) 
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree)) 
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;;; 2.17

(define (last-pair xs)
  (let ((x (car xs)))
    (if (null? (cdr xs))
        x
        (last-pair (cdr xs)))))

;;; 2.18

(define (reverse xs)
  (define (rev-iter xs acc)
    (if (null? xs)
        acc
        (rev-iter (cdr xs) (cons (car xs) acc))))
  (rev-iter xs '()))

;;; 2.19

(define (cc amount coin-values) 
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount 
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination 
                    coin-values))
                coin-values)))))

(define (first-denomination xs) (car xs))
(define (except-first-denomination xs) (cdr xs))
(define (no-more? xs) (null? xs))

; Order is unimportant in the denomination list: at each step we 
; try making change with a denom and without it, so eventually 
; every denom is represented equally.

;;; 2.20

(define (same-parity . w)
  (define (filter-by-mod base mod xs acc)
    (if (null? xs) 
        acc
        (filter-by-mod base mod (cdr xs) 
                       (if (= (remainder (car xs) base) mod)
                           (cons (car xs) acc)
                           acc))))
  (filter-by-mod 2 (remainder (car w) 2) (cdr w) '()))

; Q: how do you do two things in an 'else' statement?

;;; 2.21

(define (square n)
  (* n n))

(define (square-list items) 
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items))))) 

(define (square-list2 items)
  (map square items))
    
;;; 2.22

; Q: evolves an iterative process?

; we're popping off items the front of things
; onto the front of answer, which reverses it

; cons'ing a list onto a single item gives a pair of '(list item)

;;; 2.23

(define (foreach fun xs)
  (if (null? xs)
      #t
      (and 
       (fun (car xs))
       (foreach fun (cdr xs)))))

;;; 2.24

; can we generalize the types of tree recursion
; (i.e. fun leaves, fun branches, fun both, ... )

; Q: why does pair? return #t for '(1 2 3) and '(1) but not '() ???

; > (list 1 (list 2 (list 3 4)))
; '(1 (2 (3 4)))
; [this margin is too small]

;;; 2.25

; (car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
; (cadr (caddr '(1 3 (5 7) 9)))
; (caar '((7)))
; (cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7)))))))))

;;; 2.26

; > (append x y)
; '(1 2 3 4 5 6)
; > (cons x y)
; '((1 2 3) 4 5 6)
; > (list x y)
; '((1 2 3) (4 5 6))

;;; 2.27

(define (deep-reverse xs)
  (if (not (pair? xs))
      xs
      (if (pair? (car xs))
          (cons (deep-reverse (cdr xs)) (deep-reverse (car xs)))
          (cons (deep-reverse (cdr xs)) (car xs)))))
      

(define (revrec xs)
  (if (pair? xs)
      (append (revrec (cdr xs)) (list (car xs)))
      '()))


;;; 2.28

; (define (fringe xs)

;;; 2.29

; OO for selectors... encapsulate the data with the abstraction layer

; why doesn't the top have a rod?
; what does length+number mean? is it a weighted rod?
; O I C

;;; 2.30







;;;;;;;;; 2.2.3 ;;;;;;;;;;;;

; 157: what's the downside to signal-flow decomposition? 
;      how far does it scale? [different levels?]
; 159: diagrams vs code...

;;; 2.33

(define (map2 p sequence) 
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(define (append2 seq1 seq2) 
  (accumulate cons seq2 seq1))
(define (length2 sequence) 
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;;; 2.34

(define (horner-eval x coefficient-sequence) 
  (accumulate (lambda (this-coeff higher-terms) 
                (+ this-coeff (* x higher-terms)))
              0 
              coefficient-sequence))

;;; 2.35

(define (count-leaves t) 
  (accumulate + 0
              (map (lambda (x) (if (pair? x) 
                                   (count-leaves x) 1)) t)))

;;; 2.36

(define (accumulate-n op init seqs) 
  (if (null? (car seqs))
      '() 
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;;; 2.37

(define (matrix-*-vector m v) 
  (map (lambda (mi) (accumulate + 0 (map * mi v))) m))
(define (transpose mat) 
  (accumulate-n cons '() mat))
(define (matrix-*-matrix m n) 
  (let ((cols (transpose n)))
    (map (lambda (mi) (matrix-*-vector cols mi)) m)))

; (matrix-*-matrix '((1 1 1 4) (1 2 3 4) (5 5 5 4))
;                  '((1 1 5) (1 2 5) (1 3 5) (4 4 4)))
; -- ((19 22 31) (22 30 46) (31 46 91))

;;; 2.38

(define (fold-left op initial sequence) 
  (define (iter result rest)
    (if (null? rest) result
        (iter (op result (car rest)) 
              (cdr rest))))
  (iter initial sequence))

; 3/2, 1/6, (1 (2 (3 ()))), (((() 1) 2) 3)
; op should be communtative:
; (op a (op b c)) === (op (op a b) c)

;;; 2.39

(define fold-right accumulate)
(define (reverse3 sequence) 
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
(define (reverse4 sequence) 
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;;; 2.40

(define (flatmap proc seq) 
  (accumulate append nil (map proc seq)))

(define (permutations s) 
  (if (null? s)	
      (list nil) 
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p)) 
                      (permutations (remove x s))))
               s)))

; is tomfoolery like (list nil) useful or too low level for gpp?

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j)) 
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; yuck so many old includes just to test this
(define (divides? a b) (= (remainder b a) 0))
(define (find-divisor n test-divisor) 
  (cond ((> (square test-divisor) n) n) 
        ((divides? test-divisor n) test-divisor) 
        (else (find-divisor n (+ test-divisor 1)))))
(define (smallest-divisor n) (find-divisor n 2))
(define (prime? n) (= n (smallest-divisor n)))
(define (prime-sum? pair) (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair) 
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n) 
  (map make-pair-sum
       (filter prime-sum? 
               (unique-pairs n))))

;;; 2.41

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k)) 
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (sum-to n s)
  (map (lambda (triple) (append triple (list (accumulate + 0 triple))))
       (filter (lambda (triple) (= (accumulate + 0 triple) s))
               (unique-triples n))))
          
;;; 2.42

(define (queens board-size) 
  (define (queen-cols k)
    (if (= k 0) 
        (list empty-board) 
        (filter
         (lambda (positions) (safe? k positions)) 
         (flatmap
          (lambda (rest-of-queens) 
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size))) 
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (adjoin-position row k rest)
  (append rest (list row)))

(define (safe? k positions)
  (define val (car (reverse positions)))
  (define (safe-val? val offset positions)
    (if (= offset 0)
        #t
        (and (not (= (car positions) val))
             (not (= (car positions) (+ val offset)))
             (not (= (car positions) (- val offset)))
             (safe-val? val (- offset 1) (cdr positions)))))
  (safe-val? val (- (length positions) 1) positions))
    
; why k???

;;; 2.43

(define (slow-queens board-size) 
  (define (queen-cols k)
    (if (= k 0) 
        (list empty-board) 
        (filter
         (lambda (positions) (safe? k positions)) 
         (flatmap 
          (lambda (new-row)
            (map (lambda (rest-of-queens) 
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1)))) 
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

; 1,000 times slower experimentally for 8x8
; 100   times slower for 6x6
; maybe 8 hours for 10x10?
; fast version saves the work done for smaller boards
; slow version does the work over again each time

(define (fast-queens size)
  (define (queen-cols k)
    (if (= k 0)
        (list '())
        (filter 
         fast-safe?
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (append rest-of-queens (list new-row)))
                 (enumerate-interval 1 size)))
          (queen-cols (- k 1))))))
  (queen-cols size))
                   
         
(define (fast-safe? positions)
  (define val (car (reverse positions)))
  (define (safe-val? val offset positions)
    (if (= offset 0)
        #t
        (and (not (= (car positions) val))
             (not (= (car positions) (+ val offset)))
             (not (= (car positions) (- val offset)))
             (safe-val? val (- offset 1) (cdr positions)))))
  (safe-val? val (- (length positions) 1) positions))
