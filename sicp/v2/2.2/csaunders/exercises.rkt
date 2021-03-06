#lang racket
;; Utilities from Previous Sections
(define (prime? n)
  (= n (smallest-divisor n)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ test-divisor 1)))))

(displayln "exercise 2.17")
(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

(last-pair (list 23 72 149 34))

(displayln "exercise 2.18")
(define (reverse lst)
  (define (reverse-iter lst acc)
    (if (null? lst)
        acc
        (reverse-iter (cdr lst)
                      (cons (car lst) acc))))
  (reverse-iter lst '()))

(reverse (list 1 4 9 16 25))

(displayln "exercise 2.19")
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (first-denomination coins)
  (car coins))

(define (except-first-denomination coins)
  (cdr coins))

(define (no-more? coins)
  (null? coins))

(define (cc amount coin-values)
  (cond [(= amount 0) 1]
        [(or (< amount 0) (no-more? coin-values)) 0]
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(cc 100 us-coins)
;; (cc 125 uk-coins)
;; > 287957

(displayln "exercise 2.20")
(define (same-parity first . rest)
  (define (match? a b)
    (= (remainder a 2) (remainder b 2)))
  (define (iter lst)
    (cond [(null? lst) '()]
          [(match? first (car lst))
           (cons (car lst) (iter (cdr lst)))]
          [else (iter (cdr lst))]))
  (cons first (iter rest)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
    
(displayln "exercise 2.21")
(define (square x) (* x x))
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list (cdr items)))))

(define (mapped-square-list items)
  (map square items))

(square-list (list 1 2 3 4))
(mapped-square-list (list 1 2 3 4))

(displayln "exercise 2.22")
(define (sqr-lst items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              ;;Louis First Attempt
              ;;(sqr-lst 1 2 3 4) results in (1 4 9 16)
              ;;(cons answer (square (car things)))
              
              ;;Second Attempt
              ;;(sqr-lst 1 2 3 4) results in:
              ;;((((() . 1) . 4) . 9) . 16)
              (cons answer (square (car things))))))
  (iter items '()))
(sqr-lst (list 1 2 3 4))

;; The reason why this isn't working is because the object that the objects he's appending
;; together aren't cons cells, but simply numbers. Therefore they are shown to be cons pairs
;; instead of a list, as we'd expect. We can get closer to the result we want by ensuring that
;; we cons the answer to a proper cons cell.
;; (cons answer (cons (square (car things)) '()))
;; Unforunately it means that we are essentially doing the following:
;; (cons (cons '() '(1)) '(4))
;; > '((() 1) 4)
;; Louis could simply build up the results in reverse order then call the reverse procedure on
;; them, but that does mean we are doing extra computation for no good reason. He could also use
;; the append procedure from p. 103, though as the results list grows the function will take longer
;; to run

(displayln "exercise 2.23")
(define (for-each proc items)
  (map proc items)
  #t)

(for-each (lambda (x) (displayln x))
          (list 1 2 3 4 5))

(displayln "exercise 2.24")
(displayln "see exercise-2.24.jpg")

(displayln "exercise 2.25")
(define ex-2-25-a '(1 3 (5 7) 9))
(define ex-2-25-b '((7)))
(define ex-2-25-c '(1 (2 (3 (4 (5 (6 7)))))))

;; 2.25 a
(cadr (caddr ex-2-25-a))
;; aka
(car (cdr (car (cdr (cdr ex-2-25-a)))))

;; 2.25 b
(caar ex-2-25-b)
;; aka
(car (car ex-2-25-b))

;; 2.25 c
(cadr (cadr (cadr (cadr (cadr (cadr ex-2-25-c))))))

(displayln "exercise 2.26")

;; x '(1 2 3)
;; y '(4 5 6)

;; (append x y)
;;>(1 2 3 4 5 6)

;; (cons x y)
;;>((1 2 3) 4 5 6)

;; (list x y)
;;>((1 2 3) (4 5 6))

(displayln "exercise 2.27")
(define (deep-reverse x)
  (define (iter lst acc)
    (cond [(null? lst) acc]
          [(pair? (car lst)) (iter (cdr lst)
                                   (cons (deep-reverse (car lst)) acc))]
          [else (iter (cdr lst) (cons (car lst) acc))]))
  (iter x '()))

(deep-reverse (list (list 1 2) (list 3 4)))

(displayln "exercise 2.28")
(define (fringe lst)
  (cond [(null? lst) '()]
        [(not (pair? lst)) (list lst)]
        [else (append (fringe (car lst))
                      (fringe (cdr lst)))]))
(define ex-2-28-x (list (list 1 2) (list 3 4)))
(fringe ex-2-28-x)
(fringe (list (list ex-2-28-x ex-2-28-x) (list 5 6 7 9 (list 10 11 12))))

(displayln "exercise 2.29")
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define ex-2-29-left-branch (make-branch 5 6))
(define ex-2-29-right-branch (make-branch 2 3))

(displayln "exercise 2.29 a")

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (branch? branch)
  (not (pair? (branch-structure branch))))

(define (branch-weight branch)
  (if (branch? branch)
      (branch-structure branch)
      (total-weight (branch-structure branch))))

(define (branch-torque branch)
  (let [(len (branch-length branch))
        (struct (branch-structure branch))]
    (* len
       (if (branch? branch)
           struct
           (let [(tl (branch-torque (left-branch struct)))
                 (tr (branch-torque (right-branch struct)))]
             (if (= tl tr)
                 (+ tl tr)
                 0))))))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(when (not (= (branch-length ex-2-29-left-branch) 5))
  (error "Branch length did not match"))
(when (not (= (branch-structure ex-2-29-left-branch) 6))
  (error "Branch height did not match"))
(when (not (equal? (branch-structure (make-branch 3 ex-2-29-right-branch)) ex-2-29-right-branch))
  (error "Child branch did not match"))

(displayln "exercise 2.29 b")
(define simple (make-mobile (make-branch 5 5) (make-branch 5 5)))
(define airplanes
  (make-mobile
   (make-branch 5
                (make-mobile
                   (make-branch 5 5)
                   (make-branch 5 5)))
   (make-branch 10
                (make-mobile
                    (make-branch 5
                                 (make-mobile (make-branch 3 2)
                                              (make-branch 5 3)))
                    (make-branch 6 5)))))

(define (weird-mobile)
  (make-mobile
   (make-branch 5 480)
   (make-branch 10
                (make-mobile
                 (make-branch 10 (make-mobile (make-branch 3 2) (make-branch 2 3)))
                 (make-branch 6 20)))))
                 
(define (total-weight mobile)
  (cond [(null? mobile) 0]
        [(branch? mobile) (branch-structure mobile)]
        [else
         (let ([left (left-branch mobile)]
               [right (right-branch mobile)])
           (+ (branch-weight left)
              (branch-weight right)))]))

(total-weight (weird-mobile))
(total-weight airplanes)

(displayln "exercise 2.29 c")
(define (balanced? mobile)
  (= (branch-torque (left-branch mobile))
     (branch-torque (right-branch mobile))))

(balanced? simple)
(balanced? airplanes)
(balanced? (weird-mobile))

(displayln "exercise 2.29d")
(set! make-mobile (lambda (l r) (cons l r)))
(set! make-branch (lambda (len str) (cons len str)))

;; Changes to make program work
(set! branch-structure (lambda (b) (cdr b)))
(set! right-branch (lambda (b) (cdr b)))

;; tests
(total-weight (weird-mobile))
(balanced? (weird-mobile))

(displayln "exercise 2.30")
(define ex-2-30-tree (list 1
                           (list 2 (list 3 4) 5)
                           (list 6 7)))

(define (square-tree tree)
  (cond [(null? tree) '()]
        [(not (pair? tree)) (* tree tree)]
        [else (cons (square-tree (car tree))
                    (square-tree (cdr tree)))]))
(square-tree ex-2-30-tree)

(displayln "exercise 2.31")
(define (tree-map proc tree)
  (cond [(null? tree) '()]
        [(not (pair? tree)) (proc tree)]
        [else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree)))]))

(tree-map (lambda (x) (* x x)) ex-2-30-tree)

(displayln "exercise 2.32")
(define (subsets s)
  (if (null? s)
      (list '())
      (let [(rest (subsets (cdr s)))]
        (append rest
                (map (lambda (x) (append (list (car s)) x))
                     rest)))))

(subsets '(1 2 3))
;; The rest is the list we have built out so far. The first item will always be the empty
;; list. So that's our identity and we return a new list from that. The rest of the list items
;; are simply those we need to update with our details as well.
;; If we have a null 'x' we know that we need to take the head of the list and create an entry for that, otherwise
;; we create a new list by cons-ing the head of the list to whatever x is.
;; 
;; We basically unwind our list and then build it back up, that's why the return values aren't pretty they way we'd
;; probably like to see them.

;; Section 2.2.3 Functions
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(displayln "exercise 2.33")
(define (accum-map p seq)
  (accumulate (lambda (x y) (cons (p x) y)) '() seq))
(accum-map square (list 1 2 3 4 5))

(define (accum-append seq1 seq2)
  (accumulate cons seq2 seq1))
(accum-append (list 1 2 3 4 5) (list 6 7 8))

(define (accum-len seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))
(accum-len (list 1 2 3 4 5 6 7))
(accum-len (list 23 5 3 2))

(displayln "exercise 2.34")
(define (horner-eval x coeff-seq)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coeff-seq))
(horner-eval 2 (list 1 3 0 5 0 1))

(displayln "exercise 2.35")
(define (count-leaves t)
  (accumulate + 0
              (map (lambda (x)
                     (cond [(null? x) 0]
                           [(pair? x) (count-leaves x)]
                           [else 1]))
                   t)))
(count-leaves (list (list (list 1 2) 3) 4))
                         
(displayln "exercise 2.36")
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init
                        (map car seqs))
            (accumulate-n op init
                          (map cdr seqs)))))

(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(displayln "exercise 2.37")
(displayln "dot product")
(define (dot-product v w)
  (accumulate + 0
              (accumulate-n * 1 (list v w))))
(dot-product '(1 2) '(3 4)) ;; Should be 11

(displayln "matrix * vector")
(define vec '(3 2 1))
(define mat '((1 2 3) (4 5 6) (7 8 9)))
(define iden '((1 0 0) (0 1 0) (0 0 1)))
(define mat2 '((2 4 6) (1 1 1) (7 2 0)))
(define (matrix-*-vector m v)
  (map (lambda (x)
         (accumulate + 0
                     (accumulate-n * 1 (list v x)))) m))

;; Not super sure about this answer. Should it be the other way?
;; The correct solution is '(10 28 46)
(matrix-*-vector mat vec)

(displayln "transpose")
(define (transpose mat)
  (accumulate-n cons '() mat))

(transpose mat)

(displayln "matrix * matrix")
(define (matrix-*-matrix m n)
  (let [(cols (transpose n))]
    (map (lambda (x)
           (matrix-*-vector cols x)) m)))

;; Should be '((1 2 3) (4 5 6) (7 8 9))
(matrix-*-matrix mat iden)
;; Should be '((25 12 8) (55 33 29) (85 54 50))
(matrix-*-matrix mat mat2)

(displayln "exercise 2.38")
(define fold-right accumulate)
(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))

(fold-right / 1 '(1 2 3)) ; 3 / 2 / 1
(fold-left / 1 '(1 2 3)) ; 1 / 2 / 3

(fold-right list '() '(1 2 3)) ;; (list 1 (list 2 (list 3 '())))
(fold-left list '() '(1 2 3)) ;; (list (list (list '() 1) 2) 3)

(fold-right + 0 '(1 2 3))
(fold-left + 0 '(1 2 3))

(fold-right * 1 '(1 2 3))
(fold-left * 1 '(1 2 3))

;; The operations need to be commutative in order to produce the same result.
;; As shown above the sum and product operands satisfy that requirement.

(displayln "exercise 2.39")
(define (reverse-r seq)
  (fold-right (lambda (x y) (append y (list x))) '() seq))

(define (reverse-l seq)
  (fold-left (lambda (x y) (cons y x)) '() seq))

(reverse-r '(1 2 3 4 5))
(reverse-l '(1 2 3 4 5))

;; From SICP 2.2.3
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (remove item seq)
  (filter (lambda (x) (not (= x item))) seq))

(displayln "exercise 2.40")
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(unique-pairs 5)

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (let ([first (car pair)]
        [second (cadr pair)])
    (list first second (+ first second))))

(define (prime-sum-pairs n)
  (map make-pair-sum
   (filter prime-sum?
           (unique-pairs n))))

(prime-sum-pairs 10)

(displayln "exercise 2.41")
(define (triples-that-sum n sum)
  (filter (lambda (x) (= sum (+ (car x) (cadr x) (caddr x))))
          (flatmap (lambda (x)
                     (map (lambda (pair) (append (list x) pair))
                          (unique-pairs (- x 1))))
                   (enumerate-interval 1 n))))

(triples-that-sum 10 15)

(displayln "exercise 2.42")
;; Let's talk about this one when we meetup
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (or #f (safe? k positions)))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (adjoin-position row col other-queens)
  (append (list row) other-queens))

(define (safe? k positions)
  (check-safe
   (car positions)
   1
   (cdr positions)))

(define (check-safe position distance cols)
  (cond [(empty? cols) #t]
        [(= (car cols) position) #f]
        [(= (- (car cols) distance) position) #f]
        [(= (+ (car cols) distance) position) #f]
        [else (check-safe position (+ distance 1) (cdr cols))]))

(for-each (lambda (x)
            (display "Queens for ")
            (display x)
            (displayln ":")
            (map displayln (queens x)))
          (enumerate-interval 1 8))



(displayln "exercise 2.43")
;; enumerate-interval results in generating a bunch of extra calls to queen-cols
;; for computation that is going to be re-performed multiple times but in the
;; end thrown out because we don't need this. If we wanted to calculate (queens 3)
;; using Louis implementation we'd call the first (queen-cols 2) 3 times, then the
;; next queen cols results in calling (queen-cols 1) 3 more times, then finally calling
;; (queen-cols 1) 3 more times. If T was the orignal time we've taken that and
;; increased it to the power of the board-size (T^board-size)