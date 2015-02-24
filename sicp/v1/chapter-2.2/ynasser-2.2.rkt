;#lang planet neil/sicp
#lang racket

;; 2.17
(define (last-pair lst)
  (if (null? (cdr lst))
      (list (car lst))
      (last-pair (cdr lst))))

;; 2.18
(define (reverse lst)
  (define (reverse-helper lst reversed-lst)
    (if (null? lst)
        reversed-lst
        (reverse-helper (cdr lst) (cons (car lst) reversed-lst))))
    (reverse-helper lst '()))

;; 2.19
(define (no-more? lst)
  (if (null? lst)
      #t
      #f))

(define first-denomination car)
(define except-first-denomination cdr)

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;; Does the order of the list coin-values affect the answer produced by cc?
;; (define us-coins (list 50 25 10 5 1))
;; (define r-us (reverse us-coins))
;; (define pr-us (list 25 10 1 50 5))
;; (cc 100 us-coins) => 292
;; (cc 100 r-us) => 292
;; (cc 100 pr-us) => 292
;; The order doesn't matter. 
;; TODO: explain why

;; 2.20
(define (same-parity . lst)
  (filter (lambda (x) (even? (+ x (car (car lst))))) (car lst)))

;; 2.21
(define (square-list-one items)
  (if (null? items)
      '()
      (cons (* (car items) (car items)) (square-list-one (cdr items)))))

(define (square-list-two items)
  (map (lambda (x) (* x x)) items))

;; 2.22
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (sqr (car things)) answer))))
(iter items '()))

;; Louis' procedure returns the list backwards because it's taking the first
;; thing and putting it on a stack in this line:
;; (cons (square (car things)) answer))
;; and then it iterates through the rest of list, pushing each new item on to
;; the top of the stack. His procedure has the same structure as exercise 2.18.

(define (square-list-new items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (sqr (car things))))))
  (iter items '()))

;; This doesn't work because the second item passed to cons must be a list
;; if the end product to be a flat list.

;; 2.23
(define (for-each proc lst)
  (cond
    [(not (empty? lst))(proc (first lst))
          (for-each proc (rest lst))]))


;; Section 2.2.2 Heirarchical Structures

;; 2.24
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
;; (count-leaves (list 1 (list 2 (list 3 4)))) => 4

;; 2.25
;(define lst1 '(1 3 (5 7) 9))
;(car (cdr (car (cdr (cdr lst1)))))

;(define lst2 '((7)))
;(car (car lst2))

;(define lst3 '(1 (2 (3 (4 (5 (6 7)))))))
;(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr lst3))))))))))))

;; 2.26
;(define x (list 1 2 3))
;(define y (list 4 5 6))
;(append x y) => '(1 2 3 4 5 6)
;(cons x y) => '((1 2 3) 4 5 6)
;(list x y) => '((1 2 3) (4 5 6))

;; Note: I'm going to assume trees split up in pairs, as the book
;; seems to hint at this. I think it's also a much harder problem
;; otherwise.

;; 2.27

;; this works, but is not very pretty :(
(define (flat? lst)
  (andmap (lambda (x) (not (list? x))) lst))

(define (deep-reverse lst)
  (cond
    [(empty? lst) lst]
    [(flat? lst) (reverse lst)]
    [else (reverse (map (lambda (x) (cond 
                                      [(list? x) (deep-reverse x)]
                                      [else x])) lst))]))
     
;; 2.28
;; This just flattens the list.
;; Todo: can be made more efficient!
(define (fringe lst)
  (cond
    [(empty? lst) empty]
    [(integer? lst) (list lst)]
    [(list? (car lst)) (fringe (append (car lst) (cdr lst)))]
    [else (cons (first lst) (fringe (cdr lst)))]))

;; 2.29
;; I feel like I've had to solve this *exact* set of problems 3 times a year
;; since starting university.
(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

;; part a
(define left-branch car)
(define (right-branch m) (car (cdr m)))
(define branch-length car)
(define (branch-structure m) (car (cdr m)))

;; part b
(define (branch-weight branch)
  (cond
    [(empty? branch) 0]
    [(integer? (branch-structure branch)) (branch-structure branch)]
    [else (total-weight (branch-structure branch))]))

(define (total-weight mobile)
  (cond
    [(empty? mobile) 0]
    [else (+ (branch-weight (left-branch mobile))
             (branch-weight (right-branch mobile)))]))

;; part c 
(define (total-length mobile)
  (cond
    [(empty? mobile) 0]
    ;; max depth reach, just sum the current lengths
  ;  [(not (pair? mobile)) mobile]
    [(andmap integer? (list (branch-structure (left-branch mobile))
                            (branch-structure (right-branch mobile))))
     (+ (branch-length (left-branch mobile))
        (branch-length (right-branch mobile)))]    
    ;; recurse only on right branch
    [(integer? (branch-structure (left-branch mobile)))    
     (+ (branch-length (left-branch mobile))
        (branch-length (right-branch mobile))
        (total-length (branch-structure (right-branch mobile))))]    
    ;; recurse only on left branch
    [(integer? (branch-structure (right-branch mobile)))
     (+ (branch-length (left-branch mobile))
        (branch-length (right-branch mobile))
        (total-length (branch-structure (left-branch mobile))))]     
    ;; otherwise, recurse on both branches 
    [else (+ (branch-length (left-branch mobile))
             (branch-length (right-branch mobile))
             (total-length (branch-structure (left-branch mobile)))
             (total-length (branch-structure (right-branch mobile))))]))

;; another ugly solution :(
(define (balanced? mobile)
  (cond
    ;; a very small mobile
    [(andmap (lambda (x) (not (pair? x))) 
             (list (branch-structure (left-branch mobile))
                   (branch-structure (right-branch mobile))))
     (eq? (* (car (car mobile)) (car (cdr (car mobile))))
          (* (car (car (cdr mobile))) (car (cdr (car (cdr mobile))))))]
    ;; only the left branch is out
    [(not (pair? (branch-structure (left-branch mobile))))
     (eq? (* (car (car mobile)) (car (cdr (car mobile))))
          (* (total-length (branch-structure (right-branch mobile)))
          (total-weight (branch-structure (right-branch mobile)))))]
    
    ;; only the right branch is out
    [(not (pair? (branch-structure (right-branch mobile)))) 
     (eq?
      (* (total-length (branch-structure (left-branch mobile))) 
          (total-weight (branch-structure (left-branch mobile))))
      (* (car (car (cdr mobile))) (car (cdr (car (cdr mobile))))))]
    ;; a non-trivial mobile
    [else
    (eq? (* (total-length (branch-structure (left-branch mobile))) 
          (total-weight (branch-structure (left-branch mobile))))
       (* (total-length (branch-structure (right-branch mobile)))
          (total-weight (branch-structure (right-branch mobile)))))]))

;; part d
;; TODO
;; I don't even want to think about it.

;; "Mapping over trees"
(define (scale-tree-old tree factor)
  (cond [(null? tree) empty]
        [(not (pair? tree)) (* tree factor)]
        [else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor))]))

(define (scale-tree-new tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree-new sub-tree factor)
             (* sub-tree factor)))
       tree))

;; Exercise 2.30
(define (square-tree-1 tree)
  (cond
    [(null? tree) empty]
    [(not (pair? tree)) (sqr tree)]
    [else (cons (square-tree-1 (car tree))
                (square-tree-1 (cdr tree)))]))

(define (square-tree-2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-2 sub-tree)
             (sqr sub-tree)))
       tree))

;; Exercise 2.31
(define (tree-map func tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map func sub-tree)
             (func sub-tree)))
       tree))

(define (square-tree tree) (tree-map sqr tree))
(define (scale-tree factor tree) (tree-map (lambda (x) (* x factor)) tree))

;; Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list '())
      ;; I do not want to redefine rest, because I'm using racket.
      (let ((rest-new (subsets (cdr s))))
       ; (display "s is ")(display s)(newline)
       ; (display "rest-new is ")(display rest-new)(newline)
        (append rest-new (map (lambda (x) (cons (car s) x)) rest-new)))))

;; explanation: at each "iteration", the value of s the value of the original
;; list lst with cdr applied to it (- (length lst) n) times, where n is the
;; number which represents the "iteration" you are on.
;; Thus, the value of s in (subsets '(1 2 3 4)) would be:
;; '(4)
;; '(3 4)
;; '(2 3 4)
;; '(1 2 3 4)
;;
;; It's a direct implementation of the definition from Wikipedia. To generate
;; the power set of s, find the union of the following:
;; - the power set of s-e (e is some element)
;; - the power set of all subsets of s containing e
;;
;; This combinatorics-y logic is similar to Isabel's explanation of the "rotten
;; egg lemma": 
;; http://isabelandbenson.wordpress.com/2013/07/11/a-picture-and-the-rotten-egg-lemma/

;; Stolen tests!
(define sicp-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(define mobile-0 (make-mobile (make-branch 0 0)
                              (make-branch 0 0)))

(define mobile-1 (make-mobile (make-branch 2 1) 
                              (make-branch 1 2))) 

(define mobile-2 (make-mobile (make-branch 3 mobile-1) 
                              (make-branch 9 5)))

(define mobile-3 (make-mobile (make-branch 4 mobile-2) 
                              (make-branch 1000 mobile-2)))

(define mobile-4 (make-mobile (make-branch 4 56)
                              (make-branch 3 mobile-2)))

;; (orange juice + coffee) = (orange juice + coffee)
;; the flavours didn't merge at all
;; it tasted very distinctly of orange juice and coffee
