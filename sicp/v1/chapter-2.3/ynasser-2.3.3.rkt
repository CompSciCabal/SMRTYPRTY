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
  ;(display "s is ")(display s)(newline)
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

;; Efficiency: working with ordered sets is much faster.
;; unordered union-set: could be around O(n^2) in the worst possible case
;; because of element-of-set?
;; ordered union-set: O(n)

;; unordered intersect-set: O(n^2) in the worst case
;; ordered intersect-set: O(n) < ?? < O(n^2)

;; If you didn't need to keep track of how many times each element appeared in
;; the list (and even then, a dictionary/map would be much better), then the
;; set would be a better data type. Thus, you'd want to use the non-duplicate
;; one.

;; Exercise 2.61
;; I did this above by accident, I think?

;; Exercise 2.62
;; Give an O(n) implementation of union-set for sets as ordered lists.
;; I guess n = length(lst0) + length(lst1)
;; I think my solution above is O(n), so I won't do this.

;; Exercise 2.63
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set-tree? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set-tree? x (left-branch set)))
        ((> x (entry set))
         (element-of-set-tree? x (right-branch set)))))


(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
;;    (display "Visited ")(display (entry tree))(newline)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

(define tree-1 (make-tree 7 
                          (make-tree 3 
                                     (make-tree 1 null null) 
                                     (make-tree 5 null null))
                          (make-tree 9
                                     null 
                                     (make-tree 11 null null))))

(define tree-2 (make-tree 3 
                          (make-tree 1
                                     null
                                     null) 
                          (make-tree 7 
                                     (make-tree 5 
                                                null 
                                                null) 
                                     (make-tree 9 
                                                null 
                                                (make-tree 11 null null)))))

(define tree-3 (make-tree 5
                          (make-tree 3
                                     (make-tree 1 null null)
                                     null)
                          (make-tree 9 
                                     (make-tree 7 null null)
                                     (make-tree 11 null null))))

(define tree-4 (make-tree 1
                          null
                          (make-tree 2
                                     null
                                     (make-tree 3
                                                null
                                                (make-tree 4
                                                           null
                                                           null)))))

(define fake-binary-tree (make-tree 100
                                    (make-tree 12
                                               (make-tree 34
                                                          null
                                                          (make-tree -1000 null null))
                                               (make-tree 8 null null))
                                    null))
                                                          

;; a) Do the two procedures produce the same result for every tree? If not, how
;; do the results differ? What lists do the two procedures produce for the
;; trees in figure 2.16?

;;> (tree->list-1 tree-1)
;;'(1 3 5 7 9 11)
;;> (tree->list-2 tree-1)
;;'(1 3 5 7 9 11)
;;> (tree->list-1 tree-2)
;;'(1 3 5 7 9 11)
;;> (tree->list-2 tree-2)
;;'(1 3 5 7 9 11)
;;> (tree->list-1 tree-3)
;;'(1 3 5 7 9 11)
;;> (tree->list-2 tree-3)
;;'(1 3 5 7 9 11)
;;> (tree->list-1 tree-4)
;;'(1 2 3 4)
;;> (tree->list-2 tree-4)
;;'(1 2 3 4)
;;> (tree->list-1 fake-binary-tree)
;;'(34 -1000 12 8 100)
;;> (tree->list-2 fake-binary-tree)
;;'(34 -1000 12 8 100)

;; They seem to always produce ordered lists if the tree was actually a binary
;; tree. The first element is always the leftmost element and the last is the
;; rightmost.

;; b) Do the two procedures have the same order of growth in the number of 
;; steps required to convert a balanced tree with n elements to a list? If
;; not, which one grows more slowly?

;; Both visit each node exactly once, so each is at least O(n). I'm not
;; sure what the running time of append is, but tree->list-2 is O(n).

;; Exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

;; a) Write a short paragraph explaining as clearly as you can how partial-tree
;; works. Draw the tree produced by list->tree for the list (1 3 5 7 9 11).
;;
;;                            5
;;                          /   \ 
;;                         1     9
;;                          \   / \
;;                           3 7  11
;;
;; The tree is split up into 3 parts: left, center, and right. This is done
;; recursively, until there are no more nodes, and the final list is
;; assembled. 

;; b) What is the order of growth in the number of steps required by list->tree
;; to convert a list of n elements.
;;
;; I think it is O(n), based on the structure of the program and because
;; it doesn't use append or other costly list functions.
                

;; Exercise 2.65
;; Give O(n) implementations of union-set and intersection-set for sets
;; implemented as balanced binary trees.

(define (union-set-2.65 b1 b2)
  (list->tree (union-set (tree->list-2 b1)
			  (tree->list-2 b2))))

(define (intersection-set-2.65 b1 b2)
  (list->tree (intersect-set (tree->list-2 b1) 
			     (tree->list-2 b2))))

;; Tests for 2.65
;;
;;(union-set-2.65 (list->tree '(1 3 6))
;;		(list->tree '(2 4 7)))
;;
;;(intersection-set-2.65 (list->tree '(1 3 6))
;;                       (list->tree '(2 4 7)))
;;
;;(intersection-set-2.65 (list->tree '(1 3 67))
;;                       (list->tree '(2 4 67)))
;;
;;(intersection-set-2.65 (list->tree '(1 3 67 90 200))
;;                       (list->tree '(2 4 67 99 200)))