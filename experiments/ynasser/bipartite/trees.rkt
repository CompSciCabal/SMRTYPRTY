#lang racket
(provide (all-defined-out))
(require "sets-lists-fn.rkt")

;; This module provides the functions which will convert a list of vertices
;; and a list of edges into the data type which the graph algorithm
;; operates on. The code is not very pretty :(
;; I did not adhere to the 79 character line limit.

(define (other-vertex vertex edge)
  (cond
    [(equal? vertex (first edge)) (second edge)]
    [else (first edge)]))

(define (already-in-tree? vertex tree)
  (cond
    [(empty? tree) #f]
    [(equal? vertex (first (first tree))) #t]
    [else (already-in-tree? vertex (rest tree))]))

;; this preserverse order (I hope)
(define (add-to-tree neighbor tree)
  (define first-tree (first tree))
  (define rest-tree (rest tree))
  (define first-first-tree (first first-tree))
  (define rest-first-tree (second first-tree))
  ;; the reconstructed tree:
  (cons (list first-first-tree (cons neighbor rest-first-tree)) rest-tree))

(define (find-neighbours vertex tree)
  (cond
    [(empty? tree) null]
    [(equal? vertex (first (first tree))) (second (first tree))]
    [else (find-neighbours vertex (rest tree))]))

(define (tree-maker vertices edges)
  (make-tree vertices edges edges null))

;; This needs to be re-factored ... 
(define (make-tree vertices edges static-edges tree)
  (cond
    [(empty? vertices) tree]
    
    ;; non-empty vertices, but out of edges, but tree is empty
    [(and (empty? tree) (empty? edges))
     (make-tree (rest vertices) static-edges static-edges (list (list (first vertices) null)))]
    
    ;; non-empty vertices, out of edges, but you are already in the tree!
    [(and (and (not (empty? tree)) (empty? edges)) (already-in-tree? (first vertices) tree))
     (make-tree (rest vertices) static-edges static-edges tree)]
    
    ;; non-empty vertices, out of edges, but the tree is not empty
    [(and (and (not (empty? tree)) (empty? edges)) (not (already-in-tree? (first vertices) tree)))
     (make-tree (rest vertices) static-edges static-edges (cons (list (first vertices) null) tree))]
    
    ;; non-empty vertices, has edges, but not a member
    [(and (and (not (empty? vertices)) (not (empty? edges))) (not (member? (first vertices) (first edges))))
     (make-tree vertices (rest edges) static-edges tree)]
    
    ;; now, we go through vertices (and edges is non-empty) (and we are operating on (first vertices))
    ;; it's a member! so we add it to its edge set ... but the tree is currently empty
    [(and (empty? tree) (member? (first vertices) (first edges)))
     (make-tree vertices (rest edges) static-edges (cons (list (first vertices) (list (other-vertex (first vertices) (first edges)))) tree))]
    
    ;; it's a member! but the tree is *not* empty ... but is it already in the *ordered* tree?
    [(and (member? (first vertices) (first edges)) (already-in-tree? (first vertices) tree))
     (make-tree vertices (rest edges) static-edges (add-to-tree (other-vertex (first vertices) (first edges)) tree))]    
    
    ;; it's a member, but it's not already in the tree!
    [(member? (first vertices) (first edges))
     (make-tree vertices (rest edges) static-edges (cons (list (first vertices) (list (other-vertex (first vertices) (first edges)))) tree))]))



;; Tests
;;(equal? (make-tree '(a b) (list '(a b)) (list '(a b)) null) (list (list 'a '(b)) (list 'b '(a))))
;;(equal? (add-to-tree 'b (list (list 'a '(c g)))) (list (list 'a '(b c g))))
;;(equal? (add-to-tree 'b (list (list 'a '(c g)) (list 'z '(c o)))) (list (list 'a '(b c g)) (list 'z '(c o))))
;;(equal? (add-to-tree 'b (list (list 'a '(c g)) (list 'z '(c o)) (list 'p '(w q))))
;;        (list (list 'a '(b c g)) (list 'z '(c o)) (list 'p '(w q))))
;;(equal? (make-tree '(a b c) null null null) '((c ()) (b ()) (a ())))
;;(equal? (make-tree '(a b c) (list '(a b) '(b c) '(c a)) (list '(a b) '(b c) '(c a)) null) '((c (a b)) (b (c a)) (a (c b))))
