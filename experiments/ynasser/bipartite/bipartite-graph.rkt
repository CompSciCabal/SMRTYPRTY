#lang racket
(require "trees.rkt")
(require "sets-lists-fn.rkt")
(require "test-graphs.rkt")

;; This contains the bipartite? function, which determines
;; if a simple, connected graph is bipartite. It could be re-used
;; to determine if a simple, disconnected graph is bipartite.

;; dog is a helper function for bipartite? and is kind of bloated atm
(define (dog tree stack red blue visited last-colour f)
  (cond
    ;; the empty graph is bipartite
    [(empty? tree) #t]
    ;; do red and blue have a non-empty intersection?
    [(not (empty? (intersection red blue))) #f]
    ;; is the stack empty?
    [(and (not f) (empty? stack)) #t]
    ;; first iteration, need to seed the colour pods!
    [f (dog tree (find-neighbours (first (first tree)) tree) (cons (first (first tree)) red) (find-neighbours (first (first tree)) tree) (cons (first (first tree)) visited) 'red #f)]
    ;; have we already visited this vertex? skip it!
    [(member? (first stack) visited) (dog tree (rest stack) red blue visited last-colour #f)]
    ;; otherwise, start eating the stack!
    ;; is the first vertex in red? put its neighbours in blue
    [(member? (first stack) red)
     (dog tree (union (find-neighbours (first stack) tree) (rest stack)) red (union (find-neighbours (first stack) tree) blue) (cons (first stack) visited) 'red #f)]
    ;; else it's in blue
    [else ;;(member? (first stack) blue)
     (dog tree (union (find-neighbours (first stack) tree) (rest stack)) (union (find-neighbours (first stack) tree) red) blue (cons (first stack) visited) 'blue #f)]))
    
(define (bipartite? vertices edges)
  (dog (tree-maker vertices edges) null null null null 'blue #t))

;; bipartite cases
(display "bipartite graphs:\n")
(bipartite? dead-v dead-e)
(bipartite? baby-v baby-e)
(bipartite? b-v1 b-e1)
(bipartite? square-v square-e)
(bipartite? b-v2 b-e2)
(bipartite? eight-cycle-v eight-cycle-e)

;; non-bipartite cases
(display "non-bipartite graphs:\n")
(bipartite? nb-v1 nb-e1)
(bipartite? nb-v2 nb-e2)
(bipartite? triangle-v triangle-e)
(bipartite? nb-v5 nb-e5)
(bipartite? c6-v c6-e)
