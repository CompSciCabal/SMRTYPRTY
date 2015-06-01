;#lang planet neil/sicp
#lang racket

;; note: only dealing with simple graphs

;; member?
(define (member? key lst)
  (cond
    [(not (member key lst)) #f]
    [else #t]))

;; empty? ehh sicp lisp is kind of irritating
(define (empty? lst)
  (cond
    [(eq? 0 (length lst)) #t]
    [else #f]))

(define (union a b)
  (union-helper a b null))

(define (union-helper a b lst)
  (cond
    [(and (empty? a) (empty? b)) lst]
    [(and (empty? a) (not (member? (first b) lst)))
     (union-helper a (rest b) (cons (first b) lst))]
    [(and (empty? b) (not (member? (first a) lst))) 
     (union-helper (rest a) b (cons (first a) lst))]
    [(empty? a) (union-helper a (rest b) lst)]
    [(empty? b) (union-helper (rest a) b lst)]
    [else (union-helper (rest a) b (cons (first a) lst))]))

;; same as union, but preserves order
(define (combine a b)
  (union null (union a b)))

(define (intersection a b) (intersection-helper a b null))

(define (intersection-helper a b lst)
  (cond
    [(empty? a) lst]
    [(member? (first a) b) (intersection-helper (rest a) b (cons (first a) lst))]
    [else (intersection-helper (rest a) b lst)]))

(define (other-vertex vertex edge)
  (cond
    [(equal? vertex (first edge)) (second edge)]
    [else (first edge)]))

;; already-in-tree is wrong (what???)
(define (already-in-tree? vertex tree)
  (cond
    [(empty? tree) #f]
    [(equal? vertex (first (first tree))) #t]
    [else (already-in-tree? vertex (rest tree))]))

;; it's ordered!! remember! i hope this preserved order
(define (add-to-tree neighbor tree)
  (define first-tree (first tree))
  (define rest-tree (rest tree))
  (define first-first-tree (first first-tree))
  (define rest-first-tree (second first-tree))
  ;; the reconstructed tree:
  (cons (list first-first-tree (cons neighbor rest-first-tree)) rest-tree))

(define (tree-maker vertices edges)
  (make-tree vertices edges edges null))

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
    
    ;; it's a member, but it's not already in the tree! (same as two cases before
     ;; refactor ... 
    [(member? (first vertices) (first edges))
     (make-tree vertices (rest edges) static-edges (cons (list (first vertices) (list (other-vertex (first vertices) (first edges)))) tree))]))


;(equal? (make-tree '(a b) (list '(a b)) (list '(a b)) null) (list (list 'a '(b)) (list 'b '(a))))
;; so, add-to-tree works!
;;(equal? (add-to-tree 'b (list (list 'a '(c g)))) (list (list 'a '(b c g))))
;;(equal? (add-to-tree 'b (list (list 'a '(c g)) (list 'z '(c o)))) (list (list 'a '(b c g)) (list 'z '(c o))))
;;(equal? (add-to-tree 'b (list (list 'a '(c g)) (list 'z '(c o)) (list 'p '(w q))))
;;        (list (list 'a '(b c g)) (list 'z '(c o)) (list 'p '(w q))))
;;(equal? (make-tree '(a b c) null null null) '((c ()) (b ()) (a ())))
;;(equal? (make-tree '(a b c) (list '(a b) '(b c) '(c a)) (list '(a b) '(b c) '(c a)) null) '((c (a b)) (b (c a)) (a (c b))))

;; complicated test graphs
;; bipartite ones
;(define b-v0 null)
;(define b-e0 null)
;(define b0 (tree-maker b-v0 b-e0))

(define b-v1 '(a b c d e f)) 
(define b-e1 (list '(a b) '(b c) '(c d) '(e f) '(f a) '(d a)))
;(define b1 (tree-maker b-v1 b-e1))
;(define b1 '((f (a e)) (e (f)) (d (a c)) (c (d b)) (b (c a)) (a (d f b))))
(define b1 '((f (a e)) (e (f)) (d (a c)) (c (d b)) (b (c a)) (a (d f b))))


(define b-v3 '(a b c d e f g h i j x y z))
(define b-e3 (list '(a b) '(a d) '(a e) '(a g) 
                   '(b c) '(e f)
                   '(c d) '(c h) '(c j)
                   '(d x) '(d z) 
                   '(f g) 
                   '(h i) '(i j)
                   '(x y) '(z y)))
(define b3 (tree-maker b-v3 b-e3))

;; non-bipartite graphs meow
(define nb-v1 '(a b c e f g h i))
(define nb-e1 (list '(a b) '(a c) '(a f) '(b c) '(b e)
                    '(c e) '(c g) '(c f)
                    '(e g) '(e h)
                    '(f g) '(f i)
                    '(g i) '(g h)
                    '(h i)))
(define nb1 (tree-maker nb-v1 nb-e1))

(define nb-v2 '(a b c d e f))
(define nb-e2 (list '(a b) '(a c) '(a d) '(a e) '(a f)
                    '(b c) '(b d) '(b e) '(b f)
                    '(c d) '(c e) '(c f)
                    '(d e) '(d f)
                    '(e f)))
(define nb2 (tree-maker nb-v2 nb-e2))

(define nb-v5 '(a b c d e f g h))
(define nb-e5 (list '(a b) '(a f) '(a h) '(b e) '(b c) 
                   '(c h) '(c e) '(e d) '(e f)
                   '(f g) '(g d) '(g h)))
(define nb5 (tree-maker nb-v5 nb-e5))

(define (find-neighbours vertex tree)
  (cond
    [(empty? tree) null]
    [(equal? vertex (first (first tree))) (second (first tree))]
    [else (find-neighbours vertex (rest tree))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (dog tree stack red blue visited last-colour f)
  (cond
    ;; is the stack empty?
    [(and (not f) (empty? stack)) #t]
    ;; first iteration, need to seed the colour pods!
    [f (dog tree (find-neighbours (first (first tree)) tree) (cons (first (first tree)) red) (find-neighbours (first (first tree)) tree) (cons (first (first tree)) visited) 'red #f)]
    ;; do red and blue have a non-empty intersection?
    [(not (empty? (intersection red blue))) #f]
    ;; is the stack empty?
    [(empty? stack) #t]
    ;; have we already visited this vertex? skip it!
    [(member? (first stack) visited) (dog tree (rest stack) red blue visited last-colour #f)]
    ;; otherwise, start eating the stack!
    ;; is the first vertex in red? put its neighbours in blue
    [(member? (first stack) red)
     (dog tree (rest stack) red (union (find-neighbours (first stack) tree) blue) (cons (first stack) visited) 'red #f)]
    ;; it's in blue
    [else ;;(member? (first stack) blue)
     (dog tree (rest stack) (union (find-neighbours (first stack) tree) red) blue (cons (first stack) visited) 'blue #f)]))
    
(define (dogs vertices edges)
  (dog (tree-maker vertices edges) null null null null 'blue #t))

(define baby-v (list 'a 'b))
(define baby-e (list (list 'a 'b)))
(define baby-tree (tree-maker baby-v baby-e))

(define square-v '(a b c d))
(define square-e (list '(a b) '(b c) '(c d) '(d a)))
(define square-tree (tree-maker square-v square-e))

(define eight-cycle-v '(a b c d e f g h))
(define eight-cycle-e (list '(a b) '(b c) '(c d) '(d e) '(e f) '(f g) '(g h) '(h a)))
(define eight-tree (tree-maker eight-cycle-v eight-cycle-e))

(define dead-v null)
(define dead-e null)
(define dead (tree-maker dead-v dead-e))

;; a nice quote:

;; It should be clear that the possibility of associating values with
;; symbols and later retrieving them means that the interpreter must
;; maintain some sort of memory that keeps track of the name-object pairs.
;; This memory is called the environment (more precisely the global
;; environment, since we will see later that a computation may involve
;; a number of different environments).

