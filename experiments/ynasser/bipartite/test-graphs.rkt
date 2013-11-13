#lang racket
(require "trees.rkt")
(provide (all-defined-out))

;; This module contains both bipartite and non-bipartite test graphs.

;; bipartite graphs in no particular order
(define dead-v null)
(define dead-e null)
;(define dead (tree-maker dead-v dead-e))

(define baby-v (list 'a 'b))
(define baby-e (list (list 'a 'b)))
;(define baby-tree (tree-maker baby-v baby-e))

(define b-v1 '(a b c d e f)) 
(define b-e1 (list '(a b) '(b c) '(c d) '(e f) '(f a) '(d a)))
;(define b1 (tree-maker b-v1 b-v1))

(define square-v '(a b c d))
(define square-e (list '(a b) '(b c) '(c d) '(d a)))

(define b-v2 '(a b c d e f g h i j x y z))
(define b-e2 (list '(a b) '(a d) '(a e) '(a g) 
                   '(b c) '(e f)
                   '(c d) '(c h) '(c j)
                   '(d x) '(d z) 
                   '(f g) 
                   '(h i) '(i j)
                   '(x y) '(z y)))
;(define b2 (tree-maker b-v2 b-e2))

(define eight-cycle-v '(a b c d e f g h))
(define eight-cycle-e (list '(a b) '(b c) '(c d) '(d e) '(e f) '(f g) '(g h) '(h a)))
;(define eight-tree (tree-maker eight-cycle-v eight-cycle-e))

;; non-bipartite graphs meow
(define nb-v1 '(a b c e f g h i))
(define nb-e1 (list '(a b) '(a c) '(a f) '(b c) '(b e)
                    '(c e) '(c g) '(c f)
                    '(e g) '(e h)
                    '(f g) '(f i)
                    '(g i) '(g h)
                    '(h i)))
;(define nb1 (tree-maker nb-v1 nb-e1))

(define nb-v2 '(a b c d e f))
(define nb-e2 (list '(a b) '(a c) '(a d) '(a e) '(a f)
                    '(b c) '(b d) '(b e) '(b f)
                    '(c d) '(c e) '(c f)
                    '(d e) '(d f)
                    '(e f)))
;(define nb2 (tree-maker nb-v2 nb-e2))

(define triangle-v '(a b c))
(define triangle-e (list '(a b) '(b c) '(c a)))

(define nb-v5 '(a b c d e f g h))
(define nb-e5 (list '(a b) '(a f) '(a h) '(b e) '(b c) 
                    '(c h) '(c e) '(e d) '(e f)
                    '(f g) '(g d) '(g h)))
;(define nb5 (tree-maker nb-v5 nb-e5))

;; the complete graph on 6 vertices
(define c6-v '(a b c d e f))
(define c6-e '((a b) (a c) (a d) (a e) (a f)
                     (b c) (b d) (b e) (b f)
                           (c d) (c e) (c f)
                                 (d e) (d f)
                                       (e f)))