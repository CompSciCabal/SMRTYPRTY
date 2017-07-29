#lang racket

; just raw
;(define (square-tree tree)
;  (cond [(null? tree) null]
;        [(not (pair? tree)) (* tree tree)]
;        [else (cons (square-tree (car tree))
;                    (square-tree (cdr tree)))]))

; with map
;(define (square-tree tree)
;  (map (lambda (tree)
;         (cond [(not (pair? tree)) (* tree tree)]
;               [else (square-tree tree)])) tree))

(define my-tree
  (list (list 5 4)
        (list (list 4 9)
              5)
        2))
              
(define (tree-map f tree)
  (cond [(null? tree) null]
        [(not (pair? tree)) (f tree)]
        [else (cons (tree-map f (car tree))
                    (tree-map f (cdr tree)))]))

; with custom tree-map!
(define (square-tree tree) (tree-map (lambda (x) (* x x)) tree))

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset)
                            (cons (car s) subset))
                          rest)))))
