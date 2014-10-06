#lang racket

;; -------------------------------------------------------
;; Memory as Vectors
;; -------------------------------------------------------

;; Exercise 5.20, p.539
;; Assuming list procedure is defined as

(define (list . x)
  (if (empty? x)
      null
      (cons (car x)
            (apply list (cdr x)))))

(define x (cons 1 2))
(define y (list x x))

;; Registers:
;;
;;           0 1  2  3  4
;;          ┌─┬──┬──┬──┬──┐
;; the-cars │ │n1│p1│p1│  
;;          ├─┼──┼──┼──┼──┤
;; the-cdrs │ │n2│e0│p2│  
;;          └─┴──┴──┴──┴──┘
;;    free  p4
;;       x  p1
;;       y  p3
