#lang racket
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
;; From the Book (2.2.4)
(define (flipped-pairs painter)
  (let [(painter2 (beside painter (flip-vert painter)))]
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let [(smaller (right-split painter (- n 1)))]
        (beside painter (below smaller smaller)))))

;; Exercise 2.44 -- up-split
(define (up-split painter n)
  (if (= n 0)
      painter
      (let [(smaller (up-split painter (- n 1)))]
        (below painter (beside smaller smaller)))))


(define (corner-split painter n)
  (if (= n 0)
      painter
      ;; The book does embedded lets
      ;; and I think that's silly. Using
      ;; let* because it makes things easier
      ;; to read and such.
      (let* [(up (up-split painter (- n 1)))
             (right (right-split painter (- n 1)))
             (top-left (beside up up))
             (bottom-right (below right right))
             (corner (corner-split painter (- n 1)))]
        (beside (below painter top-left)
                (below bottom-right corner)))))

(define (square-limit painter n)
  (let* [(quarter (corner-split painter n))
         (half (beside (flip-horiz quarter) quarter))]
    (below (flip-vert half) half)))

;; Exercise 2.45 - split
(define (split first second)
  (lambda (painter)
    (first painter (second painter painter))))

(define ez-right-split (split beside below))
(define ez-up-split (split below beside))

;; Exercise 2.46
(define make-vector cons)
(define xcor-vect car)
(define ycor-vect cadr)

(define (add-vect v1 v2)
  (make-vector
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vector
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vector (* s (xcor-vect v))
               (* s (ycor-vect v))))

;; Exercise 2.47
(define (l-make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define l-origin-frame car)
(define l-edge1-frame cadr)
(define l-edge2-frame caddr)

(define (c-make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define c-origin-frame car)
(define c-edge1-frame cadr)
(define c-edge2-frame cddr)