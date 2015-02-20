#lang racket

;;; PRELUDE

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 1 1)))

(define wave einstein)
(define wave2 (beside wave (flip-vert wave))) 
(define wave4 (below wave2 wave2))

(define (flipped-pairs painter) 
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n) 
  (if (= n 0)
      painter 
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n) 
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1))) 
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up)) 
              (bottom-right (below right right)) 
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left) 
                  (below bottom-right corner))))))

(define (square-limit painter n) 
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter))) 
      (below (flip-vert half) half))))

;(define (frame-coord-map frame)
;  (lambda (v)
;    (add-vect (origin-frame frame)
;              (add-vect (scale-vect (xcor-vect v) (edge1-frame frame)) 
;                        (scale-vect (ycor-vect v) (edge2-frame frame))))))

;;; 2.44

(define (up-split painter n) 
  (if (= n 0)
      painter 
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;;; 2.45

(define (multisplit . ops)
  (define (inner painter)
    (display ops)
    (if (or (null? ops)
            (null? (car ops)))
        (lambda id (car id))
        (let ((smaller ((apply multisplit (cdr ops)) painter) ))
          ((car ops) smaller painter))))
  inner)
; this needs a 'spread' operator to work correctly... ask leo.

(define (split a b)
  (define foo 
    (lambda (painter n)
      (if (= n 0)
          painter 
          (let ((smaller (foo painter (- n 1))))
            (a (b smaller smaller) painter)))))
  foo)

;;; 2.46

(define (make-vect1 x y) (cons x y))
(define (xcor-vect v)    (car v))
(define (ycor-vect v)    (cdr v))
(define (add-vect v w)
  (make-vect1 (+ (xcor-vect v) (xcor-vect w))
              (+ (ycor-vect v) (ycor-vect w))))
(define (sub-vect v w)
  (make-vect1 (- (xcor-vect v) (xcor-vect w))
              (- (ycor-vect v) (ycor-vect w))))
(define (scale-vect v s)
  (make-vect1 (* s (xcor-vect v))
              (* s (ycor-vect v))))

;;; 2.47

(define (make-frame1 origin edge1 edge2) 
  (list origin edge1 edge2))
(define (origin-frame1 f)   (car f))
(define (edge1-frame1  f)   (cadr f))
(define (edge2-frame1  f)   (caddr f))
  
(define (make-frame2 origin edge1 edge2) 
  (cons origin (cons edge1 edge2)))
(define (origin-frame2 f)   (car f))
(define (edge1-frame2  f)   (cadr f))
(define (edge2-frame2  f)   (cddr f))

;;; 2.48

(define (make-segment1 v1 v2) (cons v1 v2))
(define (start-segment1 s) (car s))
(define (end-segment1   s) (cdr s))

;;; 2.49

(define l 0.01)  ; the racket ide blanks out points on the edges
(define h 0.99)
(define m 0.5)
(paint (segments->painter (list (cons (cons l l) (cons h h))
                                (cons (cons l h) (cons h l)))))
(paint (segments->painter (list (cons (cons l l) (cons h l))
                                (cons (cons h l) (cons h h))
                                (cons (cons h h) (cons l h))
                                (cons (cons l h) (cons l l)))))
(paint (segments->painter (list (cons (cons l m) (cons m h))
                                (cons (cons h m) (cons m l))
                                (cons (cons m h) (cons h m))
                                (cons (cons m l) (cons l m)))))
(paint wave)   ; ha ha not really

;;; 2.50

(define (flip-vert1 painter) 
  ((transform-painter (make-vect 0.0 1.0)
                      (make-vect 1.0 1.0)
                      (make-vect 0.0 0.0))
   painter))

(define (flip-horiz1 painter) 
  ((transform-painter (make-vect 1.0 0.0)
                      (make-vect 0.0 0.0)
                      (make-vect 1.0 1.0))
   painter))

(define (rot180 painter) 
  ((transform-painter (make-vect 1.0 1.0)
                      (make-vect 0.0 1.0)
                      (make-vect 1.0 0.0))
   painter))

(define (rot270 painter) 
  ((transform-painter (make-vect 1.0 1.0)
                      (make-vect 1.0 0.0)
                      (make-vect 0.0 1.0))
   painter))

;;; 2.51

(define (below1 painter1 painter2) 
  (let ((paint-left ((transform-painter
                       (make-vect 0.0 0.0) 
                       (make-vect 1.0 0.0) 
                       (make-vect 0.0 0.5))
                     painter1))
        (paint-right ((transform-painter
                       (make-vect 0.0 0.5)
                       (make-vect 1.0 0.5)
                       (make-vect 0.0 1.0))
                      painter2)))
    (lambda (frame) 
      (paint-left frame) 
      (paint-right frame))))

(define (below2 painter1 painter2)
  (rot270 (beside (rot270 painter2) (rot270 painter1))))