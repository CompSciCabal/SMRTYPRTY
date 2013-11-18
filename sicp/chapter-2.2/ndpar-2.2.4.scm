#lang racket

;; Drawing procedures
(require (lib "racket/draw"))
(require racket/class)

(define target (make-bitmap 500 500))
(define dc (new bitmap-dc% [bitmap target]))

(define (my-draw-line v-start v-end)
  (send dc draw-line
        (xcor-vect v-start) (ycor-vect v-start)
        (xcor-vect v-end) (ycor-vect v-end)))

;; Helper procedures
(define (dec n) (- n 1))

;; Picture Language
(define ((square-of-four tl tr bl br) painter)
  (let ((top (beside (tl painter) (tr painter)))
        (bottom (beside (bl painter) (br painter))))
    (below bottom top)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let* ((up (up-split painter (dec n)))
             (right (right-split painter (dec n)))
             (top-left (beside up up))
             (bottom-right (below right right))
             (corner (corner-split painter (dec n))))
        (beside (below painter top-left)
                (below bottom-right corner)))))

;; Exercise 2.44, p.132
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (dec n))))
        (below painter (beside smaller smaller)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (dec n))))
        (beside painter (below smaller smaller)))))

;; Exercise 2.45, p.134
(define ((split horiz vert) painter n)
  (if (= n 0)
      painter
      (let ((smaller ((split horiz vert) painter (dec n))))
        (horiz painter (vert smaller smaller)))))

;(define up-split (split below beside))
;(define right-split (split beside below))

;; p.135: Frames
(define ((frame-coord-map frame) v)
  (add-vect (origin-frame frame)
            (add-vect (scale-vect (xcor-vect v)
                                  (edge1-frame frame))
                      (scale-vect (ycor-vect v)
                                  (edge2-frame frame)))))

;; Exercise 2.46, p.136
;; Vector abstractions
(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

;; Vector algebra
(define ((op-vect op) u v)
  (make-vect (op (xcor-vect u) (xcor-vect v))
             (op (ycor-vect u) (ycor-vect v))))

(define add-vect (op-vect +))
(define sub-vect (op-vect -))
(define (scale-vect s v)
  ((op-vect *) (make-vect s s) v))

;; Exercise 2.47, p.136
;; Frame abstractions
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define edge2-frame cddr)

;(define (make-frame origin edge1 edge2)
;  (list origin edge1 edge2))
;(define edge2-frame caddr)

(define origin-frame car)
(define edge1-frame cadr)

;; Exercise 2.48, p.137
;; Segment asbractions
(define (make-segment start end) (cons start end))
(define start-segment car)
(define end-segment cdr)

(define ((segments->painter segment-list) frame)
  (for-each
   (lambda (segment)
     (my-draw-line
      ((frame-coord-map frame) (start-segment segment))
      ((frame-coord-map frame) (end-segment segment))))
   segment-list))

;; Exercise 2.49, p.137
;; Primitive painters
(define outline-painter
  (segments->painter
   (list (make-segment (make-vect 0.0 0.0) (make-vect 0.0 1.0))
         (make-segment (make-vect 0.0 1.0) (make-vect 1.0 1.0))
         (make-segment (make-vect 1.0 1.0) (make-vect 1.0 0.0))
         (make-segment (make-vect 1.0 0.0) (make-vect 0.0 0.0)))))

(define x-painter
  (segments->painter
   (list (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
         (make-segment (make-vect 0.0 1.0) (make-vect 1.0 0.0)))))

(define diamond-painter
  (segments->painter
   (list (make-segment (make-vect 0.0 0.5) (make-vect 0.5 1.0))
         (make-segment (make-vect 0.5 1.0) (make-vect 1.0 0.5))
         (make-segment (make-vect 1.0 0.5) (make-vect 0.5 0.0))
         (make-segment (make-vect 0.5 0.0) (make-vect 0.0 0.5)))))

(define wave
  (segments->painter
   (list (make-segment (make-vect 0.00 0.80) (make-vect 0.20 0.55)) ; 1 line
         (make-segment (make-vect 0.20 0.55) (make-vect 0.40 0.60))
         (make-segment (make-vect 0.40 0.60) (make-vect 0.35 0.80))
         (make-segment (make-vect 0.35 0.80) (make-vect 0.40 1.00))
         (make-segment (make-vect 0.60 1.00) (make-vect 0.65 0.80)) ; 2 line
         (make-segment (make-vect 0.65 0.80) (make-vect 0.60 0.60))
         (make-segment (make-vect 0.60 0.60) (make-vect 0.80 0.60))
         (make-segment (make-vect 0.80 0.60) (make-vect 1.00 0.40))
         (make-segment (make-vect 1.00 0.20) (make-vect 0.80 0.50)) ; 3 line
         (make-segment (make-vect 0.80 0.50) (make-vect 0.60 0.55))
         (make-segment (make-vect 0.60 0.55) (make-vect 0.60 0.50))
         (make-segment (make-vect 0.60 0.50) (make-vect 0.80 0.00))
         (make-segment (make-vect 0.60 0.00) (make-vect 0.50 0.30)) ; 4 line
         (make-segment (make-vect 0.50 0.30) (make-vect 0.40 0.00))
         (make-segment (make-vect 0.20 0.00) (make-vect 0.40 0.50)) ; 5 line
         (make-segment (make-vect 0.40 0.50) (make-vect 0.40 0.55))
         (make-segment (make-vect 0.40 0.55) (make-vect 0.20 0.40))
         (make-segment (make-vect 0.20 0.40) (make-vect 0.00 0.60)))))

;; p.138: Transforming and combining painters
(define ((transform-painter painter origin corner1 corner2) frame)
  (let* ((m (frame-coord-map frame))
         (new-origin (m origin)))
    (painter (make-frame new-origin
                         (sub-vect (m corner1) new-origin)
                         (sub-vect (m corner2) new-origin)))))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (beside painter1 painter2)
  (let* ((split-point (make-vect 0.5 0.0))
         (paint-left
          (transform-painter painter1
                             (make-vect 0.0 0.0)
                             split-point
                             (make-vect 0.0 1.0)))
         (paint-right
          (transform-painter painter2
                             split-point
                             (make-vect 1.0 0.0)
                             (make-vect 0.5 1.0))))
    (lambda (frame)
      (paint-left frame)
      (paint-right frame))))

;; Exercise 2.50, p.140
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define rotate180 (compose rotate90 rotate90))
(define rotate270 (compose rotate90 rotate180))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

;; Exercise 2.51, p.140
(define (below painter1 painter2)
  (let* ((split-point (make-vect 0.0 0.5))
         (paint-bottom
          (transform-painter painter1
                             (make-vect 0.0 0.0)
                             (make-vect 1.0 0.0)
                             split-point))
         (paint-top
          (transform-painter painter2
                             split-point
                             (make-vect 1.0 0.5)
                             (make-vect 0.0 1.0))))
    (lambda (frame)
      (paint-bottom frame)
      (paint-top frame))))

(define (below2 painter1 painter2)
  (compose rotate90 beside rotate270))

;; Tests
(define canvas
  (make-frame (make-vect 0 500)
              (make-vect 500 0)
              (make-vect 0 -500)))

;(wave canvas)
;((corner-split wave 4) canvas)

((square-limit wave 4) canvas)
(send target save-file "ndpar-2.2.4.png" 'png)