;;; The givens:

;; (define wave2 (beside wave (flip-vert wave)))
;; (define wave4 (below wave2 wave2))

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

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;;; 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;;; 2.45
;; Our target is

; (define right-split (split beside below))
; (define up-split (split below beside))

;; so...

(define (split fst snd)
  (define (rec painter n)
    (if (= n 0)
        painter
        (let ((smaller (rec painter (- n 1))))
          (fst painter (snd smaller smaller)))))
  rec)

;;; More givens:
(define (frame-coord-map frame)
  (lambda (v)
    (vector-+
     (frame-origin frame)
     (vector-+ (scale-vect (xcor-vect v)
                           (frame-edge1 frame))
               (scale-vect (ycor-vect v)
                           (frame-edge2 frame))))))

;;; 2.46
(define (make-vector x y) (list x y))
(define (vector-x vec) (car vec))
(define (vector-y vec) (cadr vec))

(define (vector-+ vec-a vec-b) (map + vec-a vec-b))
(define (vector-- vec-a vec-b) (map - vec-a vec-b))
(define (vector-scale scalar vec) (map (lambda (coord) (* scalar coord)) vec))

;;; 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (frame-origin frame) (car frame))
(define (frame-edge1 frame) (cadr frame))
(define (frame-edge2 frame) (caddr frame))

;; (define (make-frame origin edge1 edge2)
;;   (cons origin (cons edge1 edge2)))

;; The first two selectors work on this representation too. Only edge2 would need to change:
;
;    (define (edge2 frame) (cddr frame))

;;; Yet more givens:
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))

;;; 2.48
(define (make-segment start end)
  (cons start end))
(define (segment-start seg) (car seg))
(define (segment-end seg) (cdr seg))

;;; 2.49
;a)
(define (outline-painter frame)
  (segments->painter
   (let ((a (frame-origin frame))
         (b (frame-edge1 frame))
         (c (vector-+ (frame-edge1 frame)
                      (frame-edge2 frame)))
         (d (frame-edge2)))
     (list (make-segment a b)
           (make-segment b c)
           (make-segment c d)
           (make-segment d a)))))

;b)
(define (cross-painter frame)
  (segments->painter
   (let ((a (frame-origin frame))
         (b (frame-edge1 frame))
         (c (vector-+ (frame-edge1 frame)
                      (frame-edge2 frame)))
         (d (frame-edge2)))
     (list (make-segment a c)
           (make-segment b d)))))

;c)
(define (average . num-list)
  (/ (apply + num-list) (length num-list)))

(define (vector-midpoint vec-a vec-b)
  (map average vec-a vec-b))

(define (diamond-painter frame)
  (segments->painter
   (let ((a (frame-origin frame))
         (b (frame-edge1 frame))
         (c (vector-+ (frame-edge1 frame)
                      (frame-edge2 frame)))
         (d (frame-edge2)))
     (let ((mid-a (vector-midpoint a b))
           (mid-b (vector-midpoint b c))
           (mid-c (vector-midpoint c d))
           (mid-d (vector-midpoint d a))))
     (list
      (make-segment mid-a mid-b)
      (make-segment mid-b mid-c)
      (make-segment mid-c mid-d)
      (make-segment mid-d mid-a)))))

;d)
;; .... no. Ok, ok, not right now at least.