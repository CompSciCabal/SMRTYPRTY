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

(define (frame-coord-map frame)
  (lambda (v)
    (vector-+
     (frame-origin frame)
     (vector-+ (vector-scale (vector-x v)
                             (frame-edge1 frame))
               (vector-scale (vector-y v)
                             (frame-edge2 frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (segment-start segment))
        ((frame-coord-map frame)
         (segment-end segment))))
     segment-list)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (vector-- (m corner1) new-origin)
                  (vector-- (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vector 0.0 1.0)
                     (make-vector 1.0 1.0)
                     (make-vector 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vector 0.5 0.5)
                     (make-vector 1.0 0.5)
                     (make-vector 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vector 1.0 0.0)
                     (make-vector 1.0 1.0)
                     (make-vector 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vector 0.0 0.0)
                     (make-vector 0.65 0.35)
                     (make-vector 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vector 0.5 0.0)))
    (let ((paint-left (transform-painter painter1
                                         (make-vector 0.0 0.0)
                                         split-point
                                         (make-vector 0.0 1.0)))
          (paint-right (transform-painter painter2
                                          split-point
                                          (make-vector 1.0 0.0)
                                          (make-vector 0.5 1.0))))
      (lambda (frame) (paint-left frame) (paint-right frame)))))

;;; 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;;; 2.45
;; Our target is

;   (define right-split (split beside below))
;   (define up-split (split below beside))

;; so...

(define (split fst snd)
  (define (rec painter n)
    (if (= n 0)
        painter
        (let ((smaller (rec painter (- n 1))))
          (fst painter (snd smaller smaller)))))
  rec)

(define right-split (split beside below))
(define up-split (split below beside))

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

;    (define (edge2 frame) (cddr frame))

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
         (d (frame-edge2 frame)))
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
         (d (frame-edge2 frame)))
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
         (d (frame-edge2 frame)))
     (let ((mid-a (vector-midpoint a b))
           (mid-b (vector-midpoint b c))
           (mid-c (vector-midpoint c d))
           (mid-d (vector-midpoint d a)))
       (list
        (make-segment mid-a mid-b)
        (make-segment mid-b mid-c)
        (make-segment mid-c mid-d)
        (make-segment mid-d mid-a))))))

;d)
;; .... Here's a shitty minimalist version.
;; To actually replicate it, we'd need a notional primitive
;; called "curve", and we'd need to compute a fuckton of points,
;; which I really don't have time to do just now.
(define (wave-painter frame)
  (rotate90
   (segments->painter
    (let ((a (frame-origin frame))
          (b (frame-edge1 frame))
          (c (vector-+ (frame-edge1 frame)
                       (frame-edge2 frame)))
          (d (frame-edge2 frame)))
      (letrec ((mid-a (vector-midpoint a b))
               (mid-b (vector-midpoint b c))
               (mid-c (vector-midpoint c d))
               (mid-d (vector-midpoint d a))
               (center (vector-midpoint mid-a mid-c))
               (q1b (vector-midpoint b mid-b))
               (q3b (vector-midpoint mid-b c)))
        (list
         (make-segment mid-a center)
         (make-segment center q1b)
         (make-segment center q3b)
         (make-segment q3b center)
         (make-segment center mid-c)
         (make-segment mid-c center)
         (make-segment center mid-d)
         (make-segment mid-d center)
         (make-segment mid-d center)
         (make-segment center mid-a)))))))

;;; 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vector 1.0 0.0)
                     (make-vector 0.0 0.0)
                     (make-vector 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vector 1.0 1.0)
                     (make-vector 0.0 1.0)
                     (make-vector 1.0 0.0)))
(define (rotate270 painter)
  (transform-painter painter
                     (make-vector 0.0 1.0)
                     (make-vector 0.0 0.0)
                     (make-vector 1.0 1.0)))

;;; 2.51
(define (below painter1 painter2)
  (let ((split-point (make-vector 0.0 0.5)))
    (let ((paint-top (transform-painter painter2
                                        split-point
                                        (make-vector 1.0 0.5)
                                        (make-vector 0.0 1.0)))
          (paint-bottom (transform-painter painter1
                                           (make-vector 0.0 0.0)
                                           (make-vector 1.0 0.0)
                                           split-point)))
      (lambda (frame) (paint-top frame) (paint-bottom frame)))))

(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter2)
                    (rotate270 painter1))))

;;; 2.52

;; -_-

;;; BONUS ROUND
;; (load "inaimathi-2.2.4.scm")

(define *scale* 300)

(define (draw-line a b)
  (printf
   "<line stroke-width=\"1\" stroke=\"black\" x1=\"~a\" y1=\"~a\" x2=\"~a\" y2=\"~a\" />"
   (* *scale* (vector-x a))
   (* *scale* (vector-y a))
   (* *scale* (vector-x b))
   (* *scale* (vector-y b))))

(define (->svg painter frame filename)
  (with-output-to-file filename
    (lambda ()
      (print "<svg>")
      (painter frame)
      (print "</svg>"))))

;;; TESTING
(define *frame*
  (make-frame (make-vector 0 0)
              (make-vector 1 0)
              (make-vector 0 1)))

(->svg (diamond-painter *frame*) *frame* "single-diamond.svg")
(->svg (outline-painter *frame*) *frame* "single-outline.svg")
(->svg (cross-painter *frame*) *frame* "single-cross.svg")
(->svg (wave-painter *frame*) *frame* "single-wave.svg")

(->svg (corner-split (diamond-painter *frame*) 5) *frame* "DIAMONDS-MOTHERFUCKER.svg")
(->svg (corner-split (outline-painter *frame*) 5) *frame* "OUTLINES-MOTHERFUCKER.svg")
(->svg (corner-split (cross-painter *frame*) 5) *frame* "CROSSES-MOTHERFUCKER.svg")
(->svg (corner-split (wave-painter *frame*) 5) *frame* "ALSO-WAVES-MOTHERFUCKER-I-GUESS?.svg")

(->svg (square-limit (wave-painter *frame*) 5) *frame* "square-limit-wave.svg")

;; Incidentally, this is the sort of situation where having dynamic scope would be fucking awesome.
;; If we had that, we could easily re-define `draw-line` inside of something like ->svg.
;; That would let us use the same program for whatever kind of output we'd like. As it stands, what we'd
;; have to do is either change draw-line for each representation, OR, make draw-line collect output into some
;; globally accessible variable, the output of which we'd then funnel into different formats.

;; (let ((f (make-frame (make-vector 0 0)
;;                      (make-vector 4 4)
;;                      (make-vector 3 3)))
;;       (draw-line (lambda (a b)
;;                    ;; insert whatever implementation you like at the time.
;;                    )))
;;   ((diamond-painter f) f))