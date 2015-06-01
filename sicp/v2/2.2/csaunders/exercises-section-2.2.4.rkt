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
(define ycor-vect cdr)

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

;; Exercise 2.48
(define my-make-segment cons)
(define my-start-segment car)
(define my-end-segment cdr)

;; Exercise 2.49

;; Not an original idea, but found it on a blog
;; post and figured connecting dots makes way
;; more sense.
(define (connect points)
  (if (empty? (cdr points))
      '()
      (cons (make-vector (car points)
                         (cadr points))
            (connect (cdr points)))))
        
(define one 0.99)
(define o (make-vector 0 0))
(define x (make-vector one 0))
(define y (make-vector 0 one))
(define z (make-vector one one))

(define half-x (scale-vect 0.5 x))
(define half-y (scale-vect 0.5 y))

(define outline (segments->painter
                 (connect (list o x z y o))))

(define cross (segments->painter
               (list (make-segment x y)
                     (make-segment o z))))
(define diamond (segments->painter
                 (connect
                  (list
                   half-y
                   (add-vect half-x y)
                   (add-vect x half-y)
                   half-x
                   half-y))))

(define wave-segs (append
               (connect (list
                         (make-vect 0.45 0)
                         (make-vect 0.5 0.33) ;; Inside Legs
                         (make-vect 0.55 0)))
               (connect (list
                         (make-vect 0.65 0)
                         (make-vect 0.55 0.5) ;; Lower Right Arm
                         (make-vect one 0.25)))
               (connect (list
                         (make-vect one 0.45)
                         (make-vect 0.80 0.65)
                         (make-vect 0.55 0.65) ;; Upper Right Arm
                         (make-vect 0.60 0.80) ;; and Right Head
                         (make-vect 0.55 one)))
               (connect (list
                         (make-vect 0.45 one)
                         (make-vect 0.40 0.80)
                         (make-vect 0.45 0.65) ;; Upper Left Arm
                         (make-vect 0.40 0.65) ;; and Left Head
                         (make-vect 0.25 0.60)
                         (make-vect 0 0.75)))
               (connect (list
                         (make-vect 0 0.65)
                         (make-vect 0.25 0.55)
                         (make-vect 0.40 0.60)
                         (make-vect 0.45 0.55)
                         (make-vect 0.30 0)))))
(define wave (segments->painter wave-segs))
   
;; (paint outline)
;; (paint cross)
;; (paint diamond)
;; (paint wave)

;; Exercise 2.50
(define (rotate-90 painter)
  ((transform-painter (make-vect one 0)
                     (make-vect one one)
                     (make-vect 0 0))
   painter))

(define (my-flip-horiz painter)
  ((transform-painter (make-vect one 0)
                     (make-vect 0 0)
                     (make-vect one one)) painter))

(define (rotate-180 painter)
  ((transform-painter (make-vect one one)
                      (make-vect 0 one)
                      (make-vect one 0))
   painter))

(define (rotate-270 painter)
  ((transform-painter (make-vect 0 one)
                      (make-vect 0 0)
                      (make-vect one one))
   painter))

;; One way using cheats
(define (lazy-rotate-270 painter)
  (rotate-180 (rotate-90 painter)))

;; Exercise 2.51
(define (my-below p1 p2)
  (let [(split-point (make-vector 0 0.5))]
    (let [(paint-top
           ((transform-painter (make-vector 0 0)
                               (make-vector 1 0)
                               split-point)
            p1))
          (paint-bottom
           ((transform-painter split-point
                               (make-vector 1 0.5)
                               (make-vector 0 1))
            p2))]
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))


(define (my-below-rots p1 p2)
  (rotate-270 (beside (rotate-90 p2) (rotate-90 p1))))

;; (paint (my-below einstein diagonal-shading))
;; (paint (my-below-rots einstein diagonal-shading))

;; Exercise 2.52
(define more-wave
  (append wave-segs
          (connect (list
                    (make-vector 0.45 0.75)
                    (make-vector 0.50 0.70)
                    (make-vector 0.55 0.75)))))
;; Exercise 2.52 a.
;;(paint (segments->painter more-wave))

;; Exercise 2.52 b.
(define (my-corner-split painter n)
  (if (= n 0)
      painter
      (let [(up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (my-corner-split painter (- n 1)))]
        (beside (below painter up)
                (below right corner)))))
(paint (my-corner-split einstein 2))

;; Exercise 2.52 c.
(define (identity x) x)
(define (square-of-four tl bl tr br)
  (lambda (painter)
    (let [(top (beside (tl painter) (tr painter)))
          (bot (beside (bl painter) (br painter)))]
      (below bot top))))

;; For some reason my rotate 180 implementation is
;; like half broken :(
(define (my-sq-limit painter n)
  (let [(combine4 (square-of-four flip-vert identity
                                  rotate180 flip-horiz))]
    (combine4 (my-corner-split painter n))))

(paint (my-sq-limit einstein 4))