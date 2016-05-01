
#lang sicp
(#%require sicp-pict)

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter 
                                  (- n 1))))
        (beside painter 
                (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right 
                                   right))
              (corner (corner-split painter 
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right 
                         corner))))))


;; Exercise 2.44: Define the procedure up-split used by corner-split. It is similar to right-split, except that it switches the roles of below and beside.

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter 
                               (- n 1))))
        (below painter
               (beside smaller smaller)))))

;; Exercise 2.45: Right-split and up-split can be expressed as instances of a general splitting operation. Define a procedure split with the property that evaluating

;; (define right-split (split beside below))
;; (define up-split (split below beside))

;; produces procedures right-split and up-split with the same behaviors as the ones already defined.

(define (split f g)
  (define (splitter painter n)
    (if (= n 0)
        painter
        (let ((smaller (splitter painter 
                                    (- n 1))))
          (f painter 
                  (g smaller smaller)))))
  splitter)


;; Exercise 2.46: A two-dimensional vector vv running from the origin to a point can be represented as a pair consisting of an x-coordinate and a y-coordinate.
;; Implement a data abstraction for vectors by giving a constructor make-vect and corresponding selectors xcor-vect and ycor-vect. In terms of your selectors and constructor,
;; implement procedures add-vect, sub-vect, and scale-vect that perform the operations vector addition, vector subtraction, and multiplying a vector by a scalar:


(define make-vect2 cons)
(define xcor-vect2 car)
(define ycor-vect2 cdr)

(define (add-vect2 v1 v2)
  (make-vect (+ (xcor-vect2 v1) (xcor-vect2 v2))
             (+ (ycor-vect2 v1) (ycor-vect2 v2))))

(define (sub-vect2 v1 v2)
  (make-vect (- (xcor-vect2 v1) (xcor-vect2 v2))
             (- (ycor-vect2 v1) (ycor-vect2 v2))))

(define (scale-vect2 s v)
  (make-vect (* s (xcor-vect2 v))
             (* s (ycor-vect2 v))))


;; Exercise 2.47: Here are two possible constructors for frames:

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

;; For each constructor supply the appropriate selectors to produce an implementation for frames.

(define origin-frame1 car) ;; same for both
(define origin-frame2 car) 
(define edge1-frame1 cadr) ;; same for both
(define edge1-frame2 cadr) ;; same for both
(define edge2-frame1 cadr) 
(define edge2-frame2 cddr)

;; Exercise 2.48: A directed line segment in the plane can be represented as a pair of vectors—the vector running from the origin to the start-point of the segment,
;; and the vector running from the origin to the end-point of the segment. Use your vector representation from Exercise 2.46 to define a representation for segments
;; with a constructor make-segment and selectors start-segment and end-segment.

(define (make-segment2 start end)
  (cons start end))

(define start-segment2 car)

(define end-segment2 cdr)

;; Exercise 2.49: Use segments->painter to define the following primitive painters:

(define bl (make-vect 0 0))
(define br (make-vect 1 0))
(define tl (make-vect 0 1))
(define tr (make-vect 1.0 1.0))
(define ml (make-vect 0 .5))
(define mr (make-vect 1 .5))
(define mb (make-vect .5 0))
(define mt (make-vect .5 1))

;; 1) The painter that draws the outline of the designated frame.

;; we dont get the top or the right part of the frame here, I think it's an issue with the painter in Racket though (?)
(define outline-painter
  (segments->painter
   (list (make-segment bl br)
         (make-segment bl tl)
         (make-segment br tr)
         (make-segment tl tr))))
;; 2) The painter that draws an “X” by connecting opposite corners of the frame.
(define x-painter
  (segments->painter
   (list (make-segment bl tr)
         (make-segment br tl))))
;; 3) The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
(define diamond-painter
  (segments->painter
   (list (make-segment mb ml)
         (make-segment ml mt)
         (make-segment mt mr)
         (make-segment mr mb))))
;; 4) The wave painter.
;; Skipping for now

;; Exercise 2.50: Define the transformation flip-horiz, which flips painters horizontally, and transformations that rotate painters counterclockwise by 180 degrees and 270 degrees.

;; SICP's transform-painter takes 4 arguments, the one in Racket takes 3. I think the one in Racket returns a fn which acts on a painter, where as SICP's acts on the painter directly.
(define flip-horiz
  (transform-painter tl tr bl))

(define (rotate180 painter)
  (rotate90 (rotate90 painter)))

(define (rotate270 painter)
  (rotate90 (rotate180 painter)))

;; Exercise 2.51: Define the below operation for painters. Below takes two painters as arguments.
;; The resulting painter, given a frame, draws with the first painter in the bottom of the frame and with the second painter in the top.
;; Define below in two different ways—first by writing a procedure that is analogous to the beside procedure given above, and again in terms
;; of beside and suitable rotation operations (from Exercise 2.50).

;; Again transform painter is a bit diff here
(define (below1 painter1 painter2)
  (let ((paint-bottom  (transform-painter 
                        bl
                        br
                        ml))
        (paint-top (transform-painter
                    ml
                    mr
                    tl)))
                      
      (lambda (frame)
        ((paint-bottom painter1) frame)
        ((paint-top painter2) frame))))


(define (below2 painter1 painter2)                      
      (rotate270 (beside (rotate90 painter2) (rotate90 painter1))))


;; Exercise 2.52: Make changes to the square limit of wave shown in Figure 2.9 by working at each of the levels described above. In particular:

;; 1) Add some segments to the primitive wave painter of Exercise 2.49 (to add a smile, for example).
;; 2) Change the pattern constructed by corner-split (for example, by using only one copy of the up-split and right-split images instead of two).
;; 3) Modify the version of square-limit that uses square-of-four so as to assemble the corners in a different pattern. (For example, you might make the big Mr. Rogers look outward from each corner of the square.)
