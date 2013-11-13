#lang racket
;; wip

;; representing a rubiks cube ...
;; 1 - white, 2 - yellow, 3 - red, 4 - orange, 5 - blue, 6 - green
(define colour-map (list (list 'top 1) (list 'bottom 2) (list 'left 3)
                         (list 'right 4) (list 'front 5) (list 'back 6)))

(define (dict-value key dict)
  (if (equal? key (first (first dict)))
      (second (first dict))
      (dict-value key (cdr dict))))    

(define (make-cube-side side)
         (build-list 9 (lambda (x) (- x (- x (dict-value side colour-map))))))

(define (make-cube)
  (list (make-cube-side 'top) (make-cube-side 'bottom)
        (make-cube-side 'left) (make-cube-side 'right)
        (make-cube-side 'front) (make-cube-side 'back)))

;; front back wtf
(define (replace old new place side cube)
  (define side-number (dict-value side colour-map))
  
  (define (make-new-lst old new place count front back cubey)
    (if (equal? count place)
        ;; note: append doesn't exist ... use union from before?
        (append front (cons new (rest back)))
        (make-new-lst old new place (+ 1 count) (cons (first cubey) first) (rest cubey))))
        
  (define (helper count front back) ;; init count from 1
    (if (equal? count side-number) ;; you've found the row
        'do-something-clever
        (helper (+ 1 count) (rest cube) (cons (first cube) back))))
  

;; moving the rubiks cube around: there are 12 possible moves ...
;; fc, fcc (front-clockwise, front-counter-clockwise)
;; bac, bacc (back)
;; lc, lcc
;; rc, rcc
;; tc, tcc
;; boc, bocc (bottom)
;; I guess for each, I have to manually determine the transformations ...
;; I probably didn't pick the most beautiful, symmetry inducing representation.
;; each will require 17 transformations in a naive implementation :( :( :( :(

;; note that blue is always the side which is facing you and white is on top
(define (fc cube)
  (define original-cube cube)
  ;; cases 1 - 3
  (set! cube (replace (seventh (first cube)) (ninth (third original-cube)))
  cube)
  ;; cases 4 - 6
  ;; cases 7 - 9
  ;; cases 10 - 12
  ;; cases 13 - 15
  ;; cases 16 - 17

;  (define (fc cube)
;  (define original-cube cube)
;  ;; cases 1 - 3
;  ;; cases 4 - 6
;  ;; cases 7 - 9
;  ;; cases 10 - 12
;  ;; cases 13 - 15
;  ;; cases 16 - 18
;
;(define (fc cube)
;  (define original-cube cube)
;  ;; cases 1 - 3
;  ;; cases 4 - 6
;  ;; cases 7 - 9
;  ;; cases 10 - 12
;  ;; cases 13 - 15
;  ;; cases 16 - 18
    
