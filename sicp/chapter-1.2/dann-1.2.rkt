#lang racket


;;; SICP CHAPTER 1.2

;;; SICP EX 1.9

; A is recursive because you have to retain the inc's
; B is iterative because each call contains all needed knowledge

;;; SICP EX 1.10 

(define (A x y) 
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y)) 
        ((= y 1) 2) 
        (else (A (- x 1) (A x (- y 1))))))

(define (f n) (A 0 n)) ;; 2n
(define (g n) (A 1 n)) ;; 2^n
(define (h n) (A 2 n)) ;; power tower of 2s n high

;;; ;;;; also fib dim

;;; EX 1.11

