#lang racket

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low 
            (enumerate-interval
              (+ low 1) 
              high))))

(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y)) 
              null sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (el counter) (+ counter 1)) 0 sequence))

(define
  (horner-eval x coeff-seq)
  (accumulate
    (lambda (coeff higher-terms)
      (+ coeff (* higher-terms x)))
    0
    coeff-seq))

(define (count-leaves t)
  #| TODO |#
  #| (accumulate (lambda (left rest-of-tree) ()) |#
  #|             0 |#
  #|             (map (lambda (sub-t) ()) t)) |#

;; tests
(map (lambda (x) (* x x)) (list 1 2 3 4 5))
(append (list 1 2 3 4 5) (list 6 7 8 9 10))
(length (list 1 2 3 4))
(horner-eval 2 (list 1 3 0 5 0 1)) ; should be 79
