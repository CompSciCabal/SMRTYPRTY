#lang racket

(define (last-pair lst)
  (if (null? (cdr lst)) (car lst)
    (last-pair (cdr lst))))

(define (append lst ele)
  (cond [(null? lst) (cons ele null)]
        [else (cons (car lst)
                    (append (cdr lst) ele))]))

#| (define (reverse lst) |#
#|   (cond [(null? lst) null] |#
#|         [else (append (reverse (cdr lst)) (car lst))])) |#

(define (reverse lst)
  (define (reverse-helper lst ans)
    (cond [(empty? lst) ans]
          [else (reverse-helper (cdr lst)
                                (cons (car lst) ans))]))
  (reverse-helper lst empty))

(define (parity? x y)
  (= (modulo (- x y) 2) 0))

(define (same-parity num . lst)
  (define (same-parity-helper num lst)
    (cond [(null? lst) null]
          [(parity? num (car lst)) (cons (car lst) (same-parity-helper num (cdr lst)))]
          [else (same-parity-helper num (cdr lst))]))
  (same-parity-helper num lst))