#lang racket

;; 2.40

(define (prime? n)
  (define (check-prime n divisor)
    (cond [(> divisor (sqrt n))
           #t]
          [(= (modulo n divisor) 0)
           #f]
          [else (check-prime n (+ divisor 1))]))
  (check-prime n 2))

(define (flatmap proc seq)
  (foldr append empty (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) 
        (cadr pair) 
        (+ (car pair) (cadr pair))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low
            (enumerate-interval
              (+ low 1)
              high))))

(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) 
             (list i j))
               (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter 
        prime-sum?
        (unique-pairs n))))

(prime-sum-pairs 6)


;; 2.41
;; unfinished!
#| (define (unique-triples n) |#
#|   (flatmap |#
#|     (lambda (i) |#
#|       (flatmap (lambda (j) |#
#|                  (map (lambda (k) |#
#|                         (list i j k)) |#
#|                         (enumerate-interval 1 (- i 1)))) |#
#|                (enumerate-interval 1 (- j 1)))) |#
#|     (enumerate-interval 1 n))) |#
#| (define (triples-that-sum-to n s) |#
#|   (filter |#
#|     sum-to-s |#
#|     (unique-triples n))) |#

#| (unique-triples 4) |#
