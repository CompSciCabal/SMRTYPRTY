#lang racket

(require test-engine/racket-tests)

(define (adjoin-set x set)
  (cond ((or (empty? set) (< x (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (adjoin-set-iter x front set)
  (cond ((or (empty? set) (< x (car set))) (append (reverse front) (cons x set)))
        (else (adjoin-set-iter x (cons (car set) front)
                                 (cdr set)))))
;; Tests
(check-expect (adjoin-set 1 '()) '(1))
(check-expect (adjoin-set 1 '(0)) '(0 1))
(check-expect (adjoin-set 1 '(2)) '(1 2))
(check-expect (adjoin-set 4 '(1 2 3)) '(1 2 3 4))
(check-expect (adjoin-set 3 '(1 2 4)) '(1 2 3 4))
(check-expect (adjoin-set 1 '(2 3 4)) '(1 2 3 4))
(check-expect (adjoin-set-iter 4 '() '(1 2 3)) '(1 2 3 4))
(check-expect (adjoin-set-iter 3 '() '(1 2 4)) '(1 2 3 4))
(check-expect (adjoin-set-iter 1 '() '(2 3 4)) '(1 2 3 4))
(test)