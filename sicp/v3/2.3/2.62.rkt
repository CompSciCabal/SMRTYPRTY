#lang racket

(require test-engine/racket-tests)

(define (union-set set1 set2)
  (cond ((empty? set1) set2)
        ((empty? set2) set1)
        ((= (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))
        (else (cons (car set2) (union-set set1 (cdr set2))))))

(define (union-set-iter set1 set2 acc)
  (cond ((empty? set1) (append (reverse acc) set2))
        ((empty? set2) (append (reverse acc) set1))
        ((= (car set1) (car set2)) (union-set-iter (cdr set1) (cdr set2) (cons (car set1) acc)))
        ((< (car set1) (car set2)) (union-set-iter (cdr set1) set2 (cons (car set1) acc)))
        (else (union-set-iter set1 (cdr set2) (cons (car set2) acc)))))

;; Tests
(check-expect (union-set '(1 2 3) '(1 2 3 4)) '(1 2 3 4))
(check-expect (union-set '(1 2 3 4) '(2 3 4 5)) '(1 2 3 4 5))
(check-expect (union-set-iter '(1 2 3) '(1 2 3 4) '()) '(1 2 3 4))
(check-expect (union-set-iter '(1 2 3 4) '(2 3 4 5) '()) '(1 2 3 4 5))
(test)