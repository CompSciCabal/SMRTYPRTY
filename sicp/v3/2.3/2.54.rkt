#lang racket

(require test-engine/racket-tests)

(define (equal? a b)
  (cond ((and (symbol? a) (symbol? b)) (eq? a b))
        ((empty? a) (empty? b))
        ((empty? b) #f)
        ((and (list? a) (list? b)) (and (equal? (car a) (car b))
                                        (equal? (cdr a) (cdr b))))
        (else #f)))

;; Tests
(check-expect (equal? '() '()) #t)
(check-expect (equal? '(this is a list) '(this is a list)) #t)
(check-expect (equal? '(this (is a) list) '(this is a list)) #f)
(check-expect (equal? '(this list is longer) '(this list is)) #f)
(check-expect (equal? '(this (is a) list) '(this (is a) list)) #t)
(test)
