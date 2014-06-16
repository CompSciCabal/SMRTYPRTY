#lang racket

(provide put get)

;; -------------------------------------------------------
;; Copied from Chapter 2.5
;; -------------------------------------------------------

(define *OP-TYPE-ARRAY* '())

(define make-entry list)
(define key car)
(define value cadr)

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list (make-entry k item)))
          ((equal? (key (car array)) k)
           (cons (make-entry k item) (cdr array)))
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! *OP-TYPE-ARRAY* (put-helper (list op type) *OP-TYPE-ARRAY*)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) *OP-TYPE-ARRAY*))
