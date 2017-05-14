#lang racket

;;; Ex 3.28

(define (or-gate a1 a2 output) 
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay
       or-gate-delay
       (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or w1 w2)
  (cond ((= w1 1) 1)
        ((= w2 1) 1)
        (else 0)))

(define (add-action! w1) 1)
(define (get-signal w1) 1)
(define (set-signal! w1) 1)
(define (after-delay w1) 1)
(define or-gate-delay 1)

;;; Ex 3.29

(define (composite-or w1 w2)
  (not (and (not w1) (not w2))))

; this is OR defined by ANDs and NOTs sans boilerplate
; the time delay is 2*NOT-delay + AND-delay

;;; Ex 3.30


