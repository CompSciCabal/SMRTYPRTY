#lang racket

; a)

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine-with-counter angle counter)
  (display counter )
  (newline)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine-with-counter (/ angle 3.0) (+ counter 1)))))

(define (sine angle)
  (sine-with-counter angle 1))

;> (sine 12.15)
;123456-0.39980345741334
;sine has been applied 6 times

;; expand 

(sine 12.15)
(p (sine (/ 12.15 3.0)))
(p (p (sine (/ (/ 12.15 3.0) 3.0)))))
(p (p (p (sine (/ 12.15 3.0 3.0 3.0))))))
(p (p (p (p (sine (/ 12.15 3.0 3.0 3.0 3.0)))))))
(p (p (p (p (p (sine (/ 12.15 3.0 3.0 3.0 3.0 3.0)))))))
(p (p (p (p (p 0.04))))));0.04

; b)
;; The order of growth in space is (log 3 a)
;; The order of growth in steps should be proportional to 
;; the order of growth in space, which is also (log 3 a)