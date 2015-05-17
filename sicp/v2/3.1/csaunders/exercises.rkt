#lang racket

(displayln "exercise 3.1")
(define (make-accumulator init)
  (lambda (x)
    (set! init (+ init x))
    init))

(displayln "exercise 3.2")
(define (make-monitored fn)
  (define counter 0)
  (lambda (msg)
    (cond [(eq? msg 'how-many-calls?) counter]
          [(eq? msg 'reset-count) (set! counter 0)]
          [else (begin
                  (set! counter (+ counter 1))
                  (fn msg))])))

(displayln "exercise 3.3")
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "You require more vespene gas!"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password x) "Incorrect Password")
  (define (dispatch m)
    (cond [(eq? m 'withdraw) withdraw]
          [(eq? m 'deposit) deposit]
          [else (error "Unknown Request -- MAKE-ACCOUNT" m)]))
  (define (auth pass msg)
    (if (eq? pass password)
        (dispatch msg)
        incorrect-password))
  
  (displayln "exercise 3.4")
  (define max-retry 7)
  (define retry-count 0)
  (define (security-check pass msg)
    (define (call-the-cops)
      (when (> retry-count max-retry)
        (error "Busted -- SECURITY-CHECK")))
    
    (define (update-failure-count res)
      (if (eq? res incorrect-password)
          (set! retry-count (+ retry-count 1))
          (set! retry-count 0)))
    
    (let [(res (auth pass msg))]
      (update-failure-count res)
      (call-the-cops)
      res))
  
  security-check)

(displayln "exercise 3.5")
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond [(= trials-remaining 0)
           (/ trials-passed trials)]
          [(experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1))]
          [else (iter (- trials-remaining 1) trials-passed)]))
  (iter trials 0))

(define (random-in-range low high)
  (let [(range (- high low))]
    (+ low (random (inexact->exact range)))))

(define (P x y)
  (<= (+ (expt (- x 5) 2)
         (expt (- y 7) 2))
      9)) ;; 3 ^ 2

(define (estimate-integral pred x1 x2 y1 y2 num-trials)
  (define (experiment)
    (P (random-in-range x1 x2)
       (random-in-range y1 y2)))
  (monte-carlo num-trials experiment))

(define rect-area (* (- 8.0 2.0) (- 10.0 4.0)))
(define sq-rad 9.0)

;; a = pi * r^2
;; a == rect-area * monte-carlo
;; r == sq-rad
;; ∴ pi = (/ (* rect-area monte-carlo) sq-rad)
(define estim-pi
  (/ (* rect-area (estimate-integral P 2.0 8.0 4.0 10.0 1000))
     sq-rad))
;; My estimates are super far off for some reason
estim-pi

;; I dont know how to use the prng API in order to return new procs
;; this means that all make-rands are going to be sharing the same
;; global PRNG :(
(define (make-rand seed)
  (define generate random)
  (define (reset) (random-seed seed))
  (reset)
  (lambda (msg)
    (cond [(eq? msg 'generate) (generate)]
          [(eq? msg 'reset) (reset)]
          [else (error "Unknown message -- MAKE-RAND" msg)])))
(define prng (make-rand 1024))