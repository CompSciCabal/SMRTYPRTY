#lang racket

;;; PRELUDE

(define balance 100) 
(define (withdraw amount) 
  (if (>= balance amount)
      (begin (set! balance (- balance amount)) 
             balance)
      "Insufficient funds"))

(define new-withdraw 
  (let ((balance 100))
    (lambda (amount) 
      (if (>= balance amount)
          (begin (set! balance (- balance amount)) 
                 balance)
          "Insufficient funds"))))

(define (make-withdraw balance) 
  (lambda (amount)
    (if (>= balance amount) 
        (begin (set! balance (- balance amount))
               balance) 
        "Insufficient funds")))


(define weird1 
  (let ((start 10))
    (lambda (one)
      (lambda (two)
        (+ start one two)))))

(define (weird2 one)
  (let ((start 10))
    (lambda (two)
      (+ start one two))))

(define weird3 
  (let ((start 10))
    (begin (set! start 5)
           (lambda (one)
             (+ start one)))))

(define (make-account balance) 
  (define (withdraw amount) 
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) 
               balance)
        "Insufficient funds")) 
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance) 
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw) 
          ((eq? m 'deposit) deposit) 
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)


;;; 3.1

(define (make-accumulator sum)
  (lambda (value)
    (set! sum (+ sum value))
    sum))

; (define A (make-accumulator 5))
; (A 10)
;   15
; (A 10)
;   25

;;; 3.2

(define (make-monitored f)
  (let ((call-count 0))
    (lambda (value)
      (if (eq? value 'how-many-calls?)
          call-count
          (begin (set! call-count (+ 1 call-count))
                 (f value))))))

; (define s (make-monitored sqrt)) (s 100)
;   10
; (s 'how-many-calls?)
;   1

;;; 3.3

(define (make-protected-account balance password) 
  (define (withdraw amount) 
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) 
               balance)
        "Insufficient funds")) 
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance) 
  (define (dispatch p m)
    (cond ((not (eq? p password)) (error "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'clone) dispatch)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

; (define acc (make-protected-account 100 'secret-password))
; ((acc 'secret-password 'withdraw) 40)
;   60
; ((acc 'some-other-password 'deposit) 50)
;   Incorrect password

;;; 3.4

(define (make-extra-protected-account balance password) 
  (define incorrect-count 0)
  (define (withdraw amount) 
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) 
               balance)
        "Insufficient funds")) 
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance) 
  (define (dispatch p m)
    (if (eq? p password)
        (begin (set! incorrect-count 0)
               (cond ((eq? m 'withdraw) withdraw) 
                     ((eq? m 'deposit) deposit) 
                     (else (error "Unknown request: MAKE-ACCOUNT" m))))
        (if (< incorrect-count 7)
            (begin (set! incorrect-count (+ 1 incorrect-count))
                   (error "Incorrect password"))
            (error "calling the cops!!!"))))
  dispatch)

;; this works, but is slightly annoying to test in DrRacket

;;; 3.5

(define (random-in-range low high) 
  (let ((range (- high low)))
    (+ low (random range) (random))))

(define (monte-carlo trials experiment) 
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) 
           (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1)
                              (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) 
                 trials-passed))))
  (iter trials 0))

(define (estimate-pi trials) 
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test) 
  (= (gcd (random 1000000) (random 1000000)) 1))

; weird...
; > (estimate-pi 100)
; 3.2163376045133845
; > (estimate-pi 1000)
; 3.143990895015552
; > (estimate-pi 10000)
; 3.1211922734731212
; > (estimate-pi 100000)
; 3.1415334897403415
; > (estimate-pi 1000000)
; 3.140812882551379


(define (estimate-integral P x1 x2 y1 y2 trials)
  (define new-P 
    (lambda () (P (random-in-range x1 x2) (random-in-range y1 y2))))
  (* (- x2 x1) (- y2 y1) (monte-carlo trials new-P)))

(define (circle-P x y)
  (<= (+ (sqr (- x 5)) (sqr (- y 7))) 9))
   
; (+ (estimate-integral circle-P 2 8 4 10 100000) 0.0)
;   28.2654
; (* pi 9)
;   28.274333882308138


;;; 3.6

(define (rand-update x)
  (begin (display x) (modulo (* x 48271) 2147483647)))

(define (rand x)
  (define (generate)
    (begin (set! x (rand-update x)) x))
  (define (reset new-x)
    (set! x new-x))
  (define (dispatch m)
    (cond ((eq? m 'generate) (generate))
          ((eq? m 'reset) reset) 
          (else (error "Unknown request: " m))))
  dispatch)

;;; 3.7

(define (make-joint account master newpass)
  (let ((old-dispatch (account master 'clone)))
    (define (dispatch p m)
      (if (eq? p newpass)
          (old-dispatch master m)
          (error "Incorrect password")))
    dispatch))

; (define acc (make-protected-account 100 'asdf))
; (define acc2 (make-joint acc 'asdf 'foo))
; ((acc 'asdf 'withdraw) 10)
;   90
; (( acc2 'foo 'withdraw) 10)
;   80
; ((acc 'asdf 'withdraw) 10)
;   70

;;; 3.8

(define f
  (let ((current 0))
    (lambda (n)
      (define foo current)
      (set! current n)
      foo)))

; (+ (f 0) (f 1))
; (+ (f 1) (f 0))

