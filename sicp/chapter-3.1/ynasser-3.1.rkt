#lang racket

;; Exercise 3.1
(define (make-accumulator a)
  (lambda (x) (begin (set! a (+ a x)) a)))

;; Exercise 3.2
;; This works, but I don't quite understand why. Is it just because of the
;; order in which things are called?
(define (make-monitored fn)
  (define counter 0)
  (define (mf n)
    (cond
      ((eq? n 'how-many-calls) counter)
      ((eq? n 'reset) (set! counter 0))
      (else (set! counter (+ 1 counter))
            (fn n))))
  mf)

;; Exercises 3.3, 3.4, 3.7
;The first is a password-protected account. The second argument must match the password with
;which the account was defined in order for the make-joint operation to proceed. The third argument is
;a new password. Make-joint is to create an additional access to the original account using the new
;password. For example, if peter-acc is a bank account with password open-sesame, then
;(define paul-acc
;  (make-joint peter-acc 'open-sesame 'rosebud))
;will allow one to make transactions on peter-acc using the name paul-acc and the password
;rosebud. You may wish to modify your solution to exercise 3.3 to accommodate this new feature.

(define (make-account balance password)
  (define bad 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops)
    (error "The lisp police have been alerted -- they have parentheses instead of tasers!"))
  (define (dispatch p m)
    (cond
      ((> bad 7) (call-the-cops))
      ((not (eq? p password)) (set! bad (+ 1 bad)) (error "Incorrect password"))
      ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      (else (error "Unknown request -- MAKE-ACCOUNT"
                   m))))
  dispatch)

;((acc 'some-other-password 'deposit) 50)
;(define paul-acc
;  (make-joint peter-acc 'open-sesame 'rosebud))

(define (make-joint acct orig-pw second-pw)
  (define (dispatch p m)
    (cond
      ((not (eq? second-pw p)) (error "Bad password"))
      ((eq? m 'withdraw) (acct orig-pw m))
      ((eq? m 'deposit) (acct orig-pw m))
      (else (error "What"))))
  dispatch)

;; Exercise 3.5
;; TODO
;(define rand
;  (let ((x random-init))
;    (lambda ()
;      (set! x (rand-update x))
;      x)))

;; Exercise 3.6
;; TODO

;; Exercise 3.8
;; Define a simple procedure f such that evaluating (+ (f 0) (f 1)) will return 0 if the arguments to + are
;; evaluated from left to right but will return 1 if the arguments are evaluated from right to left.

