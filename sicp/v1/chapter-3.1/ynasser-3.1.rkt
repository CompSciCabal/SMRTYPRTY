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

;(define (estimate-pi trials)
;  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
;(define (cesaro-test)
;  (= (gcd (rand) (rand)) 1))
;(define (monte-carlo trials experiment)
;  (define (iter trials-remaining trials-passed)
;    (cond ((= trials-remaining 0)
;           (/ trials-passed trials))
;          ((experiment)
;           (iter (- trials-remaining 1) (+ trials-passed 1)))
;          (else
;           (iter (- trials-remaining 1) trials-passed))))
;  (iter trials 0))


(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

;(define (estimate-integral P x1 y1 x2 y2 num-trials)
; )


;; Exercise 3.6
;; I can't figure out how to make seed an internal state variable while 
;; new-rand is a procedure.
(define seed 1)
(define (new-rand s)
  (define (rand-gen)
    (set! seed (+ 1 seed))
    (modulo (* 3847583 seed) 27343)) ;; fake randomness
  (define dispatch
    (cond
      ((eq? s 'generate) (rand-gen))
      ((eq? s 'reset) (lambda (n) (set! seed n)))))
  dispatch)

;; Exercise 3.8
;; Define a simple procedure f such that evaluating (+ (f 0) (f 1)) will return 0 if the arguments to + are
;; evaluated from left to right but will return 1 if the arguments are evaluated from right to left.
;; Note: scheme evaluates left to right 

(define f
  (let ((c 1))
    (lambda (n) (set! c (* c n)) c)))

;; Why f works:
;; f is defined as a variable, not as a function, so it can have a "state" c.
;; Suppose we are evaluating left to right. (f 0) is called first, and so
;; c remains as 0, as c=1 and n=0, and c is set! to (* 0 0). Then, (f 1)
;; results in c=0, n=1, and c is set! to (* 0 1).

;; In reverse order, when (f 1) is called first, the value of c is set! to 1,
;; and then (f 0) sets the value of c to 0.

;; I tried doing this by using make-accumulator and make-monitored, but
;; the above solution which is due to bendersky is much better
;; than my earlier attempts.

;; Potentially confusing consequence, but is correct behaviour:
;> (+ (f 1) (f 0))
;1
;> (+ (f 0) (f 1))
;0
;> (+ (f 1) (f 0))
;0