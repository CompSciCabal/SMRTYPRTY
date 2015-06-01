#lang racket
; Exercise 3.4
; An account factory. Returns a function which accepts `password` and
; `action` args, which returns a function that accepts an amount. Eg:
; (define acc (make-account 100 'secret-password))
; ((acc 'secret-password 'withdraw) 20)
; 80
(define (make-account balance password)
  (let ((call-counter 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch supplied-password m)
      (if (not (eq? supplied-password password)) 
          (begin 
            (set! call-counter (+ 1 call-counter))
            (if (= call-counter 3) call-cops
                (error "Incorrect password")))                  
          (begin (set! call-counter 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       (else (error "Unknown request -- MAKE-ACCOUNT"
                                    m))))
          )
      
      )dispatch))

(define (call-cops arg) "Busted! Sucker.")
(define peter-acc (make-account 100 'secret-password))

; Exercise 3.7
; Consider the bank account objects created by make-account, with the 
; password modification described in exercise 3.3. Suppose that our banking 
; system requires the ability to make joint accounts. Define a procedure 
; make-joint that accomplishes this. Make-joint should take three arguments. 
; The first is a password-protected account. The second argument must match the 
; password with which the account was defined in order for the make-joint 
; operation to proceed. The third argument is a new password. Make-joint is to 
; create an additional access to the original account using the new password. 
; For example, if peter-acc is a bank account with password open-sesame, then
;
; (define paul-acc
;  (make-joint peter-acc 'open-sesame 'rosebud))

; will allow one to make transactions on peter-acc using the name paul-acc and the 
; password rosebud. You may wish to modify your solution to exercise 3.3 to 
; accommodate this new feature.

(define (make-joint account oldpassword newpassword)
 (lambda (password m) 
   (if (eq? password newpassword)
       (account oldpassword m)
       (lambda (m) 'gtfo))))

(define paul-acc
  (make-joint peter-acc 'secret-password 'rosebud))