;3.1
(define (make-accumulator n)
  (lambda (amount)
    (begin
    (set! n (+ n amount))
    n)))

;3.2
(define (make-monitored f)
  (let ((n 0))
    (lambda (sym)
      (cond ((eq? sym 'how-many-calls?) n )
	          ((eq? sym 'reset-count) (set! n 0))
	          (else (begin 
		        (set! n (+ n 1))
		        (f sym)))))))

;3.3, 3.4
(define (make-protected-account balance password)
  (define wrong-password 0)
  (define (call-the-cops amount)
    "cops are on their way!")
  (define (withdraw amount)
    (if (>= balance amount)
	(begin (set! balance (- balance amount))
	       balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m p)
    (cond ((and (eq? p password) (eq? m 'withdraw)) (begin (set! wrong-password 0) withdraw))
	  ((and (eq? p password) (eq? m 'deposit)) (begin (set! wrong-password 0) deposit))
	  ((not (eq? p password)) (begin 
				    (set! wrong-password (+ wrong-password 1))
				    (if (< wrong-password 7) (error "Incorrect password!") call-the-cops )))
	  (else (error "Unknown request -- MAKE-ACCOUNT"
		       m p))))
  dispatch)
