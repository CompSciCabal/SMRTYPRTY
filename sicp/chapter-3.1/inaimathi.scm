;;;;;;;;;; Exercise 3.1
(define (make-accumulator initial-value)
  (lambda (delta)
    (set! initial-value (+ initial-value delta))
    initial-value))

;;;;;;;;;; Exercise 3.2
(define (make-monitored unary-fn)
  (let ((ct 0))
    (lambda (arg)
      (case arg
	('how-many-calls? ct)
	('reset-count (set! ct 0))
	(else (set! ct (+ 1 ct))
	      (unary-fn arg))))))

;;;;;;;;;; Exercise 3.3
;;; Kids, don't use passwords. Stay in school and study public/symmetric key encryption.
(define (make-account balance password)
  (define (withdraw amount) ...)
  (define (deposit amount) ...)
  (lambda (pass msg)
    (if (eq? pass password)
	(case msg
	  ('withdraw withdraw)
	  ('deposit deposit)
	  (else (error "Unknown request: MAKE-ACCOUNT" m)))
	"Incorrect password")))

;;;;;;;;;; Exercise 3.4
(define (make-account balance password)
  (define (withdraw amount) ...)
  (define (deposit amount) ...)
  (let ((fails 0))
    (lambda (pass msg)
      (cond ((eq? pass password)
	     (set! fails 0)
	     (case msg
	       ('withdraw withdraw)
	       ('deposit deposit)
	       (else (error "Unknown request: MAKE-ACCOUNT" m))))
	    ((>= fails 7)
	     "The police have been notified")
	    (else
	     (set! fails (+ 1 fails))
	     "Incorrect password")))))

;;;;;;;;;; Exercise 3.7
(define (make-joint account main-pass joint-pass)
  (if (equal? "Incorrect password" ((account main-pass 'deposit) 0))
      "Incorrect password"
      (lambda (pass msg)
	(if (eq? pass joint-pass)
	    (account main-pass msg)
	    "Incorrect password"))))

;;;;;;;;;; Exercise 3.8
(define f
  (let ((n #f))
    (lambda (num)
      (if n
	  (set! n (max 0 (- n 1)))
	  (set! n num))
      n)))

(define f
  (let ((prev 100))
    (lambda (num)
      (when (> prev num) (set! prev num))
      prev)))
