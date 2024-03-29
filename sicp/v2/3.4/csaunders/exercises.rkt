#lang racket

(displayln "exercise 3.38")
;; Maybe Later

(displayln "exercise 3.39")
#|
| The values of x that are possible are:
| - 101
| - 121
| - 100
|   - This is because the first item gets
|     reserialized so it could read the value
|     set it, but the other function could be
|     executing in between. Data will get lost
|#

(displayln "exercise 3.40")
;; a. Maybe later
;; But basically there can be state change in
;; between the various reads of x

#|
| b. 
| 1000000: x = 10^2; x = 100^3
| 1000000: x = 10^3; x = 1000^2
|#

(displayln "exercise 3.41")
;; The improvement Ben makes to the account is only
;; for reads, which is only valid at the point that
;; it is read. After we have made that read something
;; could have adjusted it. So the gains we get from
;; having the read serialized are basically neutralized

(displayln "exercise 3.42")
;; They are functionally equivalent, Bens just results
;; in fewer allocations.

(displayln "exercise 3.43")
;; See attached image - ex-3-43.png

(displayln "exercise 3.45")
#|
| The serializer will already be locked by the serialized-exchange method during
| the setup. As a result, neither of the methods will be able to run because
| each will be locked up. This would result in blocking indefinitely
|#

(displayln "exercise 3.46")
#|
| Test-and-set! cannot allow methods to be interleaved, if they are it would result
| in allowing both methods to think they obtained the lock resulting in concurrent
| modification (leaving the mutex completely useless).
| Legend:
|  - F: Mux is available
|  - T: Mux has been acquired
|
| I am not entirely sure if this makes sense or not...
|
|     procA              mux            procB
|       |                 |               |
|   mux-free? -> T        F           mux-free? -> T
|       |                 |               |
|    car mux  -> F        |               |
|       |                 |            car mux  -> F
|  set-car! mux T ----->  T               |
|       |                 T <-------  set-car! mux T
|    clear! mux   ----->  F               |
|       |                 F <-------   clear! mux
|#

(displayln "exercise 3.47")
(define (make-mutex)
  (let [(cell 'free)]
    (define (clear!) (set! cell 'free))
    (define (test-and-set!)
      (if (eq? cell 'locked)
          true
          (begin (set! cell 'locked)
                 false)))
    
    (define (dispatch msg)
      (cond [(eq? msg 'acquire)
             (when (test-and-set!) (dispatch 'acquire))]
            [(eq? msg 'release) (clear!)]
            [(eq? msg 'locked?) (eq? cell 'locked)]))
    dispatch))


(define (make-semaphore n)
  (let [(mux (make-mutex))
        (available-resources n)]
    (define (synchronized p)
      (mux 'acquire)
      (let [(res (p))]
        (mux 'release)
        res))

    (define (decr)
      (if (> available-resources 0)
          (begin (set! available-resources (- available-resources 1))
                 true)
          false))

    (define (incr)
      (when (< available-resources n)
        (set! available-resources (+ available-resources 1)))
      true)

    (define (acquire-resource)
      (let [(resource-acquired (synchronized decr))]
        (if resource-acquired
            #t
            ;; Wait-until-available
            (acquire-resource))))

    (define (release-resource) (synchronized incr))

    (define (dispatch msg)
      (cond [(eq? msg 'acquire) (acquire-resource)]
            [(eq? msg 'release) (release-resource)]
            [(eq? msg 'avail) available-resources]
            [else (error "SEMAPHORE -- unsupported message" msg)]))
    dispatch))

(define s (make-semaphore 3))
(displayln (s 'avail))
(s 'acquire)
(s 'acquire)
(displayln (s 'avail))
(s 'acquire)
(displayln (s 'avail))
;;(s 'acquire) <-- will block forever

(displayln "exercise 3.48")
(define (make-serializer)
  (let [(mutex (make-mutex))]
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let [(val (apply p args))]
          (mutex 'release)
          val))
      serialized-p)))

(define make-account
  ((lambda ()
     (let [(account-id 0)]
       (define (make-account balance)
         (define id-for-account account-id)
         (set! account-id (+ account-id 1))

         (define (withdraw amount)
           (if (>= balance amount)
               (begin (set! balance (- balance amount))
                      balance)
               "Insufficient Funds"))

         (define (deposit amount)
           (set! balance (+ balance amount)))

         (let [(protected (make-serializer))]
           (define (dispatch msg)
             (cond [(eq? msg 'withdraw) withdraw]
                   [(eq? msg 'deposit) deposit]
                   [(eq? msg 'balance) balance]
                   [(eq? msg 'serializer) protected]
                   [(eq? msg 'account-id) id-for-account]
                   [else (error "Unknown request -- MAKE-ACCOUNT" msg)]))
           dispatch))
       make-account))))

(define (exchange account1 account2)
  (let [(diff (- (account1 'balance)
                 (account2 'balance)))]
    ((account1 'withdraw) diff)
    ((account2 'deposit) diff)
    )
  )

(define (serialized-exchange account-1 account-2)
  (define (account-1-has-lower-id?)
    (< (- (account-1 'account-id) (account-2 'account-id)) 0))
  (let [(lower-serializer ((if (account-1-has-lower-id?) account-1 account-2) 'serializer))
        (higher-serializer ((if (not (account-1-has-lower-id?)) account-1 account-2) 'serializer))]
    ((higher-serializer (lower-serializer exchange))
     account-1 account-2)))

(define (instrument account)
  (lambda (msg)
    (display msg)
    (display ": ")
    (displayln (account 'account-id))
    (account msg)))

(define joe (instrument (make-account 100)))
(define bob (instrument (make-account 200)))
(serialized-exchange bob joe)

(displayln "exercise 3.49")
;; There can be the case where a we don't know what other resources we will require until we've acquired
;; the lock. This could be something like a database, or an anonymous function that also locks on another
;; resource which never becomes available.