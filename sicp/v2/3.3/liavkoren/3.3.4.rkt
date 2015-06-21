#lang racket
(require scheme/mpair)
(require compatibility/mlist)

; Queue
(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item) (set-mcar! queue item))
(define (set-rear-ptr! queue item) (set-mcdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (mcons (mlist) (mlist)))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (mcar (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-mcdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue)))) 
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (mcdr (front-ptr queue)))
         queue))) 

; Electronics
(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))
(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((mcar procedures))
        (call-each (mcdr procedures)))))

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (mcons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))
(define (make-agenda) (mlist 0))
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)
(define (current-time agenda) (mcar agenda))
(define (make-time-segment time queue)
  (mcons time queue))
(define (segment-time s) (mcar s))
(define (segment-queue s) (mcdr s))
(define (segments agenda) (mcdr agenda))
(define (set-segments! agenda segments)
  (set-mcdr! agenda segments))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (mcar segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (mcar segments)) time)
        (insert-queue! (segment-queue (mcar segments))
                       action)
        (let ((rest (mcdr segments)))
          (if (belongs-before? rest)
              (set-mcdr!
               segments
               (mcons (make-new-time-segment time action)
                     (mcdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (mcons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((or (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal"))))

(define (logical-or s1 s2)
  (cond ((or (= s1 1) (= s2 0)) 1)
        ((or (= s1 0) (= s2 1)) 1)
        (else (error "Invalid signal"))))


(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

#|
Exercise 3.28
-------------
Define an or-gate as a primitive function box. Your or-gate constructor should be similar to and-gate.
|#

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value 
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda () 
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

#|
Excercise 3.30
--------------

ARRRRRRRGHGGGHH OMG SO MUCH CODE PASTEING

https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-22.html
Make a damn ripple adder.
|#

(define (ripple-carry-adder a-wires b-wires sum-wires carry-wire)
  ; given lists of a-wires, b-wires, sum-wires, plus a carry wire, makes a ripple adder.
  (let ((ripple-adder (mlist))
        (last-wire (make-wire))) ; last-wire c_n in the diagram
    (define (build-adder-stage carry-in) ; given an internal carry-wire, adds another full-adder to the ripple-adder.
      (cond ((null? (mcdr sum-wires)) ; this is the last full-adder in the ripple
             (mcons ripple-adder (full-adder (mcar a-wires) (mcar b-wires) carry-in (mcar sum-wires) (make-wire))))
            (else (begin (let ((carry-out (make-wire)))
                           (set-mcar! ripple-adder (full-adder (mcar a-wires) (mcar b-wires) carry-in (mcar sum-wires) carry-out)) 
                           ()))
                    )
             ))) 
      )
    )
; Argh super frustrating, how do you even test this as you go?




(define a (make-wire))
(define b (make-wire))
(define c-in (make-wire))
(define c-out (make-wire))
(define s (make-wire))
(full-adder a b c-in s c-out)
