#lang racket
(require scheme/mpair)
(require compatibility/mlist)
; Section 3.3.2: Queues

(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item) (set-mcar! queue item))
(define (set-rear-ptr! queue item) (set-mcdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (mcons '() '()))
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

(displayln "Exercise 3.21")
(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(insert-queue! q1 'c)
(insert-queue! q1 'zz)
;(delete-queue! q1)
;(delete-queue! q1)
(displayln "This is happening because the reference we're holding to the queue is 
the cons cell which points at the head and the tail of the queue, so both are 
printed and it gets extra confusing if the queue is short. Just print the car
of the queue to fix it.")
(define (print-queue queue)
  (displayln (mcar queue)))



