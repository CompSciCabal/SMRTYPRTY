#lang racket
(require scheme/mpair)
(require compatibility/mlist)

(displayln "Exercise 3.22: build a queue using closures and dispatch.")

#| 
The trick here is that there is one less level of indirection than in the
exercise 3.21 version. In 3.21 the queue is represented by a cons of pointers
to the head and tail, but that's not necessary here, because pointers to head
and tail are stored in the closure. Everything actually because *simpler*. 
|#

(define (make-queue)
  (let ((front-ptr (mlist))
        (rear-ptr (mlist)))
    (define (set-front-ptr! new-ptr) 
      (set! front-ptr new-ptr))
    (define (set-rear-ptr! new-ptr)
      (set! rear-ptr new-ptr))      
    (define (empty-queue?) (null? front-ptr))
    (define (insert-queue! item)
      (let ((new-pair (mcons item (mlist))))
        (cond ((empty-queue?)                
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               front-ptr)
              (else       
               (set-mcdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)               
               front-ptr))))        
    (define (dispatch m)
      (cond ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'empty?) empty-queue?)
            ((eq? m 'print) (displayln front-ptr))))
    dispatch))

#|
if the queue has one item already, set the cdr of the rear pointer to point at new-pair 
and then advance the rear-point to new-pair.
           (set-mcdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
|#

(define q1 (make-queue))
((q1 'insert-queue!) 'b)
((q1 'insert-queue!) 'c)
((q1 'insert-queue!) 'd)
(q1 'print)
