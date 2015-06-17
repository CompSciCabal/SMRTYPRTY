#lang racket
(require scheme/mpair)
(require compatibility/mlist)

(displayln "Exercise 3.23: build a deque.")

#|
Operations on deques are the constructor
- make-deque, 
- the predicate empty-deque?, 
selectors 
- front-deque and 
- rear-deque, 
and mutators 
- front-insert-deque!, 
- rear-insert-deque!, 
- front-delete-deque!, and 
- rear-delete-deque!
|#

(define (make-deque)
  (let ((front-ptr (mlist))
        (rear-ptr (mlist)))
    (define (set-front-ptr! new-ptr) 
      (set! front-ptr new-ptr))
    (define (set-rear-ptr! new-ptr)
      (set! rear-ptr new-ptr))
    (define (empty-deque?) (null? front-ptr))
    (define (front-insert-deque! item)
      (let ((new-pair (mcons item (mcons (mlist) front-ptr))))
        (cond ((empty-deque?)                
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else
               (set-mcar! (mcdr front-ptr) new-pair) 
               ;(set-front-ptr! (mcons new-pair front-ptr))
               (set-front-ptr! new-pair)))))
    (define (rear-insert-deque! item)
      (let ((new-pair (mcons item (mcons rear-ptr (mlist)))))
        (cond ((empty-deque?)                
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else       
               (set-mcdr! (mcdr rear-ptr) new-pair)
               (set-rear-ptr! new-pair)))))   
    (define (front-delete!)
      (cond ((empty-deque?) (error "Delete called called on empty queue"))
                                 (else 
                                  (set-front-ptr! (mcdr (mcdr front-ptr)))
                                  (set-mcar! (mcdr front-ptr) '()))))    
    (define (rear-delete!)
      (cond ((empty-deque?) (error "Delete called called on empty queue"))
                                 (else 
                                  (set-rear-ptr! (mcar (mcdr rear-ptr)))
                                  (set-mcdr! (mcdr rear-ptr) '()))))
    (define (print)
      (displayln front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'empty?) (empty-deque?))
            ((eq? m 'front-insert) front-insert-deque!)
            ((eq? m 'rear-insert) rear-insert-deque!)
            ((eq? m 'print) (print))
            ((eq? m 'front-delete) (front-delete!))
            ((eq? m 'rear-delete) (rear-delete!))))
    dispatch))

(define q2 (make-deque))
(q2 'empty?)
((q2 'front-insert) 'a)
(display "a: ")
(q2 'print)
((q2 'front-insert) 'b)
(display "b: ")
(q2 'print)

((q2 'rear-insert) 'z)
(display "z: ")
(q2 'print)
((q2 'rear-insert) 'y)
(display "y: ")
(q2 'print)
(q2 'front-delete)
(displayln "front-delete")
(q2 'print)
(q2 'rear-delete)
(displayln "rear-delete")
(q2 'print)

; (q2 'print)