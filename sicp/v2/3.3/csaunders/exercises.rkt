#lang racket
(require scheme/mpair)

(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item)
  (set-mcar! queue item))
(define (set-rear-ptr! queue item)
  (set-mcdr! queue item))

(define (empty-queue? queue)
  (empty? (front-ptr queue)))

(define (make-queue) (mcons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with empty queue" queue)
      (mcar (front-ptr queue))))

(define (insert-queue! queue item)
  (let [(new-pair (mcons item '()))]
    (cond [(empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue]
          [else
           (set-mcdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue])))

(define (delete-queue! queue)
  (cond [(empty-queue? queue)
         (error "DELETE! called with an empty queue" queue)]
        [else
         (set-front-ptr! queue (mcdr (front-ptr queue)))
         queue]))

(displayln "exercise 3.21")
;; Eva Lu is explaining an implementation detail
;; of how the queue is implemented. Because we only
;; consider what is valid within the queue from the
;; perspective of the front-pointer, we can end up
;; with what appears to be the duplicate 'b' in the
;; list. Though in reality it's just the tail pointer
;; still pointing to the list item.
(define (print-queue queue)
  (display (front-ptr queue)))

(displayln "exercise 3.22")
(define (make-lambda-queue)
  (let [(front-ptr '())
        (rear-ptr '())]
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (queue-empty?)
      (empty? front-ptr))
    (define (queue-front)
      (if (queue-empty?)
          (error "FRONT called on empty queue" front-ptr)
          (mcar front-ptr)))
    (define (queue-insert! item)
      (let [(new-item (mcons item '()))]
        (cond [(queue-empty?)
               (set-front-ptr! new-item)
               (set-rear-ptr! new-item)
               front-ptr]
              [else (set-mcdr! rear-ptr new-item)
                    (set-rear-ptr! new-item)
                    front-ptr])))
    (define (queue-delete!)
      (if (queue-empty?)
          (error "DELETE! called on empty queue" front-ptr)
          (begin
            (set-front-ptr! (mcdr front-ptr))
            front-ptr)))
    (define (queue-print)
      (display front-ptr))
    (define (dispatch msg)
      (cond [(eq? msg 'empty?) queue-empty?]
            [(eq? msg 'front) queue-front]
            [(eq? msg 'insert!) queue-insert!]
            [(eq? msg 'delete!) queue-delete!]
            [(eq? msg 'print) queue-print]
            [else (error "DISPATCH unsupported message" msg)]))
    dispatch))

(define lq (make-lambda-queue))

(displayln "exercise 3.23")
(define (make-deque)
  (let [(front-ptr '())
        (rear-ptr '())]
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (deque-empty?)
      (empty? front-ptr))
    (define (initialize! item)
      (set-front-ptr! item)
      (set-rear-ptr! item)
      front-ptr)
    (define (deque-front)
      (if (deque-empty?)
          (error "FRONT called on empty deque" front-ptr)
          (mcar front-ptr)))
    (define (deque-rear)
      (if (deque-empty?)
          (error "REAR called on empty deque" front-ptr)
          (mcar rear-ptr)))
    (define (deque-insert-front! item)
      (let [(new-item (mcons item '()))]
        (cond [(deque-empty?) (initialize! item)]
              [else (set-mcdr! new-item front-ptr)
                    (set-front-ptr! new-item)
                    front-ptr])))
    (define (deque-insert-rear! item)
      (let [(new-item (mcons item '()))]
        (cond [(deque-empty?) (initialize! new-item)]
              [else (set-mcdr! rear-ptr new-item)
                    (set-rear-ptr! new-item)
                    front-ptr])))
    (define (deque-delete-front!)
      (if (deque-empty?)
          (error "DELETE-FRONT! called on empty deque" front-ptr)
          (begin
            (set-front-ptr! (mcdr front-ptr))
            front-ptr)))
    (define (deque-delete-rear!)
      (define (adjust-rear-ptr! pos)
        (cond [(eq? (mcdr pos) rear-ptr)
               (set-mcdr! pos '())
               (set-rear-ptr! pos)
               rear-ptr]
              [else (adjust-rear-ptr! (mcdr pos))]))
      (if (deque-empty?)
          (error "DELETE-REAR! called on empty deque" front-ptr)
          (adjust-rear-ptr! front-ptr)))
    (define (deque-print)
      (display front-ptr))
    (define (dispatch msg)
      (cond [(eq? msg 'empty?) deque-empty?]
            [(eq? msg 'front) deque-front]
            [(eq? msg 'rear) deque-rear]
            [(eq? msg 'insert-front!) deque-insert-front!]
            [(eq? msg 'insert-rear!) deque-insert-rear!]
            [(eq? msg 'delete-front!) deque-delete-front!]
            [(eq? msg 'delete-rear!) deque-delete-rear!]
            [(eq? msg 'print) deque-print]
            [else (error "DISPATCH unsupported message" msg)]))
    dispatch))

(define deq (make-deque))
((deq 'insert-rear!) 'a)
((deq 'insert-rear!) 'b)
((deq 'insert-rear!) 'c)
((deq 'insert-front!) 'd)

(displayln "exercise 3.24")
(define (make-2d-table same-key?)
  (let ((local-table (mlist '*table*)))
    (define (assoc key records)
      (define (mcaar mlst) (mcar (mcar mlst)))
      (cond [(empty? records) #f]
            [(same-key? key (mcaar records)) (mcar records)]
            [else (assoc key (mcdr records))]))    
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                            (mcons (mcons key-2 value)
                                  (mcdr subtable)))))
            (set-mcdr! local-table
                      (mcons (mlist key-1
                                    (mcons key-2 value))
                             (mcdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define my-table (make-2d-table eq?))
((my-table 'insert-proc!) 1 1 "hello")
((my-table 'insert-proc!) 1 2 "this is nice")
((my-table 'lookup-proc) 1 2)
((my-table 'lookup-proc) 1 1)

(displayln "exercise 3.25")
(displayln "This one is really hard. I'll come back to it")
;;(define deep-t (make-deep-table))
;;((deep-t 'insert!) '(1 1 1 1))
;;((deep-t 'insert!) '(1 1 1 2))
;;((deep-t 'lookup) '(1 1 1))

(displayln "exercise 3.26")

(displayln "exercise 3.27")
;; I am not going to draw an environment diagram
;;
;; The reason why the memoized function is able to calculate
;; the n-th fibonacci number in a number of steps proportional
;; to n is because a lot of the duplicated method calls have
;; already been computed. The result is fetching the value from
;; the table instead of performing the computation.
;;
;; It doesn't work when wrapping fib because each "child" call hasn't
;; been memoized. So to first fill the table it will require doing a full
;; unmemoized fibonnaci function call, whereas making the calls in the memoized
;; version ensures that a function is never called more than once.