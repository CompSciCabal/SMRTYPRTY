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

#|
| Wire "Built-ins" from SICP
|#

(define (make-wire)
  (let [(signal-value 0) (action-procedures '())]
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond [(eq? m 'get-signal) signal-value]
            [(eq? m 'set-signal!) set-my-signal!]
            [(eq? m 'add-action!) accept-action-procedure!]
            [else (error "Unknown operation -- WIRE" m)]))
    dispatch))

(define (call-each procedures)
  (if (empty? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire value)
  ((wire 'set-signal!) value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define inverter-delay 0)
(define and-gate-delay 0)
(define or-gate-delay 0)

(define the-agenda '())
(define (empty-agenda? agenda) #t)
(define (first-agenda-item agenda) '())
(define (remove-first-agenda-item! agenda) '())
(define (current-time agenda) 0)
(define (add-to-agenda! duration action agenda) '())

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let [(first-item (first-agenda-item the-agenda))]
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

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

(define (logical-and a b)
  (if (= a b 1) 1 0))

(define (logical-or a b)
  (cond [(= 1 a) 1]
        [(= 1 b) 1]
        [else 0]))
        

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

(displayln "exercise 3.28")

(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ([new-value (logical-or (get-signal o1) (get-signal o2))])
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure)
  'ok)

(displayln "exercise 3.29")

#|
|
| NAND
| IN1 -|
| IN2 -| AND |- AndOut -| NOT |- OUT
|-------------------------------------
|  O1 -|    |
|  O1 -|NAND|-- NandO1 -|    |
|                       |NAND|-- Output
|  O2 -|    |-- NandO2 -|    |
|  O2 -|NAND|
|-------------------------------------
|
| Delay time is that of 3 NAND gates. The NAND gates for O1 and O2
| are evaluated in parallel so they will finish at the same time.
| This means that we would only "see" the delay for 2 NAND gates:
| 2*AND + 2*NOT
|#
(define (compound-or-gate o1 o2 output)
  (define (nand-gate in1 in2 out)
    (let [(andout (make-wire))]
      (and-gate in1 in2 andout)
      (inverter andout out)))
    
  (let ([nandO1 (make-wire)]
        [nandO2 (make-wire)])
    (nand-gate o1 o1 nandO1)
    (nand-gate o2 o2 nandO2)
    (nand-gate nandO1 nandO2 output)))

(displayln "exercise 3.30")
#|
| The delay for a series of full adders will be that of
| their parts. A full adder consists of: two half-adders
| along with an OR gate.
| The half adder is made up of two AND gates, an OR gate
| along with an INVERTER.
| So the propagation time for a single full adder would be:
| (4*AND-DELAY + 3*OR-DELAY + 2*INVERTER-DELAY)
| So a series consisting of N full adders would take:
| N * (4*AND-DELAY + 3*OR-DELAY + 2*INVERTER-DELAY)
|#

(displayln "exercise 3.31")
#|
| Some of the inputs might actually cause changes based on the current value of the wire.
| In a real system, if we were to wire it up there would be an immediate "trigger" that would
| cause the value on the wire to change. In our system that is not the case, so we need to manually
| trigger it when the item is added to the wire.
|#

(displayln "exercise 3.32")
#|
| The values that the output is going to be assigned to are already determined based on the state of the system
| after the value of the wire has been set. Due to the delay in the "hardware" we aren't going to see that change
| immediately, but a number of ticks afterwards. Subsequent changes to wire values are delayed as well, so while a value
| might change from 0 to 1 then back to 0, there is still going to be a point where we could've had both inputs equal to 1.
| If we don't evaluate them in order we are going to update our outputs to invalid data based on what we previously
| knew as the truth.
| 
| This solution helps explain it a lot better than I am:
|   https://github.com/qiao/sicp-solutions/blob/a2fe069ba6909710a0867bdb705b2e58b2a281af/chapter3/3.32.scm
|#
