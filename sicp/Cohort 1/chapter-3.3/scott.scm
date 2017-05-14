;;;SECTION 3.3.2

(define (front-ptr queue)
  (car queue))
(define (rear-ptr queue)
  (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))
(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (make-queue)
  (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond
      ((empty-queue? queue)
        (set-front-ptr! queue new-pair)
        (set-rear-ptr! queue new-pair)
        queue)
      (else
        (set-cdr! (rear-ptr queue) new-pair)
        (set-rear-ptr! queue new-pair)
        queue))))
(define (delete-queue! queue)
  (cond
    ((empty-queue? queue) (error "DELETE! called with an empty queue" queue))
    (else
      (set-front-ptr! queue (cdr (front-ptr queue)))
      queue)))
;; section 3.3.4

;  (define a (make-wire))

;: (define b (make-wire))

;: (define c (make-wire))

;: (define d (make-wire))

;: (define e (make-wire))

;: (define s (make-wire))

;: 

;: (or-gate a b d)

;: (and-gate a b c)

;: (inverter c e)

;: (and-gate d e s)

;; exercise 3.28 or-gate

(define (logical-or x y)
  (if (or (= x 1) (= y 1))
    1
    0))
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay (lambda ()
        (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
;;; exercise 3.29 composite or gate

(define (or-gate-2 a b output)
  (let ((c (make-wire))
  (d (make-wire))
  (e (make-wire)))
    (inverter a c)
    (inverter b d)
    (and-gate c d e)
    (inverter e output)
    'ok))
;;NB. To use half-adder, need or-gate from exercise 3.28

(define (half-adder a b s c)
  (let ((d (make-wire))
  (e (make-wire)))
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
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay (lambda ()
        (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)
(define (logical-not s)
  (cond
    ((= s 0) 1)
    ((= s 1) 0)
    (else (error "Invalid signal" s))))
;;; from ch3support.scm

(define (logical-and x y)
  (if (and (= x 1) (= y 1))
    1
    0))
;; *following uses logical-and -- see ch3support.scm

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay (lambda ()
        (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)
(define (make-wire)
  (let ((signal-value 0)
  (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
        (begin
          (set! signal-value new-value)
          (call-each action-procedures))
        'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond
        ((eq? m 'get-signal) signal-value)
        ((eq? m 'set-signal!) set-my-signal!)
        ((eq? m 'add-action!) accept-action-procedure!)
        (else (error "Unknown operation -- WIRE" m))))
    dispatch))
(define (call-each procedures)
  (if (null? procedures)
    'done
    (begin ((car procedures)) (call-each (cdr procedures)))))
(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))
(define (after-delay delay action)
  (add-to-agenda!
    (+ delay (current-time the-agenda))
    action
    the-agenda))
(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))
(define (probe name wire)
  (add-action! wire (lambda ()
    (newline)
    (display name)
    (display " ")
    (display (current-time the-agenda))
    (display "  New-value = ")
    (display (get-signal wire)))))
;;;Implementing agenda

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s)
  (car s))
(define (segment-queue s)
  (cdr s))
(define (make-agenda)
  (list 0))
(define (current-time agenda)
  (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda)
  (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or
      (null? segments)
      (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
      (insert-queue! (segment-queue (car segments)) action)
      (let ((rest (cdr segments)))
        (if (belongs-before? rest)
          (set-cdr!
            segments
            (cons (make-new-time-segment time action) (cdr segments)))
          (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
      (set-segments!
        agenda
        (cons (make-new-time-segment time action) segments))
      (add-to-segments! segments))))
(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))
(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty -- FIRST-AGENDA-ITEM")
    (let ((first-seg (first-segment agenda)))
      (set-current-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))
;;; sample simulation

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)
;: 

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
;: 

(probe 'sum sum)
(probe 'carry carry)
;: 

(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)
;: 

(set-signal! input-2 1)
(propagate)
;;; Playing with the or gates

(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(probe 'a a)
(probe 'b b)
(probe 'c c)
(or-gate a b c)
(set-signal! a 0)
(set-signal! b 1)
(propagate)
(define d (make-wire))
(define e (make-wire))
(define f (make-wire))
(probe 'd d)
(probe 'e e)
(probe 'f f)
(or-gate-2 d e f)
(set-signal! d 0)
(set-signal! e 0)
(propagate)
;;;

;;;

;;;

;;;

;;;
;;; Section 3.3.5 Constraint Propagation

;;;

(define true #t)
(define false #f)
(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))
(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond
      ((or
        (and (has-value? m1) (= (get-value m1) 0))
        (and (has-value? m2) (= (get-value m2) 0)))
       (set-value! product 0 me))
      ((and (has-value? m1) (has-value? m2)) (set-value!
        product
        (* (get-value m1) (get-value m2))
        me))
      ((and (has-value? product) (has-value? m1)) (set-value!
        m2
        (/ (get-value product) (get-value m1))
        me))
      ((and (has-value? product) (has-value? m2)) (set-value!
        m1
        (/ (get-value product) (get-value m2))
        me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)
(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)
(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)
(define (make-connector)
  (let ((value false)
  (informant false)
  (constraints '()))
    (define (set-my-value newval setter)
      (cond
        ((not (has-value? me))
          (set! value newval)
          (set! informant setter)
          (for-each-except setter inform-about-value constraints))
        ((not (= value newval)) (error "Contradiction" (list value newval)))
        (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
        (begin
          (set! informant false)
          (for-each-except retractor inform-about-no-value constraints))
        'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
        (set! constraints (cons new-constraint constraints)))
      (if (has-value? me)
        (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond
        ((eq? request 'has-value?) (if informant
          true
          false))
        ((eq? request 'value) value)
        ((eq? request 'set-value!) set-my-value)
        ((eq? request 'forget) forget-my-value)
        ((eq? request 'connect) connect)
        (else (error "Unknown operation -- CONNECTOR" request))))
    me))
(define (for-each-except exception procedure list)
  (define (loop items)
    (cond
      ((null? items) 'done)
      ((eq? (car items) exception) (loop (cdr items)))
      (else (procedure (car items)) (loop (cdr items)))))
  (loop list))
(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))
(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
  (v (make-connector))
  (w (make-connector))
  (x (make-connector))
  (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond
      ((and (has-value? a1) (has-value? a2))
        (set-value! sum (+ (get-value a1) (get-value a2)) me))
      ((and (has-value? a1) (has-value? sum))
        (set-value! a2 (- (get-value sum) (get-value a1)) me))
      ((and (has-value? a2) (has-value? sum))
        (set-value! a1 (- (get-value sum) (get-value a2)) me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)
(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)
(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)
;(set-value! C 25 'user)

;(set-value! F 212 'user)

;(forget-value! C 'user)

;(set-value! F 212 'user)

;; exercise 3.33 c= average(a, b)

;; 2c = a + b

(define (averager a b c)
  (let ((two (make-connector))
  (total (make-connector)))
    (multiplier two c total)
    (adder a b total)
    (constant 2 two)
    'ok))
(define A1 (make-connector))
(define A2 (make-connector))
(define A3 (make-connector))
(probe "A1" A1)
(probe "A2" A2)
(probe "A3" A3)
(averager A1 A2 A3)
(set-value! A1 3 'user)
(set-value! A2 5 'user)
(get-value A3)
;; exercise 3.34

;; this approach isnt a relation, it only works in one direction.  if you specify the 

;; squared value it will not fill inthe squareroot, since multiplier needs two inputs 

;; before it will propogate the constraint.

(define (squarer-wrong a b)
  (multiplier a a b))
(define va (make-connector))
(define vb (make-connector))
(squarer-wrong va vb)
(probe "va" va)
(probe "vb" vb)
(set-value! vb 25 'user)
;; exercise 3.35 squareroot

(define (squarer a b)
  (define (square x)
    (* x x))
  (define (process-new-value)
    (cond
      ((has-value? b)
        (if (< (get-value b) 0)
          (error "square less than 0: SQUARER" (get-value b))
          (set-value! a (sqrt (get-value b)) me)))
      ((has-value? a) (set-value! b (square (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond
      ((eq? request 'I-have-a-value) (process-new-value))
      ((eq? request 'I-lost-my-value) (process-forget-value))
      (else (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)
(define vc (make-connector))
(define vd (make-connector))
(squarer vc vd)
(probe "vc" vc)
(probe "vd" vd)
(set-value! vd 25 'user)
(forget-value! vd 'user)
(set-value! vc 7 'user)
;; exercise 3.36 needs a diagram

;; exercise 3.37, expression syntax

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5)) x) (cv 32)))
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))
(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))
(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))
(define (cv v)
  (let ((z (make-connector)))
    (constant v z)
    z))
(celsius-fahrenheit-converter C F)
(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)
(set-value! C 25 'user)
;(set-value! F 77 'user)

(forget-value! C 'user)
(set-value! F 212 'user)
;; footnote 33 is very interesting, they discuss how the imperative style is cumbersome, 

;; but it is straightforward to move to expression oriented and 

;; not the other way.  they also bring up vectorization, if you can 

;; return procedures than you can avoid temporary variables.

;; this is pretty important, compilers do a good job of this when they are single variables but much less so for vectors.

;; also vectorizing often brings up data flow, which often has to be done explicitly by the programmer, even though the vectorized expression is often compleely data parallel.

;; this should be interesting in the coming chapter on concurrency.

;;;

;;;

;;;

;;;

;;;

;;;
