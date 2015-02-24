#lang planet neil/sicp

;; -------------------------------------------------------------------
;; Mutable Lists, p.252
;; -------------------------------------------------------------------

;; Exrcise 3.16, p.259

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; Tests

(= 3 (count-pairs (cons 1 (cons 2 (cons 3 nil)))))

(define x (cons 1 nil))
(= 4 (count-pairs (cons (cons 2 x) x)))
(= 5 (count-pairs (cons (cons x x) x)))

(define y (cons x x))
(= 7 (count-pairs (cons y y)))

(define z (cons 3 4))
(define cycle (cons 1 (cons 2 z)))
(set-cdr! z cycle)
;(= ∞ (count-pairs cycle)) ; Infinite loop

;; Exrcise 3.17, p.259

(define (cp x)
  (let ((visited nil))
    (define (iter y)
      (if (memq y visited)
          0
          (begin (set! visited (cons y visited))
                 (if (not (pair? y))
                     0
                     (+ (iter (car y))
                        (iter (cdr y))
                        1)))))
    (iter x)))

;; Tests

(= 3 (cp (cons 1 (cons 2 (cons 3 nil)))))
(= 3 (cp (cons (cons 2 x) x)))
(= 3 (cp (cons (cons x x) x)))
(= 3 (cp (cons y y)))
(= 3 (cp cycle))

;; Exercise 3.18, p.260

(define (cycle? ls)
  (define (iter y visited)
    (cond ((memq y visited) #t)
          ((null? y) #f)
          (else (iter (cdr y) (cons y visited)))))
  (iter ls nil))

;; Tests

(not (cycle? '(1 2 3)))
(cycle? cycle)

;; Exercise 3.19, p.260

(define (floyd-cycle? ls)
  (define (iter tortoise hare)
    (cond ((eq? tortoise hare) #t)
          ((null? hare) #f)
          ((null? (cdr hare)) #f)
          (else (iter (cdr tortoise) (cddr hare)))))
  (cond ((null? ls) #f)
        ((null? (cdr ls)) #f)
        (else (iter (cdr ls) (cddr ls)))))

;; Tests

(not (floyd-cycle? nil))
(not (floyd-cycle? '(1)))
(not (floyd-cycle? '(1 2)))
(not (floyd-cycle? '(1 2 3)))
(floyd-cycle? cycle)

;; -------------------------------------------------------------------
;; Representing Queues, p.261
;; -------------------------------------------------------------------

(define front-ptr car)
(define rear-ptr cdr)
(define set-front-ptr! set-car!)
(define set-rear-ptr! set-cdr!)

(define (make-queue) (cons nil nil))
(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item nil)))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

;; Exercise 3.21, p.265

(define (print-queue queue)
  (display (front-ptr queue))
  (newline))

(define q1 (make-queue))
(print-queue (insert-queue! q1 'a))
(print-queue (insert-queue! q1 'b))
(print-queue (delete-queue! q1))
(print-queue (delete-queue! q1))

;; Exercise 3.22, p.266

(define (make-q)
  (let ((front-ptr nil)
        (rear-ptr nil))
    (define (empty-q?) (null? front-ptr))
    (define (insert-q item)
      (let ((q (cons item nil)))
        (cond ((empty-q?)
               (set! front-ptr q)
               (set! rear-ptr q)
               front-ptr)
              (else
               (set-cdr! rear-ptr q)
               (set! rear-ptr q)
               front-ptr))))
    (define (delete-q)
      (cond ((empty-q?)
             (error "DELETE! called with an empty queue" front-ptr))
            (else
             (set! front-ptr (cdr front-ptr))
             front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'insert-queue) insert-q)
            ((eq? m 'delete-queue) delete-q)))
    dispatch))

(define q2 (make-q))
((q2 'insert-queue) 'a)
((q2 'insert-queue) 'b)
((q2 'delete-queue))
((q2 'delete-queue))

;; Exercise 3.23, p.266

(define (cell item)
  (cons item (cons nil nil)))

(define (link! left-cell right-cell)
  (set-car! (cdr left-cell) right-cell)
  (set-cdr! (cdr right-cell) left-cell)
  left-cell)

;       ┌─┬─┐
; Deque │*│*┼────┐
;       └┼┴─┘    │
;        │       │
;       ┌┴┐     ┌┴┐
;     ┌─┤a│  ┌──┤b│ Cells
;     │ ├─┤  │  ├─┤
;     │ │*│  │  │*│
;     │ └┼┘  │  └┼┘
;     │ ┌┴┐  │  ┌┴┐
;     │ │*┼──┘  │ │
;     │ ├─┤     ├─┤
;     │ │ │    ┌┼*│
;     │ └─┘    │└─┘
;     └────────┘

(define (make-deque) (cons nil nil))

(define (empty-deque? d) (null? (car d)))

(define (front-deque d)
  (if (empty-deque? d)
      (error "FRONT called with an empty deque" d)
      (caar d)))

(define (rear-deque d)
  (if (empty-deque? d)
      (error "REAR called with an empty deque" d)
      (cadr d)))

(define (front-insert-deque! d item)
  (let ((n (cell item)))
    (cond ((empty-deque? d)
           (set-car! d n)
           (set-cdr! d n)
           d)
          (else
           (link! n (car d))
           (set-car! d n)
           d))))

(define (rear-insert-deque! d item)
  (let ((n (cell item)))
    (cond ((empty-deque? d)
           (set-car! d n)
           (set-cdr! d n)
           d)
          (else
           (link! (cdr d) n)
           (set-cdr! d n)
           d))))

(define (front-delete-deque! d)
  (cond ((empty-deque? d)
         (error "FRONT-DELETE! called with an empty deque" d))
        (else
         (set-car! d (cadar d))
         (set-cdr! (cdar d) nil)
         d)))

(define (rear-delete-deque! d)
  (cond ((empty-deque? d)
         (error "REAR-DELETE! called with an empty deque" d))
        (else
         (set-cdr! d (cdddr d))
         (set-car! (cddr d) nil)
         d)))

(define (to-list d)
  (define (iter acc cells)
    (if (null? cells)
        (reverse acc)
        (iter (cons (car cells) acc) (cadr cells))))
  (iter nil (car d)))

;; Tests

(define d (make-deque))
(to-list (front-insert-deque! d 5))
(front-deque d)
(rear-deque d)
(to-list (front-insert-deque! d 4))
(front-deque d)
(rear-deque d)
(to-list (rear-insert-deque! d 6))
(front-deque d)
(rear-deque d)
(to-list (front-delete-deque! d))
(front-deque d)
(rear-deque d)
(to-list (rear-delete-deque! d))
(front-deque d)
(rear-deque d)

;; -------------------------------------------------------------------
;; Representing Tables, p.270
;; -------------------------------------------------------------------

(define (assoc key records same-key?)
  (cond ((null? records) false)
        ((same-key? key (caar records)) (car records))
        (else (assoc key (cdr records) same-key?))))

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table) same-key?)))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable) same-key?)))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table) same-key?)))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable) same-key?)))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;; Tests

(put 'a 'b 5)
(get 'a 'b)

(define test-table (make-table =))
((test-table 'insert-proc!) 4 2 42)
(= 42 ((test-table 'lookup-proc) 4.0 2.0))
