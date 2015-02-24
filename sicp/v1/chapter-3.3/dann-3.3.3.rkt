#lang planet neil/sicp


;;; 3.3.3

;; EX 3.24

(define (assoc key records same-key?)
  (cond ((null? records) false)
        ((same-key? key (caar records)) (car records)) 
        (else (assoc key (cdr records)))))

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2) 
      (let ((subtable
             (assoc key-1 (cdr local-table) same-key?)))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable) same-key?)))
              (if record (cdr record) false)) 
            false)))
    (define (insert! key-1 key-2 value) 
      (let ((subtable
             (assoc key-1 (cdr local-table) same-key?))) 
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable) same-key?)))
              (if record
                  (set-cdr! record value) 
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

; (define xxx (make-table equal?))
; ((xxx 'insert-proc!) 'a 'b 1)
;   'ok
; ((xxx 'lookup-proc) 'a 'b)
;   1
; (define yyy (make-table (lambda (a b) (< (- a b) 10))))
; ((yyy 'insert-proc!) 100 200 'yep)
;   'ok
; ((yyy 'lookup-proc) 99 199)
;   'yep
; ((yyy 'lookup-proc) 101 201)
;   'yep
; ((yyy 'lookup-proc) 110 210)


; EX 3.25

(define (multi-lookup keylist table)
  (if (pair? (cdr keylist))
      (let ((subtable (assoc (car keylist) (cdr table) equal?)))
        (if subtable
            (multi-lookup (cdr keylist) subtable)
            false))
      (let ((record (assoc (car keylist) (cdr table) equal?))) 
        (if record
            (cdr record)
            false))))

(define (multi-insert! keylist value table) 
  (if (pair? (cdr keylist))
      (let ((subtable (assoc (car keylist) (cdr table) equal?)))
        (if subtable
            (multi-insert! (cdr keylist) subtable)
            (begin 
              (set-cdr! table
                        (cons (list (car keylist) '())
                              (cdr table)))
              (multi-insert! (cdr keylist) 
                             value
                             (assoc (car keylist) (cdr table) equal?)))))
      (let ((record (assoc (car keylist) (cdr table) equal?)))
        (if record
            (set-cdr! record value) 
            (set-cdr! table
                      (cons (cons (car keylist) value)
                            (cdr table))))))
'ok)

; (define x (list '*table*))
; (multi-insert! '(a b) 123 x)
; bug


; EX 3.26

; See notebook picture

; EX 3.27

(define (assoc-argh key records)
  (assoc key records equal?))

(define (lookup key table)
  (let ((record (assoc-argh key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value) 
        (set-cdr! table
                  (cons (cons key value)
                        (cdr table)))))
  'ok)

(define (make-table-argh)
  (make-table equal?))

(define (memoize f)
  (let ((table (make-table-argh)))
    (lambda (x)
      (let ((previously-computed-result
             (lookup x table)))
        (or previously-computed-result
            (let ((result (f x))) 
              (insert! x result table) 
              result))))))

(define memo-fib 
  (memoize
   (lambda (n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else (+ (memo-fib (- n 1))
                    (memo-fib (- n 2))))))))

; see notebook for env diagram

; it wouldn't work with (memoize fib) because that 
; wouldn't call memo-fib in the recursive phase.
; but if racket allowed redefinition maybe you could do
; (define fib (memoize fib)) 
; after fib was defined...