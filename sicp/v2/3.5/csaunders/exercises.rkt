#lang racket

(define the-empty-stream '())

(define (stream-null? stream)
  (null? stream))

(define (memo-proc proc)
  (let [(already-run? false)
        (result false)]
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream head tail)
     (cons head (delay tail)))))

(define-syntax delay
  (syntax-rules ()
    ((delay object)
     (memo-proc (lambda() object)))))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-ref s n)
  (if (= 0 n)
      (stream-car s)
      (stream-ref s (- n 1))))

(define (stream-map proc . streams)
  (if (stream-null? streams)
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car streams))
       (apply stream-map
              (cons proc (map stream-cdr streams))))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

;;(define (displayln line)
;;  (display line)
;;  (newline))

(displayln "exercise 3.53")
#|
| The result of the stream is a doubler
| S starts off as 1, with a deferred object that
| would sum S and S, together. So, (stream-cdr s)
| would be (sum s s) -> (sum (car s) (car s)) -> (sum 1 1)
| Afterwards, S is now 2, so calling (stream-cdr s) would
| result in (sum s s) -> (sum 2 2) -> 4
|#

(displayln "exercise 3.54")
(define (mul-streams first second)
  (stream-map * first second))

(define factorials (cons-stream 1
                                (mul-streams factorials integers)))

(displayln "exercise 3.55")
(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (stream-map +
                           (partial-sums stream)
                           (stream-cdr stream))))