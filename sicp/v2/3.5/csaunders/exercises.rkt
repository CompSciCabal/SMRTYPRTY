#lang racket
;; delay and cons-stream cannot be implemented in pure racket
;; as a result we need to use macros/custom syntax
;; http://stackoverflow.com/questions/24529271/sicp-cons-stream
(define (memo-proc proc)
  (let [(already-run? false)
        (result false)]
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define-syntax delay
  (syntax-rules ()
    ((_ exp) (lambda () exp))))
(define (force delayed-object) (delayed-object))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream '())
(define (stream-null? stream) (eq? the-empty-stream stream))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(displayln "exercise 3.50")
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams)))
       )
      )
  )

(define sum-lists (stream-map +
                              (stream-enumerate-interval 1 3)
                              (stream-enumerate-interval 4 6)
                              (stream-enumerate-interval 7 9)))

(displayln "exercise 3.51")

(define (display-line x)
  (newline)
  (display x))

(define (show x)
  (displayln x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
;; prints out 1 to 5 on new lines, then returns 5
(stream-ref x 7)
;; prints out 6 to 7 on new lines, then returns 7
;; I don't quite know how exactly pull off the correct stream for this
;; though. I'm getting 1 - 7, so the stream isn't behaving correctly.

(displayln "exercise 3.51")
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(display-stream seq)
;; full stream '(1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210)
;; sum => 1
(define y (stream-filter even? seq))
;; sum => 6
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
;; sum => 10

(stream-ref y 7)
;; sum => 162

(display-stream z)
;; sum => 362

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
