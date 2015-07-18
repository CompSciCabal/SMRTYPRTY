#lang racket
#| Exercise 3.50.  Complete the following definition, which generalizes stream-map 
to allow procedures that take multiple arguments, analogous to map in section 
2.2.3, footnote 12. 

Spending some time understanding 
https://stackoverflow.com/questions/21629203/implementation-of-variadic-map-function-in-scheme
was very helpful for answering this question.

Variadic is the college word for 'accepts arbitrary number of arguments'.
|#
(define the-empty-stream '())

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (memo-proc (lambda () b))))))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       ; map with car! ie, give me the first element of each list.
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))
(define (force proc) (proc))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
#|
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))
|#
(define (stream-for-each proc s)
  (if (null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))

(define (stream-filter pred stream)
  (cond ((null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

#|
(stream-car
 (stream-cdr
  (stream-filter prime?
                 (stream-enumerate-interval 10000 1000000))))
|#

#|
Exercise 3.52.  Consider the sequence of expressions

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(stream-ref y 7)
(display-stream z)

Q: What is the value of sum after each of the above expressions is evaluated? 
python: sum(range(1, 21)) = 210

Q: What is the printed response to evaluating the stream-ref and display-stream 
expressions? 
stream-ref: 28

Would these responses differ if we had implemented (delay <exp>) 
simply as (lambda () <exp>) without using the optimization provided by memo-proc? 
Explain.
|#

; Section 3.5.2
; -------------


(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))
; (stream-ref no-sevens 100)

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))
(stream-ref primes 50)

(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define ones (cons-stream 1 ones))
(define integers2 (cons-stream 1 (add-streams ones integers)))


#|
 Exercise 3.53.  Without running the program, describe the elements of the stream defined by

(define s (cons-stream 1 (add-streams s s)))

1 . promise to add 1 + 1 --> 1 . promise 2
1. 2 . promise to add 2 + 2 --> 4
so the sequence is 1 2 4 8 16...
|#

