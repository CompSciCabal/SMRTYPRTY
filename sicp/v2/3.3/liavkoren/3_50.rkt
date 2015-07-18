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
Exercise 3.53
-------------

Without running the program, describe the elements of the stream defined by
(define s (cons-stream 1 (add-streams s s)))

1 . promise to add 1 + 1 --> 1 . promise 2
1. 2 . promise to add 2 + 2 --> 4
so the sequence is 1 2 4 8 16...
|#
(displayln "excercise 3.53: the stream is 1 2 4 8 16...")
#|
Exercise 3.54
-------------
Define a procedure mul-streams, analogous to add-streams, that produces the 
elementwise product of its two input streams. Use this together with the stream 
of integers to complete the following definition of the stream whose nth element 
(counting from 0) is n + 1 factorial:

(define factorials (cons-stream 1 (mul-streams <??> <??>)))

factorial stream is: 
1 1*2 1*2*3 1*2*3*4 1*2*3*4*5 ... == 1 2 6 24 120... ==
(1)*1 (1)*2 ((1)*2)*3 (((1)*2)*3)*4... ==
(1 2 3 4...) * (1 1*1 1*2 1*2*3...) == int_stream * (1 . 1 . (cdr int_stream))

we need to find an implicit representation of factorial. The stream is can be 
represented implicitely as: 
1 . <promise of 2 * previous value of fact> . <promise of 3 * prev. val> . <promise of 4 * prev. value> ...>>>>
|#
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

; You can do it like this:
(define fact
  (cons-stream 1 
               (cons-stream 1
                            (mul-streams (stream-cdr fact)
                                        integers))))
; analogous to fib:
(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))
; But a more straightforward def is: 
 (define fact2 (cons-stream 1 (mul-streams integers fact2))) 
; Hoooly crap, implicit streams! 
#|
Exercise 3.55
-------------
Define a procedure partial-sums that takes as argument a stream S 
and returns the stream whose elements are S0, S0 + S1, S0 + S1 + S2, .... 
For example, (partial-sums integers) should be the stream 1, 3, 6, 10, 15, ....

This is the same structure as fact, replace * with +, and the base case is S0 rather than 1.
|#

(define (partial-sums stream)
  (cons-stream (stream-car stream)
               (add-streams (stream-cdr stream) (partial-sums stream)))) 
(define p (partial-sums integers))
; the trick here was doing `(add-streams (stream-cdr stream) (partial-sums stream))` to advance 
; the stream one more element forward. 

