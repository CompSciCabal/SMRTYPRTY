#lang racket
#| Exercise 3.50.  Complete the following definition, which generalizes stream-map 
to allow procedures that take muliple arguments, analogous to map in section 
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

#|
Exercise 3.56
-------------
A famous problem, first raised by R. Hamming, is to enumerate, in ascending
order with no repetitions, all positive integers with no prime factors other
than 2, 3, or 5. One obvious way to do this is to simply test each integer 
in turn to see whether it has any factors other than 2, 3, and 5. But this 
is very inefficient, since, as the integers get larger, fewer and fewer of 
them fit the requirement. As an alternative, let us call the required stream 
of numbers S and notice the following facts about it.

- S begins with 1.
- The elements of (scale-stream S 2) are also elements of S.
- The same is true for (scale-stream S 3) and (scale-stream 5 S).
- These are all the elements of S.

Now all we have to do is combine elements from these sources. For this we define
a procedure merge that combines two ordered streams into one ordered result 
stream, eliminating repetitions:
|#

(define (merge s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))


#|
Then the required stream may be constructed with merge, as follows:

(define S (cons-stream 1 (merge <??> <??>)))

Fill in the missing expressions in the places marked <??> above.
|#
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))


#|
Exercise 3.57
-------------
How many additions are performed when we compute the nth Fibonacci 
number using the definition of fibs based on the add-streams procedure? 
Show that the number of additions would be exponentially greater if we 
had implemented (delay <exp>) simply as (lambda () <exp>), without using 
the optimization provided by the memo-proc procedure described in section 
3.5.1.64
|#
(newline)
(displayln "Exercise 3.57: ")
(displayln "Using call-by-need, the nth Fib number needs n-2 additions, because all previous")
(displayln "additions are cached and the first two elements of the list are predefinded. ")
(displayln "Withour caching, the number of additions follows a very similar pattern to the ")
(displayln "fib sequence itself: the number of additions needed to find f(i) is the number of ")
(displayln "additions for f(i-1) + number of additions for f(i-2) plus one, so the number of ")
(displayln "additions involved grows even faster than the Fib sequence.")

#|
Exercise 3.58
-------------
  Give an interpretation of the stream computed by the following procedure:

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(Quotient is a primitive that returns the integer quotient of two integers.) 
What are the successive elements produced by (expand 1 7 10) ? What is produced by (expand 3 8 10) ?
|#
(newline)
(displayln "Exercise 3.58")
(displayln "Oh tricksy hobbiteses! Very nice. Expand(1 7 10) = cycle(1 4 2 8 5 7),")
(displayln "expand(3 8 10) = 3 7 5 0 0 0... These are the base 10 decimal expansions")
(displayln "Of num/den.")

#|
Exercise 3.59
-------------
a. The integral of the series a_0 + a_1*x + a_2*x^2 + a_3*x^3 + ··· is the series
c + a_0*x + a_1*x^2 + a_2*x^3 + a_3*x^4 where c is any constant. Define a procedure 
integrate-series that takes as input a stream a_0, a_1, a_2... representing a power 
series and returns the stream a0, (1/2)a1, (1/3)a2, ... of coefficients of the 
non-constant terms of the integral of the series.
|#

(define (integrate-series series)
  (let ((coefficients (stream-map (lambda (x) (/ 1 x)) (integers-starting-from 1))))
    (mul-streams series coefficients)))

(define test (integrate-series ones)) ; use stream-ref to confirm coefficients are correct.


#|
b. The function x -> e^x is its own derivative. This implies that e^x and the integral of 
e^x are the same series, except for the constant term, which is e_0 = 1. Accordingly, we 
can generate the series for e^x as

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

Show how to generate the series for sine and cosine, starting from the facts that the 
derivative of sine is cosine and the derivative of cosine is the negative of sine:

(define cosine-series
  (cons-stream 1 <??>))
(define sine-series
  (cons-stream 0 <??>))
|#
(newline)
(displayln "Exercise 3.59b:")
(displayln "Let Dx stand for Derivitive of a function w/r/t x, and Int be the integral ")
(displayln "of the function w/r/t dx. Dx sin(x) = cos(x) and Dx cos(x) = -sin(x), ")
(displayln "so sin(x) = [Int cos(x)] - C and cos(x) = -[Int sin(x)] - C.")
(displayln "It is witchy and insane that this works.. but you can define them in terms of each other.")

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

#|
Exercise 3.60
-------------  
With power series represented as streams of coefficients as in exercise 3.59, 
adding series is implemented by add-streams. Complete the definition of the 
following procedure for multiplying series:

(define (mul-series s1 s2)
  (cons-stream <??> (add-streams <??> <??>)))
|#

; Eli Bendersky's solution and explaination are the best I've seen so far:

(define (mul-series u v)
  (cons-stream 
    (* (stream-car u) (stream-car v))
    (add-streams 
      (scale-stream (stream-cdr v) (stream-car u))
      (mul-series (stream-cdr u) v))))

(define mul-test 
  (add-streams (mul-series sine-series sine-series)
              (mul-series cosine-series cosine-series)))

#|
Exercise 3.61
-------------
Let S be a power series (exercise 3.59) whose constant term is 1. Suppose we
want to find the power series 1/S, that is, the series X such that S · X = 1.
Write S = 1 + SR where SR is the part of S after the constant term. Then we can
solve for X as follows:

See https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-24.html#%_thm_3.61 
for diagram.

In other words, X is the power series whose constant term is 1 and whose higher-
order terms are given by the negative of SR times X. Use this idea to write a
procedure invert-unit-series that computes 1/S for a power series S with
constant term 1.
|#

(define (invert-unit-series series)
  (cons-stream 1 (scale-stream (mul-series (stream-cdr series) (invert-unit-series series)) -1)))
(newline)
(displayln "Exercise 3.61:")
(displayln "This is straightforward, just translate the formula presented to into a recursive stream.")
(displayln "The solution can be checked with by `stream-ref`ing into the series produced by:")
(displayln "(mul-series (invert-unit-series cosine-series) cosine-series).")

(define invert-unit-test (invert-unit-series cosine-series))
#|
Exercise 3.62
-------------  
Use the results of exercises 3.60 and 3.61 to define a procedure div-series that
divides two power series. Div-series should work for any two series, provided
that the denominator series begins with a nonzero constant term. (If the
denominator has a zero constant term, then div-series should signal an error.)
Show how to use div-series together with the result of exercise 3.59 to generate
the power series for tangent.
|#
(newline)
(displayln "Exercise 3.62:")
(displayln "I went totally down the wrong path for this, you can see what I was trying to do in")
(displayln "commit 1b271234a3c. In short, though, I was trying to add an offset to the stream to make")
(displayln "it's constant term 1, then add the inverse of the offset back in. Invert-unit-stream, however,")
(displayln "just ignores the constant term, so it's much more elegant (and correct!) to use")
(displayln "invert-unit-stream and scale the result by what the constant terms should be! I realized this")
(displayln "while looking at Eli Bendersky's answer. Following answer is based off of his.")

; Because I keep getting confused between the two:
(define (stream-cons x y)
  (cons-stream x y))

(define (div-series num denom)
  (let ((const (stream-car denom)))
    (mul-series num 
                (scale-stream (invert-unit-series denom) const))))

(define tan (div-series sine-series cosine-series))


; ===================================================
;
; Section 3.5.3
; -------------

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

; (display-stream (sqrt-stream 2))

; Using Leibniz's formula for Pi, pi/4 = 1 - 1/3 + 1/5 - 1/7 + 1/9 - ...

; The stream defined by (pi-summands 1) gives the sequence of fractions in the Leibniz formula.
; Eg: (stream-ref (pi-summands 1) 0) = 1, (stream-ref (pi-summands 1) 1) = -1/3, etc...
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

; Remember, partial-sums takes a stream S and returns S0, S0 + S1, S0 + S1 + S2, ...
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

;(display-stream pi-stream)

; Heavy magic follows
(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define pi (accelerated-sequence euler-transform pi-stream))


#|
Exercise 3.63
-------------
Louis Reasoner asks why the sqrt-stream procedure was not written in the
following more straightforward way, without the local variable guesses:

(define (sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))

vs:

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)


Alyssa P. Hacker replies that this version of the procedure is considerably less
efficient because it performs redundant computation. Explain Alyssa's answer.
Would the two versions still differ in efficiency if our implementation of delay
used only (lambda () <exp>) without using the optimization provided by memo-proc
(section 3.5.1)?
|#

(define (sqrt-stream-bad x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream-bad x))))

(stream-ref (sqrt-stream 5) 3)
(stream-ref (sqrt-stream-bad 5) 3)

(newline)
(displayln "Exercise 3.63:")
(displayln "It looks like in Louis' version the recursive call to sqrt-stream has to redo work that was already done.")
(displayln "Using a reference to the same stream in the correct version allows the cached values to be used instead. ")

#|
Exercise 3.64
-------------
Write a procedure stream-limit that takes as arguments a stream and a number
(the tolerance). It should examine the stream until it finds two successive
elements that differ in absolute value by less than the tolerance, and return
the second of the two elements. Using this, we could compute square roots up to
a given tolerance by

(define (sqrt x tolerance) (stream-limit (sqrt-stream x) tolerance))
|#

(define (stream-limit stream tolerance)
  (define (good-enough? x y)
    (< (abs (- x y)) tolerance))
  ; we only need an inner function if we want to track how many iterations are needed. Oh well. 
  (let ((counter 0))
    (define (inner stream tolerance)
      (set! counter (+ counter 1))
      (if (good-enough? (stream-car stream) (stream-car (stream-cdr stream))) 
          (begin           
            (display "took ")
            (display counter)
            (display " iteration(s)")
            (newline)
            (stream-car (stream-cdr stream)))        
          (inner (stream-cdr stream) tolerance)))
    (inner stream tolerance)))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 5 0.1)
(sqrt 5 0.01)
(sqrt 5 0.001)

#|
Exercise 3.65
-------------
Use the series

ln(2) = 1 - 1/2 + 1/3 - 1/4 + ...

to compute three sequences of approximations to the natural logarithm of 2, in
the same way we did above for . How rapidly do these sequences converge?
|#


; The stream defined by (ln-summands 1) gives the sequence of fractions for ln(2)
; 1, -1/2, 1/3, -1/4, ...
(define (ln-summands n)
  (cons-stream (/ 1 n)
               (stream-map - (ln-summands (+ n 1)))))
; Remember, partial-sums takes a stream S and returns S0, S0 + S1, S0 + S1 + S2, ...
(define ln-stream
  (partial-sums (ln-summands 1)))

; ln(2) = 0.693147180559945...
; ============================
;
;(define ln2 (stream-limit ln-stream 0.1)) ; 1627/2520
(displayln "Naive ln(2), to a tolerance of 0.01: ")
(define ln2 (stream-limit ln-stream 0.01)) ; o_O

(displayln "Using Euler's accel series magic, it's *soooooo* much faster. Ln(2) to a tolerance of 0.000001:")
(define ln2_less_suck (stream-limit (accelerated-sequence euler-transform ln-stream) 0.000001))


; Infinite streams of pairs
; -------------------------
; (as infinite as this chapter section..)

#|
For example, suppose we want to generalize the prime-sum-pairs procedure of
section 2.2.3 to produce the stream of pairs of all integers (i,j) with i < j
such that i + j is prime. If int-pairs is the sequence of all pairs of integers
(i,j) with i < j, then our required stream is simply66
|#
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))
#|
(stream-filter (lambda (pair)
                 (prime? (+ (car pair) (cadr pair))))
               int-pairs)

(stream-map (lambda (x) (list (stream-car s) x))
            (stream-cdr t))
|#
(define (interleave s1 s2)
  (if (null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))
(define int-pairs (pairs integers integers))

#|
Exercise 3.66
-------------  
Examine the stream (pairs integers integers). Can you make any general comments
about the order in which the pairs are placed into the stream? For example,
about how many pairs precede the pair (1,100)? the pair (99,100)? the pair
(100,100)? (If you can make precise mathematical statements here, all the
better. But feel free to give more qualitative answers if you find yourself
getting bogged down.)
|#
(newline)
(displayln "First 12 entries in int-pairs:")
(stream-ref int-pairs 0)
(stream-ref int-pairs 1)
(stream-ref int-pairs 2)
(stream-ref int-pairs 3)
(stream-ref int-pairs 4)
(stream-ref int-pairs 5)
(stream-ref int-pairs 6)
(stream-ref int-pairs 7)
(stream-ref int-pairs 8)
(stream-ref int-pairs 9)
(stream-ref int-pairs 10)
(stream-ref int-pairs 11)

#|
0:(1 1)     1:(1 2)     3:(1 3)   5:(1 4)   7:(1 5)   9:(1 6)   11:(1 7)
            2:(2 2)     4:(2 3)   8:(2 4)
                        6:(3 3)  10:(3 4)

|#

(define (find-pair x y)
  (define (inner stream counter)
    (let ((pair (stream-car stream)))
      (if (and (= x (car pair)) (= y (cadr pair)))
        (printf "Took ~a iterations to find pair." counter)
        (inner (stream-cdr stream) (+ counter 1)))
      )
    )
  (inner int-pairs 0))
(newline)
(displayln "Exercise 3.66:")
(displayln "Takes 17 iterations for 1 10")
(displayln "Takes 32 iterations for 2 10")
(displayln "Takes 37 iterations for 1 20")
(displayln "Takes 72 iterations for 2 20")
(displayln "Takes 97 iterations for 1 50")
(displayln "Takes 197 iterations for 1 100")
(displayln "Takes 397 iterations for 1 200")
(displayln "Takes 92670 iterations for 10 100")
; (find-pair 2 20)
(newline)
(displayln "It seems like to find (1 n) it takes roughly 2n iterations.")
(displayln "Every row above 1 seems to roughly double the iteration count again, eg")
(displayln "so (2 n) is roughly double (1 n).")
