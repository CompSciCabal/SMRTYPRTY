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

(displayln "exercise 3.56")
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
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

(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))

(displayln "exercise 3.57")
#|
| If delay is memoized, then it means that previous calculations for the two previous numbers
| won't need to happen again, we simply return the value. Meanwhile, if the function calls weren't
| memoized, each successive call would need to call all the previous calls (because of the recursive
| functionality of the fibonacci implementation)
|#

(displayln "exercise 3.58")
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(define first-360 (expand 1 7 10))
;; (1 . (expand 3 7 10))
;; (1 . (4 . (expand 2 7 10)))
;; (1 . (4 . (2 . (expand 6 7 10))))
;; (1 . (4 . (2 . (8 . (expand 4 7 10)))))
;; #0=(1 4 2 8 5 7 #0#)

(define secnd-360 (expand 3 8 10))
;; (3 7 5 #0=0 #0#)

(displayln "exercise 3.59")

(displayln "exercise 3.59 a.")
(define ones (cons-stream 1 ones))
(define (integrate-series s) (stream-map * (stream-map / ones integers) s))

(displayln "exercise 3.59 b.")
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

(displayln "exercise 3.60")
(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (mul-streams
                             (stream-cdr s1)
                             (stream-cdr s2))
                            (mul-series s1 s2))))
;; It isn't working... I don't know what I'm doing wrong :(

(displayln "exercise 3.63")
(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (louis-sqrt-stream x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))
;; It isn't as efficient because it keeps on re-applying the same function to x
;; over and over again and they haven't been memoized. If we didn't have memoization
;; then this function would be totes cool

(displayln "exercise 3.64")
(define (stream-limit stream tolerance)
  (let [(first (stream-ref stream 0))
        (second (stream-ref stream 1))]
    (if (< (abs (- first second)) tolerance)
        second
        (stream-limit (stream-cdr stream) tolerance))))

(define (tol-sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(displayln "exercise 3.65")
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

(stream-limit ln2-stream 0.01)

(displayln "exercise 3.66")
(define (interleave s1 s2)
  (if (stream-null? s1)
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

(define (num-until-match stream target)
  (define (eq-pair? a b)
    (and (= (car a) (car b))
         (= (cadr a) (cadr b))))
  (define (iter stream count)
    (if (eq-pair? (stream-car stream) target)
        count
        (iter (stream-cdr stream) (+ count 1))))
  (iter stream 0))

(num-until-match (pairs integers integers) '(1 100))
;; The following cannot be computed in any realistic amount of time
;; (num-until-match (pairs integers integers) '(99 100))
;; (num-until-match (pairs integers integers) '(100 100))
;; Scheme-wiki shows these to be the forumulas for how
;; many pairs will be computed. This makes sense since finding
;; the number of pairs before takes super long.
;;
;; f(i,j) = 2^i - 2, i = j
;; f(i,j) = 2^i * (j-i) + 2^(i-1) - 2, i < j

(displayln "exercise 3.67")
;; Nope

(displayln "exercise 3.68")
;; Nope

(displayln "exercise 3.69")
(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (stream-cdr (pairs t u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define (pythagorean-numbers)
  (define (sq x) (* x x))
  (define numbers (triples integers integers integers))
  (stream-filter (lambda (triple)
                   (let [(i (car triple))
                         (j (cadr triple))
                         (k (caddr triple))]
                     (and (< i j)
                          (= (+ (sq i) (sq j))
                             (sq k)))))
                 numbers))

(displayln "exercises 3.70, 3.71 & 3.72")
;; Nope

(displayln "exercise 3.73")
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

;; Had to look at scheme wiki :(
(define (RC R C dt)
  (define (proc I V)
    (add-streams (scale-stream I R) ;; R*i
                 (integral (scale-stream I (/ 1 C)) ;; v0 + (1/C)*integ([0,t], i*dt)
                           V
                           dt)))
  proc)

(displayln "exercise 3.74")
(define sense-data (cons-stream 0 sense-data))
(define (sign-change-detector value last-value) 0)

(define zero-crossings
  ;; using the multi-stream version of stream-map
  (stream-map sign-change-detector
              sense-data ;; <--- stream-car => next reading
              (cons-stream 0 sense-data))) ;; <--- stream-car => last-value

(displayln "exercise 3.75")
(define (make-zero-crossings input-stream last-value last-avpt)
  (let [(avpt (/ (+ (stream-car input-stream) last-value) 2))]
    ;; The error was we weren't preserving the last value, so our
    ;; averages would become more and more warped the further into
    ;; the stream we read. To fix this we allow the last average to
    ;; be passed in and check for a sign change against that based
    ;; on the next average
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))

(displayln "exercise 3.76")
(define (smooth input-stream)
  (define previous-values (cons-stream 0 input-stream))
  (stream-map (lambda (a b) (/ (+ a b) 2)) input-stream previous-values))

(define (better-make-zero-crossings input-stream smooth)
  (let [(smoothed-readings (smooth input-stream))]
    (stream-map sign-change-detector
                smoothed-readings
                (cons-stream 0 smoothed-readings))))
    