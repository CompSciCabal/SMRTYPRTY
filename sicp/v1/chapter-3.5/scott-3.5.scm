;;;SECTION 3.5.1

(define false #f)
(define true #t)
; Deterministic primality test

; Time complexity: Θ(√n)

(define (smallest-divisor n)
  (find-divisor n 2))
(define (square x)
  (* x x))
(define (divides? a b)
  (= (remainder b a) 0))
(define (find-divisor n test-divisor)
  (define (next td)
    (+ td 1))
  (cond
    ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (next test-divisor)))))
(define (prime? n)
  (and (> n 1) (= (smallest-divisor n) n)))
(define (sum-primes a b)
  (define (iter count accum)
    (cond
      ((> count b) accum)
      ((prime? count) (iter (+ count 1) (+ count accum)))
      (else (iter (+ count 1) accum))))
  (iter a 0))
(prime? 7)
(define (sum-primes a b)
  (accumulate + 0 (filter prime? (enumerate-interval a b))))
(define (filter pred? x)
  (define (iter x r)
    (if (null? x)
      r
      (if (pred? (car x))
        (iter (cdr x) (cons (car x) r))
        (iter (cdr x) r))))
  (iter x '()))
(filter null? (list 'h 'j 'k 'l 1 '() 'm '()))
(define (enumerate-interval a b)
  (define (iter c L)
    (if (< c a)
      L
      (iter (- c 1) (cons c L))))
  (iter b '()))
(enumerate-interval 1 10)
(prime? 10007)
(car (cdr (filter prime? (enumerate-interval 10000 10013))))
(define (stream-car s)
  (car s))
(define (stream-cdr s)
  (force (cdr s)))
(define (stream-null? s)
  (null? s))
(define the-empty-stream '())
(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream
      (proc (stream-car s))
      (stream-map proc (stream-cdr s)))))
(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin
      (proc (stream-car s))
      (stream-for-each proc (stream-cdr s)))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))
;; stream-car and stream-cdr would normally be built into

;;  the stream implementation

;: (define (stream-car stream) (car stream))

;: (define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream low (stream-enumerate-interval (+ low 1) high))))
(define (stream-filter pred stream)
  (cond
    ((stream-null? stream) the-empty-stream)
    ((pred (stream-car stream)) (cons-stream
      (stream-car stream)
      (stream-filter pred (stream-cdr stream))))
    (else (stream-filter pred (stream-cdr stream)))))
(stream-car
  (stream-cdr
    (stream-filter prime? (stream-enumerate-interval 10000 1000000))))
;; force would normally be built into

;;  the stream implementation

;: (define (force delayed-object)

;:   (delayed-object))

(define (memo-proc proc)
  (let ((already-run? false)
  (result false))
    (lambda ()
      (if (not already-run?)
        (begin (set! result (proc)) (set! already-run? true) result)
        result))))
;; EXERCISE 3.51

(define (show x)
  (display-line x)
  x)
(define x
  (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)
;; EXERCISE 3.52

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
;: (define seq (stream-map accum (stream-enumerate-interval 1 20)))

;: (define y (stream-filter even? seq))

;: (define z (stream-filter (lambda (x) (= (remainder x 5) 0))

;:                          seq))

;: (stream-ref y 7)

;: (display-stream z)

;; EXERCISE 3.56

(define (merge s1 s2)
  (cond
    ((stream-null? s1) s2)
    ((stream-null? s2) s1)
    (else
      (let ((s1car (stream-car s1))
      (s2car (stream-car s2)))
        (cond
          ((< s1car s2car)
            (cons-stream s1car (merge (stream-cdr s1) s2)))
          ((> s1car s2car)
            (cons-stream s2car (merge s1 (stream-cdr s2))))
          (else
            (cons-stream
              s1car
              (merge (stream-cdr s1) (stream-cdr s2)))))))))
;;;SECTION 3.5.2

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
(define (divisible? x y)
  (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x)
    (not (divisible? x 7)))
                 integers))
;; (display-stream integers)

;: (stream-ref no-sevens 100)

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs
  (fibgen 0 1))
(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (sieve
      (stream-filter
        (lambda (x)
          (not (divisible? x (stream-car stream))))
        (stream-cdr stream)))))
(define primes
  (sieve (integers-starting-from 2)))
(stream-ref primes 50)
;;;Defining streams implicitly;;;Defining streams implicitly

(define (stream-map proc . streams)
  (if (stream-null? (car streams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car streams))
      (apply stream-map (cons proc (map stream-cdr streams))))))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define ones
  (cons-stream 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers
  (cons-stream 1 (add-streams ones integers)))
(define fibs
  (cons-stream
    0
    (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))
(define (scale-stream stream factor)
  (stream-map (lambda (x)
    (* x factor))
              stream))
(define double
  (cons-stream 1 (scale-stream double 2)))
(define primes
  (cons-stream 2 (stream-filter prime? (integers-starting-from 3))))
(define (prime? n)
  (define (iter ps)
    (cond
      ((> (square (stream-car ps)) n) true)
      ((divisible? n (stream-car ps)) false)
      (else (iter (stream-cdr ps)))))
  (iter primes))
;;;SECTION 3.5.3

(define (average a b)
  (/ (+ a b) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0 (stream-map (lambda (guess)
      (sqrt-improve guess x))
                guesses)))
  guesses)
;; (display-stream (sqrt-stream 2))

(define (partial-sums s)
  (define (iter s)
    (cons-stream 0 (add-streams s (iter s))))
  (stream-cdr (iter s)))
(define (pi-summands n)
  (cons-stream
    (/ 1.0 n)
    (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))
;; (display-stream pi-stream)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
  (s1 (stream-ref s 1))
  (s2 (stream-ref s 2)))
    (cons-stream
      (-
        s2
        (/ (square (- s2 s1)) (+ s0 (* -2 s1) s2)))
      (euler-transform (stream-cdr s)))))
;; (display-stream (euler-transform pi-stream))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))
;; (display-stream (accelerated-sequence euler-transform pi-stream))

;; exercise 3.64

;; stream-limit procedure

(define (subtract-streams s1 s2)
  (stream-map - s1 s2))
;;(display-stream (subtract-streams integers (stream-cdr integers)))

(define (stream-limit s tolerance)
  (define (pred? x)
    (< (abs x) tolerance))
  (cond
    ((stream-null? s) the-empty-stream)
    ((pred?
      (stream-car (subtract-streams s (stream-cdr s))))
     (stream-car (stream-cdr s)))
    (else (stream-limit (stream-cdr s) tolerance))))
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
(sqrt 2 0.0001)
;; exercise 3.66

;; pairs

(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream
      (stream-car s1)
      (interleave s2 (stream-cdr s1)))))
(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x)
        (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))
;; (display-stream (pairs integers integers))

;; This interleaving approach will give a precise order to the results

;; The pairs function was split into 3 parts, the current value, 

;; the current-row stream, then the recursive pairs call

;; the results fromeach pairs call will alternate between the two streams

;; f(i,j) = 1 + (j-1)2^i

;; so the stride of iteration is 2^i

;; the offset is 2^i - 1

;; not sure it isnt quite right, but something like this

(define (f i j)
  (let ((s (expt 2 i)))
    (+ (/ s 2) (* (- j i) s))))
(f 2 5)
;; exercise 3.67

;; not doing it, but i think you would interleave in a stream for the column as well as the row

;; exercise 3.68

(define (louis-pairs s t)
  (interleave
    (stream-map (lambda (x)
      (list (stream-car s) x))
                t)
    (louis-pairs (stream-cdr s) (stream-cdr t))))
;; (display-stream (louis-pairs integers integers))

;; seems to go infinitely, I think its because no car value is ever being set in the stream

;; im not sure, this is confusing me actually

;; exercise 3.70

(define (merge-weighted s1 s2 w)
  (cond
    ((stream-null? s1) s2)
    ((stream-null? s2) s1)
    (else
      (let ((s1car (stream-car s1))
      (s2car (stream-car s2)))
        (let ((i1 (car s1car))
        (i2 (car (cdr s1car)))
        (j1 (car s2car))
        (j2 (car (cdr s2car))))
          (let ((w1 (w i1 i2))
          (w2 (w j1 j2)))
            (cond
              ((< w1 w2)
                (cons-stream s1car (merge-weighted (stream-cdr s1) s2 w)))
              (else
                (cons-stream s2car (merge-weighted s1 (stream-cdr s2) w))))))))))
(define (weighted-pairs s t w)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x)
        (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) w)
      w)))
;; (display-stream (weighted-pairs integers integers +))

;; exercise 3.71 ramanujan numbers

(define (ramjam)
  (define (cubesum i j)
    (+ (* i i i) (* j j j)))
  (define (pred? a b)
    (let ((i1 (car a))
    (i2 (cadr a))
    (j1 (car b))
    (j2 (cadr b)))
      (let ((w1 (cubesum i1 i2))
      (w2 (cubesum j1 j2)))
        (= w1 w2))))
  (define wp
    (weighted-pairs integers integers cubesum))
  (define (iter s t)
    (if (pred? (stream-car s) (stream-car t))
      (cons-stream
        (cubesum (car (stream-car s)) (cadr (stream-car s)))
        (iter t (stream-cdr t)))
      (iter t (stream-cdr t))))
  (iter wp (stream-cdr wp)))
;; (display-stream (ramjam))

;; 

;;;

;;;

;;;

;;;

;;;

;;;

;;;
