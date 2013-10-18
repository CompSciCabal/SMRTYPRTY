;; Mostly in Chicken Scheme (some require bigger numbers than Chicken supports by default)
;;; 1.9
;; a)
;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9

;; b)
;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; 9

;;; 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

;; a) 1024
;; b) 65536
;; c) 65536

;; d) (* 2 n)
;; e) (expt 2 n)
;; f) ...? (map h (list 1 2 3 4 5)) gives (2 4 16 65536 +inf)
;; g) (* 5 (expt n 2))

;;; 1.11
;; a)
(define (f n)
  (if (> 3 n)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

;; b)
(define (f2 n)
  (define (rec a b c count)
    (if (= count 0)
        a
        (rec b c (+ c (* 2 b) (* 3 a)) (- count 1))))
  (rec 0 1 2 n))

;;; 1.12
(define (pascals-triangle line-count)
  (define (line prev-line acc)
    (if (null? (cdr prev-line))
        (cons 1 acc)
        (line
         (cdr prev-line)
         (cons (+ (car prev-line) (cadr prev-line))
               acc))))
  (define (rec total ct acc)
    (if (= total ct)
        (reverse acc)
        (rec total (+ 1 ct) (cons (line (car acc) (list 1)) acc))))
  (rec line-count 1 '((1))))

;;; 1.13
;; I... have no idea how to prove things.

;;; 1.14
;; On (the tree for the solution is n deep). On^2 for time (n/5 steps to calculate the total).

;;; 1.15
(define (cube x)
  (* x x x))
(define (p x)
  (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; a)
;; Five times (obtained experimentally using the Chicken Scheme interpreter).

;; b)
;; O log a time
;; O a space

;;; 1.16
(define (square n) (* n n))
(define (fast-expt base expt)
  (print (list 'fast-expt base expt))
  (cond ((= expt 0) 1)
        ((even? expt) (square (fast-expt base (/ expt 2))))
        (else (* base (fast-expt base (- expt 1))))))

(define (fast-expt-i base expt)
  (define (rec b e acc)
    (cond ((= e 0) acc)
          ((even? e) (rec (square b) (/ e 2) acc))
          (else (rec b (- e 1) (* b acc)))))
  (rec base expt 1))

;;; 1.17
(define (halve a) (floor (/ a 2)))
(define (double a) (+ a a))

(define (times a b)
  (if (= b 0)
      0
      (+ a (times a (- b 1)))))

(define (fast-times a b)
  (cond ((= 0 b) 0)
        ((even? b) (double (fast-times a (halve b))))
        (else (+ a (fast-times a (- b 1))))))

;;; 1.18
(define (fast-times-i a b)
  (define (rec a b acc)
    (cond ((= 0 b) acc)
          ((even? b) (rec (double a) (halve b) acc))
          (else (rec a (- b 1) (+ a acc)))))
  (rec a b 0))

;;; 1.19

;;; 1.20
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; > (gcd 206 40)

;; > (if (= 40 0)
;;       206
;;       (gcd 40 (remainder 206 40)))

;; > (if (= 40 0)
;;       206
;;       (if (= (remainder 206 40) 0)
;;           40
;;           (gcd 40 (remainder a b))))

;;; 1.21
(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (rec n test)
    (cond ((> (square test) n) n)
          ((divides? test n) test)
          (else (rec n (+ test 1)))))
  (rec n 2))

;; a) 199
;; b) 1999
;; c) 7

;;; 1.22
;; The relevant chicken functions are `time` and `cpu-time`
(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (timed-prime-test n)
  (define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (cpu-time) start-time))))
  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))
  (newline)
  (display n)
  (start-prime-test n (cpu-time))
  (newline))

;;; 1.23
(define (next num)
  (if (= 2 num)
      3
      (+ num 2)))

(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))
  (define (rec n test)
    (cond ((> (square test) n) n)
          ((divides? test n) test)
          (else (rec n (next test)))))
  (rec n 2))

;;; 1.24
;; :(

;;; 1.25
;; It wouldn't perform as well, but it's much clearer and produces correct results.
;; So yes, she's correct.


;;; 1.26
;; This is now a tree recursion. The (even? exp) clause calls expmod twice,
;; which means we're branching there.

;;; 1.27
;; This one doesn't work in Chicken scheme
(define (carmichael-test n)
  (define (rec n a)
    (cond ((= 0 a)
           #t)
          ((= (modulo (expt a n) n) a)
           (rec n (- a 1)))
          (else
           #f)))
  (rec n (- n 1)))

(map carmichael-test (list 561 1105 1729 2465 2821 6601))

;; Yes, that returns (list #t #t #t #t #t #t)


;;; 1.28
