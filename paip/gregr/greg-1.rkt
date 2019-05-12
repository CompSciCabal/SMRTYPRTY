#lang racket

(define-syntax example
  (syntax-rules ()
    ((_ e) (printf "\n~s\n==>\n~s\n" 'e e))))


(define (last-name n)
  (define rn (reverse n))
  (case (car rn)
    ;; TODO: fix this to handle all human life before deploying.
    ((MD M.D. PhD Ph.D. DPhil Dr.phil Dr.
      Sr Sr. Jr Jr. I i II ii III iii IV iv)
     (cadr rn))
    (else (car rn))))

;; Commas are not supported.
(example (last-name '(Rex Morgan MD)))
(example (last-name '(Morton Downey Jr.)))


(define (power n p)
  (cond ((= 0 p)   1)
        ((odd? p)  (* n (power n (- p 1))))
        ((even? p) (power (* n n) (/ p 2)))))

(example (power 3 2))
(example (power 2 10))


(define (count-atoms d)
  (match d
    (`(,a . ,b) (+ (count-atoms a) (count-atoms b)))
    (`#(,@ds)   (apply + (map count-atoms ds)))
    ('()        0)
    (_          1)))

(example (count-atoms '(a (b) c)))


(define (count-anywhere x d)
  (let loop ((d d))
    (if (equal? x d) 1
      (match d
        (`(,a . ,b) (+ (loop a) (loop b)))
        (`#(,@ds)   (apply + (map loop ds)))
        (_          0)))))

(example (count-anywhere 'a '(a ((a) b) a)))
(example (count-anywhere '(x y) '((z x y) ((x y z) b) x y)))


(define (dot-product a b) (apply + (map * a b)))

(example (dot-product '(10 20) '(3 4)))
