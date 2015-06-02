#lang racket
(require scheme/mpair)

(displayln "exercise 3.10")
(displayln "--- TODO ---")

(displayln "exercise 3.11")
(displayln "--- TODO ---")

(displayln "exercise 3.12")
(define (append x y)
  (if (empty? x)
      y
      (mcons (mcar x) (append (mcdr x) y))))

(define (append! x y)
  (set-mcdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (empty? (mcdr x))
      x
      (last-pair (mcdr x))))
(define x (mlist 'a 'b))
(define y (mlist 'c 'd))
(define z (append x y))
(displayln "> z")
z ;> '(a b c d)
(displayln "> (cdr x)")
(mcdr x) ;> '(b)
(define w (append! x y))
(displayln "> w")
w ;> '(a b c d)
(displayln "> (cdr x)")
(mcdr x) ;> '(b c d)

;; Box and pointer (-]-> means ptr, /] means null)
;; x [a|-]->[b|/]
;; y [c|-]->[d|/]
;; (append! x y)
;; Directly modifies (cddr x) to point to y
;; [a|-]->[b|-]->|
;;               |
;; |<------------|
;; |
;; |->[c|-]->[d|/]

(displayln "exercise 3.13")
(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)

(define z1 (make-cycle (mlist 'a 'b 'c)))
;; [a|-]->[b|-]->[c|-]-|
;;  ^                  |
;;  |------------------|

(displayln "exercise 3.14")
(define (mystery x)
  (define (loop x y)
    (if (empty? x)
        y
        (let [(temp (mcdr x))]
          (set-mcdr! x y)
          (loop temp x))))
  (loop x (mlist)))

;; Mystery "in place" reverses the list
(define v1 (mlist 'a 'b 'c 'd))
;; [a|-]->[b|-]->[c|-]->[d|/]
(define w1 (mystery v1))
;; [d|-]->[c|-]->[b|-]->[a|/]

(displayln "exercise 3.15")
(define x1 (mlist 'a 'b))
(define zz1 (mcons x1 x1))
(define zz2 (mcons (mlist 'a 'b) (mlist 'a 'b)))

(define (set-to-wow! x)
  (set-mcar! (mcar x) 'wow)
  x)
;; x1 [wow|-]->[b|/]
;;     ^---------------|
;; zz1 |<(mcar)-[-|-]->|
;; because the car and cdr of zz1 both point
;; to x and set-to-wow! sets the (mcar x) to 'wow;
;; zz1's car and cdr both see the change
;;    [wow|-]->[b|/]       [a|-]->[b|/]
;;     ^                  ^
;; zz2 |-<(mcar)-[-|-]->--|
(set-to-wow! zz1)
(set-to-wow! zz2)
(displayln "> zz1")
zz1
(displayln "> zz2")
zz2
