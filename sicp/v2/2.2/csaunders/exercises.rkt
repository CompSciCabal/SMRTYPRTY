#lang racket
(displayln "exercise 2.17")
(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

(last-pair (list 23 72 149 34))

(displayln "exercise 2.18")
(define (reverse lst)
  (define (reverse-iter lst acc)
    (if (null? lst)
        acc
        (reverse-iter (cdr lst)
                      (cons (car lst) acc))))
  (reverse-iter lst '()))

(reverse (list 1 4 9 16 25))

(displayln "exercise 2.19")
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (first-denomination coins)
  (car coins))

(define (except-first-denomination coins)
  (cdr coins))

(define (no-more? coins)
  (null? coins))

(define (cc amount coin-values)
  (cond [(= amount 0) 1]
        [(or (< amount 0) (no-more? coin-values)) 0]
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(cc 100 us-coins)
;; (cc 125 uk-coins)
;; > 287957

(displayln "exercise 2.20")
(define (same-parity first . rest)
  (define (match? a b)
    (= (remainder a 2) (remainder b 2)))
  (define (iter lst)
    (cond [(null? lst) '()]
          [(match? first (car lst))
           (cons (car lst) (iter (cdr lst)))]
          [else (iter (cdr lst))]))
  (cons first (iter rest)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
    
(displayln "exercise 2.21")
(define (square x) (* x x))
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list (cdr items)))))

(define (mapped-square-list items)
  (map square items))

(square-list (list 1 2 3 4))
(mapped-square-list (list 1 2 3 4))

(displayln "exercise 2.22")
(define (sqr-lst items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              ;;Louis First Attempt
              ;;(sqr-lst 1 2 3 4) results in (1 4 9 16)
              ;;(cons answer (square (car things)))
              
              ;;Second Attempt
              ;;(sqr-lst 1 2 3 4) results in:
              ;;((((() . 1) . 4) . 9) . 16)
              (cons answer (square (car things))))))
  (iter items '()))
(sqr-lst (list 1 2 3 4))

;; The reason why this isn't working is because the object that the objects he's appending
;; together aren't cons cells, but simply numbers. Therefore they are shown to be cons pairs
;; instead of a list, as we'd expect. We can get closer to the result we want by ensuring that
;; we cons the answer to a proper cons cell.
;; (cons answer (cons (square (car things)) '()))
;; Unforunately it means that we are essentially doing the following:
;; (cons (cons '() '(1)) '(4))
;; > '((() 1) 4)
;; Louis could simply build up the results in reverse order then call the reverse procedure on
;; them, but that does mean we are doing extra computation for no good reason. He could also use
;; the append procedure from p. 103, though as the results list grows the function will take longer
;; to run

(displayln "exercise 2.23")
(define (for-each proc items)
  (when (not (null? items))
    (proc (car items))
    (for-each proc (cdr items)))
  #t)

(for-each (lambda (x) (displayln x))
          (list 1 2 3 4 5))

(displayln "exercise 2.24")
(displayln "see exercise-2.24.jpg")

(displayln "exercise 2.25")
(define ex-2-25-a '(1 3 (5 7) 9))
(define ex-2-25-b '((7)))
(define ex-2-25-c '(1 (2 (3 (4 (5 (6 7)))))))

;; 2.25 a
(cadr (caddr ex-2-25-a))
;; aka
(car (cdr (car (cdr (cdr ex-2-25-a)))))

;; 2.25 b
(caar ex-2-25-b)
;; aka
(car (car ex-2-25-b))

;; 2.25 c
(cadr (cadr (cadr (cadr (cadr (cadr ex-2-25-c))))))

(displayln "exercise 2.26")

;; x '(1 2 3)
;; y '(4 5 6)

;; (append x y)
;;>(1 2 3 4 5 6)

;; (cons x y)
;;>((1 2 3) 4 5 6)

;; (list x y)
;;>((1 2 3) (4 5 6))

(displayln "exercise 2.27")
(define (deep-reverse x)
  (define (iter lst acc)
    (cond [(null? lst) acc]
          [(pair? (car lst)) (iter (cdr lst)
                                   (cons (deep-reverse (car lst)) acc))]
          [else (iter (cdr lst) (cons (car lst) acc))]))
  (iter x '()))

(deep-reverse (list (list 1 2) (list 3 4)))

(displayln "exercise 2.28")
(define (fringe lst)
  (cond [(null? lst) '()]
        [(not (pair? lst)) (list lst)]
        [else (append (fringe (car lst))
                      (fringe (cdr lst)))]))
(define ex-2-28-x (list (list 1 2) (list 3 4)))
(fringe ex-2-28-x)
(fringe (list (list ex-2-28-x ex-2-28-x) (list 5 6 7 9 (list 10 11 12))))

(displayln "exercise 2.29")
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define ex-2-29-left-branch (make-branch 5 6))
(define ex-2-29-right-branch (make-branch 2 3))

(displayln "exercise 2.29 a")

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (branch? branch)
  (not (pair? (branch-structure branch))))

(define (branch-weight branch)
  (if (branch? branch)
      (branch-structure branch)
      (total-weight (branch-structure branch))))

(define (branch-torque branch)
  (let [(len (branch-length branch))
        (struct (branch-structure branch))]
    (* len
       (if (branch? branch)
           struct
           (let [(tl (branch-torque (left-branch struct)))
                 (tr (branch-torque (right-branch struct)))]
             (if (= tl tr)
                 (+ tl tr)
                 0))))))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(when (not (= (branch-length ex-2-29-left-branch) 5))
  (error "Branch length did not match"))
(when (not (= (branch-structure ex-2-29-left-branch) 6))
  (error "Branch height did not match"))
(when (not (equal? (branch-structure (make-branch 3 ex-2-29-right-branch)) ex-2-29-right-branch))
  (error "Child branch did not match"))

(displayln "exercise 2.29 b")
(define simple (make-mobile (make-branch 5 5) (make-branch 5 5)))
(define airplanes
  (make-mobile
   (make-branch 5
                (make-mobile
                   (make-branch 5 5)
                   (make-branch 5 5)))
   (make-branch 10
                (make-mobile
                    (make-branch 5
                                 (make-mobile (make-branch 3 2)
                                              (make-branch 5 3)))
                    (make-branch 6 5)))))

(define (weird-mobile)
  (make-mobile
   (make-branch 5 480)
   (make-branch 10
                (make-mobile
                 (make-branch 10 (make-mobile (make-branch 3 2) (make-branch 2 3)))
                 (make-branch 6 20)))))
                 
(define (total-weight mobile)
  (cond [(null? mobile) 0]
        [(branch? mobile) (branch-structure mobile)]
        [else
         (let ([left (left-branch mobile)]
               [right (right-branch mobile)])
           (+ (branch-weight left)
              (branch-weight right)))]))

(total-weight airplanes)

(displayln "exercise 2.29 c")
(define (balanced? mobile)
  (= (branch-torque (left-branch mobile))
     (branch-torque (right-branch mobile))))

(balanced? simple)
(balanced? airplanes)
(balanced? (weird-mobile))

(displayln "exercise 2.29d")
(set! make-mobile (lambda (l r) (cons l r)))
(set! make-branch (lambda (len str) (cons len str)))

;; Changes to make program work
(set! branch-structure (lambda (b) (cdr b)))
(set! right-branch (lambda (b) (cdr b)))

;; tests
(balanced? (weird-mobile))

