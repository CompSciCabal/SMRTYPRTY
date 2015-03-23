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