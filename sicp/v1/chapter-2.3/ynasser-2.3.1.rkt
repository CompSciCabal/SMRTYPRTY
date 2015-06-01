#lang racket

;; 2.53
;(list 'a 'b 'c) => '(a b c)
;(list (list 'george)) => '((george))
;(cdr '((x1 x2) (y1 y2))) =>'((y1 y2))
;(cadr '((x1 x2) (y1 y2))) => '(y1 y2)
;(pair? (car '(a short list))) => #f
;(memq 'red '((red shoes) (blue socks))) => #f
;(memq 'red '(red shoes blue socks)) => '(red shoes blue socks)

;; 2.54
;; implement equal? such that
;; (equal? '(this is a list) '(this (is a) list) ) returns true

(define (ewqual? lst1 lst2)
  (cond
    [(eq? lst1 lst2) #t]
    [(ormap empty? (list lst1 lst2)) #f]
    [(and (andmap pair? (list lst1 lst2))
          (ewqual? (car lst1) (car lst2))
          (ewqual? (cdr lst1) (cdr lst2)))
     #t]
    [else #f]))

;; Exercise 2.55
;; (car ''abracadabra) => 'quote
;;
;; The interpreter returns 'quote instead of just ' because
;; it is differentiating between the quote which creates a symbol
;; and that which is within the symbol.


