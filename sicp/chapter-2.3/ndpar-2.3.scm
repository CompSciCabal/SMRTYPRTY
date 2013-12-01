#lang racket

;; -------------------------------------------------------------------
;; Symbolic Data
;; -------------------------------------------------------------------

;; Exercise 2.53, p.144
(list 'a 'b 'c) ;=> '(a b c)
(list (list 'george)) ;=> '((george))
(cdr '((x1 x2) (y1 y2))) ;=> '((y1 y2))
(cadr '((x1 x2) (y1 y2))) ;=> '(y1 y2)
(pair? (car '(a short list))) ;=> #f
(memq 'red '((red shoes) (blue socks))) ;=> #f
(memq 'red '(red shoes blue socks)) ;=> '(red shoes blue socks)

;; Exercise 2.54, p.145
;; Symbol equality (same as built-in equal?)
(define (my-equal? x y)
  (cond ((and (null? x) (null? y)) #t)
        ((and (pair? x) (pair? y))
         (and (my-equal? (car x) (car y))
              (my-equal? (cdr x) (cdr y))))
        ((and (symbol? x) (symbol? y))
         (eq? x y)) (else false)))

;; Tests
(my-equal? '(this is a list) '(this is a list))
(not (my-equal? '(this is a list) '(this (is a) list)))

;; Exercise 2.55, p.145
;; Double quotation
(eq? (car ''abracadabra)
     (car (quote (quote abracadabra))))
