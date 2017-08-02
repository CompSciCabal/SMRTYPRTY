#lang racket

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define simple-tree (cons (cons 2 5)
                          (cons 1 3)))
(define my-tree (cons
                  (cons
                    (cons 4
                          (cons 2 9))
                    2)
                  (cons 6
                        (cons
                          (cons 5
                            (cons 2 4))
                          2))))

#| (count-leaves simple-tree) |#
#| (count-leaves my-tree) |#


;; 2.24
;; (list 1 (list 2 (list 3 4)))
; this is equivilent to
; (cons 1 (cons (list 2 (list 3 4)) empty))
; (cons 1 (cons (cons 2 (cons (list 3 4) empty)) empty))
; (cons 1 (cons (cons 2 (cons (cons 3 (cons 4 empty)) empty)) empty))
; or, formatted:
#| (cons 1 |#
#|       (cons (cons 2 |#
#|                   (cons (cons 3 |#
#|                               (cons 4 |#
#|                                     empty)) |#
#|                         empty)) |#
#|             empty)) |#


;; 2.25
#| (cadr (caddr '(1 3 (5 7) 9))) |#
#| (car (car '((7)))) |#
#| (cadr (cadr (cadr (cadr (cadr (cadr |#
#|                           '(1 (2 (3 (4 (5 (6 7)))))))))))) |#


;; 2.26
#| (define x (list 1 2 3)) |#
#| (define y (list 4 5 6)) |#
#| (append x y) |#
#| (list 1 2 3 4 5 6) |#
#| (cons x y) |#
#| '((1 2 3) 4 5 6) |#
#| (list x y) |#
#| '((1 2 3) (4 5 6)) |#


;; 2.27
(define x (list (list 1 2) (list 3 4)))
(define y (list (list (list 1 2) (list 3 4) (list 5 6))
                (list (list 7 8 9) (list 10 11 12 13 14))
                (list 15 16 17)))
(define (deep-reverse lst)
  ; same idea as before; pop from lst to ans
  (define (deep-reverse-helper lst ans)
    (cond [(empty? lst) ans]
          [(pair? (car lst))
           (deep-reverse-helper (cdr lst)
                                (cons (deep-reverse (car lst)) ans))]
          [else (deep-reverse-helper (cdr lst)
                                     (cons (car lst) ans))]))

  (deep-reverse-helper lst empty))

#| (deep-reverse x) |#
#| (deep-reverse y) |#


;; 2.28

;; joins two lists
(define (join lst1 lst2)
  (cond [(null? lst1) lst2]
        [else (cons (car lst1)
                    (append (cdr lst1) lst2))]))

(define (fringe tree)
  (cond [(empty? tree) empty]
        [(pair? tree) (join (fringe (car tree))
                            (fringe (cadr tree)))]
        [else (list tree)]))

(define z
  (list (list 1 2) (list 3 4)))

(fringe z)
(fringe (list z z))
