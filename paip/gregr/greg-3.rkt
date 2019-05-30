#lang racket
(require racket/pretty)
(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)

;; 3.1
(let* ((x 6)
       (y (* x x)))
  (+ x y))

((lambda (x)
   ((lambda (y)
      (+ x y))
    (* x x)))
 6)

;; 3.2
(cons 1 2)
(list* 1 2)

;; 3.3
(define (print-dotted d)
  (cond ((pair? d) (display "(")
                   (print-dotted (car d))
                   (display " . ")
                   (print-dotted (cdr d))
                   (display ")"))

        (else      (write d))))
(print-dotted '(one (2 3 (4 "five" . 6) 7) 8 nine))
(newline)

;; 3.4
(define (print-nicely d)
  (cond ((pair? d) (display "(")
                   (let print* ((a (car d)) (d (cdr d)))
                     (print-nicely a)
                     (cond ((pair? d) (display " ")   (print* (car d) (cdr d)))
                           ((null? d) (void))
                           (else      (display " . ") (print-nicely d))))
                   (display ")"))

        (else      (write d))))
(print-nicely '(one (2 3 (4 "five" . 6) 7) 8 nine))
(newline)

;; 3.9
;(define (len xs) (foldr (lambda (_ l) (+ l 1)) 0 xs))
(define (len xs) (foldl (lambda (_ l) (+ l 1)) 0 xs))
(len '(1 2 3 4 5))
