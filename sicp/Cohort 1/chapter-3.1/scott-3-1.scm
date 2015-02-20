;; exercise 3.8

(define f
  (let ((var -1))
    (lambda (x)
      (display var)
      (newline)
      (display x)
      (newline)
      (if (= var -1)
        (begin (set! var (/ x 2)) var)
        var))))
(+ (f 0) (f 1))
(+ (f 1) (f 0))
