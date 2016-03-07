(define (halve x) (/ x 2))

(define (double x) (* x 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (mul a b)
	(cond ((= b 0) 0)
		((even? b) (double (mul a (halve b))))
		(else (+ a (mul a (- b 1))))))
