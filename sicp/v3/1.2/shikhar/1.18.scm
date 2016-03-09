(define (halve x) (/ x 2))

(define (double x) (* x 2))

(define (even? n)
  (= (remainder n 2) 0))

(define (mul-iter a b acc)
	(cond ((= b 0) acc)
		((even? b) (mul-iter (double a) (halve b) acc))
		(else (mul-iter a (- b 1) (+ acc a)))))

(define (mul a b)
	(mul-iter a b 0))
