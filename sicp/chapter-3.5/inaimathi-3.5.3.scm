;;; The givens
(define (sqrt-stream x)
  (define guesses 
    (cons-stream 
     1.0 
     (stream-map (lambda (guess) (sqrt-improve guess x))
		 guesses)))
  guesses)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
	(s1 (stream-ref s 1)) 
	(s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1)) 
			  (+ s0 (* -2 s1) s2)))
		 (euler-transform (stream-cdr s)))))

;;; 3.63
(define (sqrt-stream x)
  (cons-stream 
   1.0
   (stream-map 
    (lambda (guess) (sqrt-improve guess x))
    (sqrt-stream x))))

;; The call to (sqrt-stream x) in the loosely reasoned version is generating a separate stream, 
;; which will therefore re-do the work of computing each step.
;; If we didn't memoize, both versions would be re-computing things on the way down, so
;; they would no longer differ in efficiency.

;; 3.64
;; given part:
(define (sqrt x tolerance) 
  (stream-limit (sqrt-stream x) tolerance))

(define (stream-limit stream tolerance)
  (define rest (cdr-stream stream))
  (define a (car-stream stream))
  (define b (car-stream rest))
  (if (> tolerance (- (abs a) (abs b)))
      b
      (stream-limit rest tolerance)))

;; 3.76
(define smooth (stream)
  (define rest (cdr-stream (cdr-stream stream)))
  (define a (car-stream stream))
  (define b (car-stream (cdr-stream stream)))
  (cons-stream (/ (+ a b) 2) rest))
