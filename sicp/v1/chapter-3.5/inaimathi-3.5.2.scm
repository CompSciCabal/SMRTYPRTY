;;; 3.53
;; The doubling stream

;;; 3.54
(define (stream-map proc . argstreams)
  ;; I think this was the answer to an earlier question too, but my notes don't have anything about it.
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map (cons proc (map stream-car argstreams))))))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

;;; 3.55
(define (stream-fold proc start s)
  (let ((next (proc start (stream-car s))))
    (stream-cons next (stream-fold proc next (stream-cdr s)))))

(define (partial-sums s)
  (stream-fold + 0 s))
