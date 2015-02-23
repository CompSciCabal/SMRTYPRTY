#lang plai/mutator
(allocator-setup "../good-collectors/stop_and_copy_collector.rkt" 45)

(define (gen-circular)
  (let ([x (cons 3 4)])
    (let ([y (cons 2 x)])
      (set-rest! x y)
      x)))

(define x (gen-circular))
(test/location=? x (rest (rest x)))

(define (new-func)
  (let ([y (cons 7 2)])
    (* (first y) (rest y))))
(new-func)
