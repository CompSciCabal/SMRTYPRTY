#lang plai/mutator
;(allocator-setup "../good-collectors/stop_and_copy_collector.rkt" 45)
(allocator-setup "../good-collectors/mark_and_sweep_collector.rkt" 30)
(define (gen-circular)
  (let ([x (cons 3 4)])
    (let ([y (cons 2 x)])
      (set-rest! x y)
      x)))
'junk

(define x (gen-circular))
(test/location=? x (rest (rest x)))

'garbage
(define (new-func)
  (let ([y (cons 7 2)])
    (* (first y) (rest y))))
(new-func)
