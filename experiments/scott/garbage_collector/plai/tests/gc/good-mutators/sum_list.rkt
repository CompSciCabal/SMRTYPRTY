#lang plai/mutator

(allocator-setup "../good-collectors/mark_and_sweep_collector.rkt" 30)

(define (sum-list L)
  (if (empty? L)
      0
      (+ (first L) (sum-list (rest L)))))
(sum-list '(1 2 3))
