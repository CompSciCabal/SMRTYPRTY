#lang plai/mutator

(allocator-setup "../good-collectors/stop_and_copy_collector.rkt" 50)

(define (sum-list L)
  (if (empty? L)
      0
      (+ (first L) (sum-list (rest L)))))
(sum-list '(1 2 3))
