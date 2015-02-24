#lang plai/collector


; Improve the trivial collector to perform a mark-and-sweep collection
; Alternative approach to dealing with fragmentation is to compact the memory
; after each sweep

; Compaction is tricky as you must update the references, however we can take 
; advantage of the linear layout of memory to know by how much to offset all
; references

; Pass 1: Scan across adding up the free space
; This can give us an offset for each range of remaining live cells
; e.g.
; If x is live data and o is garbage
;    xxoooxxxxooxxx
; L: 0    5     11
; G:   2      9
; 
; N: 01   2345  678
; F:   f2     f5
; 
; Pass 2: Scan across updating references in cons cells to the new ones
; Pass 3: Scan across shifting all of the live cells over

; Maybe I can use Table based compaction...
; B. K. Haddon and W. M. Waite (August 1967). "A compaction procedure for variable length storage elements"

; Man compaction is terrible, I'm going to use the extra storage for the forwarding pointer
; and be done with it

; I've again simplified to use single size objects and promoted the primitives
; However, I've tried to not use the much simpler compaction scheme that can take 
; advantage of the single sizes and I do the full 3 scans to adjust the positions of 
; the objects, so it should be extensible to variable sized objects

; Not super happy with this one, it got too fiddly

(define heap-ptr 'uninitialized-heap-ptr)

(define (cell:width)
  4)

(define (cell:set! cell tag f r)
  (let ([addr cell]) 
    (begin
      (heap-set! addr tag)
      (heap-set! (+ 1 addr) f)
      (heap-set! (+ 2 addr) r)
      addr)))

(define (cell:set-tag! cell tag)
  (begin 
    (heap-set! cell tag)
    cell))

(define (cell:tag cell)
  (heap-ref cell))

(define (cell:set-forward! cell forward)
  (begin
    (heap-set! (+ 3 cell) forward)
    cell))

(define (cell:forward cell)
  (heap-ref (+ 3 cell)))

(define (cell:first cell)
  (gc:first cell))

(define (cell:rest cell)
  (gc:rest cell))

(define (free:init start)
  (let ([real-heap-size (- (heap-size) (remainder (heap-size) (cell:width)))])
    (define (init-iter cell)
      (let ([next (+ (cell:width) cell)])
        (if (= next real-heap-size)
            (cell:set! cell 'free null null)
            (begin
              (cell:set! cell 'free next null)
              (init-iter next)))))
    (init-iter start)))

(define (free:available?)
  (not (eq? heap-ptr null)))

(define (free:pop)
  (let ([cell heap-ptr])
    (begin
      (set! heap-ptr (cell:first cell))
      cell)))

(define (free:push cell)
  (begin
    (cell:set! cell 'free heap-ptr null)
    (set! heap-ptr cell)))

(define (mark cell)
  (define (mark-primitive cell)
    (cell:set-tag! cell 'live:prim))
  (define (mark-cons cell)
    (begin 
      (cell:set-tag! cell 'live:cons)
      (mark (cell:first cell))
      (mark (cell:rest cell))
      cell))
  (let ([tag (cell:tag cell)])
    (cond
      [(eq? tag 'prim) (mark-primitive cell)]
      [(eq? tag 'cons) (mark-cons cell)])))

(define (sweep)
  (let* ([real-heap-size (- (heap-size) (remainder (heap-size) (cell:width)))])
    (define (update-cell cell shift)
      (begin 
        (cell:set-forward! cell (- cell shift))
        (sweep-cell (+ cell (cell:width)) shift)))
    (define (update-shift cell shift)
      (begin
        (free:push cell)
        (sweep-cell (+ cell (cell:width)) (+ shift (cell:width)))))
    (define (sweep-cell cell shift)
      (if (= cell real-heap-size)
          null
          (begin
            (cond
              [(eq? (cell:tag cell) 'live:prim) (update-cell cell shift)]
              [(eq? (cell:tag cell) 'live:cons) (update-cell cell shift)]
              [else (update-shift cell shift)]))))
    (begin
      (set! heap-ptr null)
      (sweep-cell 0 0))))

(define (compact)
  (let ([real-heap-size (- (heap-size) (remainder (heap-size) (cell:width)))]
        [last-forward 0]) 
    (define (update-cell cell)
      (let ([tag (cell:tag cell)]
            [first (cell:first cell)]
            [rest (cell:rest cell)])
        (if (eq? tag 'live:cons)
            (let ([new-first (cell:forward first)]
                  [new-rest (cell:forward rest)])
              (cell:set! cell tag new-first new-rest))
            null)))
    (define (update-cell-iter cell)
      (if (= cell real-heap-size)
          null
          (begin 
            (update-cell cell)
            (update-cell-iter (+ cell (cell:width))))))
    (define (shift-cell cell tag forward)
      (begin 
        (cell:set! forward tag (cell:first cell) (cell:rest cell))
        (set! last-forward forward)))
    (define (shift-cell-iter cell)
      (if (= cell real-heap-size)
          null
          (let ([tag (cell:tag cell)])
            (begin
              (cond 
                [(eq? tag 'live:prim) (shift-cell cell 'prim (cell:forward cell))]
                [(eq? tag 'live:cons) (shift-cell cell 'cons (cell:forward cell))])
              (shift-cell-iter (+ cell (cell:width)))))))
    (begin
      (update-cell-iter 0)
      (shift-cell-iter 0)
      (let ([free-start (+ (cell:width) last-forward)])
        (if (> (+ free-start (cell:width)) (heap-size))
            (set! heap-ptr null)
            (begin 
              (set! heap-ptr (+ (cell:width) last-forward))
              (free:init (+ (cell:width) last-forward))))))))

(define (update-root root)
  (let ([new-addr (cell:forward (read-root root))])
    (set-root! root new-addr)))

(define (mark-and-compact roots)
  (define (root-addresses roots)
    (map read-root roots))
  (begin
    (map mark (root-addresses roots))
    (sweep)
    (compact)
    (map update-root roots)))

(define (init-allocator)
  ; calling heap-offset before init-allocator is called gives 'undefined
  ; need to be able to at least store 1 cell
  (if (< (heap-size) (cell:width))
      null
      (begin
        (set! heap-ptr 0)
        (free:init heap-ptr))))

(define (get-roots tag f r)
  (define (get-primitive-roots p)
    (if (procedure? p)
        (append (procedure-roots p) (get-root-set))
        (get-root-set)))
  (define (get-cons-roots f r)
    (get-root-set f r))
  (cond 
    [(eq? tag 'prim) (get-primitive-roots f)]
    [(eq? tag 'cons) (get-cons-roots f r)]))
  
(define (alloc tag f r)
  (if (free:available?)
      (cell:set! (free:pop) tag f r)
      (begin 
        (mark-and-compact (get-roots tag f r))
        (when (not (free:available?))
          (error "out of memory"))
        (if (eq? tag 'cons)
            (cell:set! (free:pop) tag (cell:forward f) (cell:forward r))
            (cell:set! (free:pop) tag f r)))))
  
(define (gc:alloc-flat p)
  (alloc 'prim p null))

(define (gc:cons f r)
  (alloc 'cons f r))

(define (gc:deref a)
  (heap-ref (+ 1 a)))

; number -> boolean
(define (gc:cons? a)
  (eq? (heap-ref a) 'cons))

; number -> any
(define (gc:first a)
  (heap-ref (+ 1 a)))

; number -> number
(define (gc:rest a)
  (heap-ref (+ 2 a)))

(define (gc:set-first! a f)
  (if (gc:cons? a)
      (heap-set! (+ 1 a) f)
      (error 'set-first! "expects address of cons")))

(define (gc:set-rest! a r)
  (heap-set! (+ 2 a) r))

; function number -> boolean
(define (gc:flat? a)
  (eq? 'prim (heap-ref a)))

