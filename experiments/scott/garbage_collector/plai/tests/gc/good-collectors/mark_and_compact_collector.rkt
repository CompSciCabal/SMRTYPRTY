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
; 
; Pass 2: Scan across updating references in cons cells to the new ones
; Pass 3: Scan across shifting all of the live cells over

; Maybe I can use Table based compaction...
; B. K. Haddon and W. M. Waite (August 1967). "A compaction procedure for variable length storage elements"

; Man table compaction is terrible, I'm going to use the extra storage for the forwarding pointer
; and be done with it

(define heap-ptr 'uninitialized-heap-ptr)

(define (cell:prim:width) 3)
(define (cell:cons:width) 4)

(define (cell:type cell)
  (cell:tag:type (cell:tag cell)))

(define (cell:type:width type)
  (cond
    [(eq? type 'prim) (cell:prim:width)]
    [(eq? type 'cons) (cell:cons:width)]))

(define (cell:tag:width tag)
  (cell:type:width (cell:tag:type tag)))

(define (cell:tag:type tag)
  (cond 
    [(eq? tag 'prim) 'prim]
    [(eq? tag 'live:prim) 'prim]
    [(eq? tag 'free:prim) 'prim]
    [(eq? tag 'cons) 'cons]
    [(eq? tag 'live:cons) 'cons]
    [(eq? tag 'free:cons) 'cons]
    [else (error "Unknown cell:type")]))

(define (cell:width cell)
  (cell:tag:width (cell:tag cell)))

(define (cell:prim:set! cell p)
  (begin
    (heap-set! cell 'prim)
    (heap-set! (+ 1 cell) p)
    cell))

(define (cell:cons:set! cell f r)
  (begin
    (heap-set! cell 'cons)
    (heap-set! (+ 1 cell) f)
    (heap-set! (+ 2 cell) r)
    cell))

(define (cell:set-tag! cell tag)
  (begin 
    (heap-set! cell tag)
    cell))

(define (cell:tag cell)
  (heap-ref cell))

(define (cell:prim:set-forward! cell forward)
  (heap-set! (+ 2 cell) forward))

(define (cell:cons:set-forward! cell forward)
  (heap-set! (+ 3 cell) forward))

(define (cell:set-forward! cell forward)
  (let ([type (cell:type cell)])
    (begin
      (cond
        [(eq? type 'prim) (cell:prim:set-forward! cell forward)]
        [(eq? type 'cons) (cell:cons:set-forward! cell forward)])
      cell)))

(define (cell:prim:forward cell)
  (heap-ref (+ 2 cell)))

(define (cell:cons:forward cell)
  (heap-ref (+ 3 cell)))

(define (cell:forward cell)
  (if (eq? cell null)
      null
      (let ([type (cell:type cell)])
        (cond
          [(eq? type 'prim) (cell:prim:forward cell)]
          [(eq? type 'cons) (cell:cons:forward cell)]))))

(define (cell:set-free! cell)
  (let ([type (cell:type cell)])
    (cond 
      [(eq? type 'prim) (begin
                          (cell:prim:set! cell null)
                          (cell:set-tag! cell 'free:prim)
                          (cell:set-forward! cell null))]
      [(eq? type 'cons) (begin
                          (cell:cons:set! cell null null)
                          (cell:set-tag! cell 'free:cons)
                          (cell:set-forward! cell null))])))

(define (cell:deref cell)
  (gc:deref cell))

(define (cell:first cell)
  (gc:first cell))

(define (cell:rest cell)
  (gc:rest cell))

(define (mem:init start)
  (define (zero-mem-iter cell) 
    (if (= cell (heap-size))
        null
        (begin 
          (heap-set! cell null)
          (zero-mem-iter (+ 1 cell)))))
  (begin
    (set! heap-ptr start)
    (zero-mem-iter start)))

(define (mem:last? cell)
  (= cell heap-ptr))

(define (mem:available? size)
  (<= (+ heap-ptr size) (heap-size)))

(define (mem:allocate size)
  (let ([cell heap-ptr])
    (begin
      (set! heap-ptr (+ heap-ptr size))
      cell)))

(define (mem:free cell)
    (cell:set-free! cell))

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
  (define (next-cell cell)
    (+ cell (cell:width cell)))
  (define (update-cell cell shift)
    (begin 
      (cell:set-forward! cell (- cell shift))
      (sweep-cell (next-cell cell) shift)))
  (define (update-shift cell shift)
    (begin
      (mem:free cell)
      (sweep-cell (next-cell cell) (+ shift (cell:width cell)))))
  (define (sweep-cell cell shift)
    (if (mem:last? cell)
        null
        (begin
          (cond
            [(eq? (cell:tag cell) 'live:prim) (update-cell cell shift)]
            [(eq? (cell:tag cell) 'live:cons) (update-cell cell shift)]
            [else (update-shift cell shift)]))))
  (sweep-cell 0 0))

(define (update)
  (define (update-references cell)
    (let ([tag (cell:tag cell)]
          [first (cell:first cell)]
          [rest (cell:rest cell)])
      (if (eq? tag 'live:cons)
          (let ([new-first (cell:forward first)]
                [new-rest (cell:forward rest)])
            (begin
              (cell:cons:set! cell new-first new-rest)
              (cell:set-tag! cell 'live:cons)))
          null)))
  (define (update-cell-iter cell)
    (if (mem:last? cell)
        null
        (begin
          (update-references cell)
          (update-cell-iter (+ cell (cell:width cell))))))
  (update-cell-iter 0))

(define (compact)
  (define (shift-prim-cell cell forward)
    (let ([width (cell:width cell)])
      (begin
        (cell:prim:set! forward (cell:deref cell))
        (cell:set-forward! forward null)
        (shift-cell-iter (+ cell width) (+ forward width)))))
  (define (shift-cons-cell cell forward)
    (let ([width (cell:width cell)])
      (begin
        (cell:cons:set! forward (cell:first cell) (cell:rest cell))
        (cell:set-forward! forward null)
        (shift-cell-iter (+ cell width) (+ forward width)))))
  (define (shift-cell-iter cell new-heap-ptr)
    (if (mem:last? cell)
        new-heap-ptr
        (let ([tag (cell:tag cell)])
          (begin
            (cond 
              [(eq? tag 'live:prim) (shift-prim-cell cell (cell:forward cell))]
              [(eq? tag 'live:cons) (shift-cons-cell cell (cell:forward cell))]
              [else (shift-cell-iter (+ cell (cell:width cell)) new-heap-ptr)])))))
    (mem:init (shift-cell-iter 0 0)))



(define (mark-and-compact roots live-refs)
  (define (root-addresses roots)
    (map read-root roots))
  (define (update-root root)
    (let ([new-addr (cell:forward (read-root root))])
      (set-root! root new-addr)))
  (define (update-references refs)
    (if (eq? refs null)
        null
        (cons (cell:forward (car refs)) (cell:forward (cdr refs)))))
  (begin
    (map mark (root-addresses roots))
    (sweep)
    (update)
    (map update-root roots)
    (let ([new-refs (update-references live-refs)])
      (begin 
        (compact)
        new-refs))))

(define (init-allocator)
  ; calling heap-offset before init-allocator is called gives 'undefined
  ; need to be able to at least store 1 cell
  (mem:init 0))

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
  
(define (gc:alloc-flat p)
  (let ([size (cell:type:width 'prim)])
    (if (mem:available? size)
        (cell:prim:set! (mem:allocate size) p)
        (begin
          (mark-and-compact (get-roots 'prim p null) null)
          (when (not (mem:available? size))
            (error "out of memory"))
          (cell:prim:set! (mem:allocate size) p)))))

(define (gc:cons f r)
  (let ([size (cell:type:width 'cons)])
    (if (mem:available? size)
        (cell:cons:set! (mem:allocate size) f r)
          (let ([new-refs (mark-and-compact (get-roots 'cons f r) (cons f r))])
            (begin
              (when (not (mem:available? size))
                (error "out of memory"))
              (cell:cons:set! (mem:allocate size) (car new-refs) (cdr new-refs)))))))

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

