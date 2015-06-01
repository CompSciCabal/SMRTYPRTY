#lang plai/collector


; Improve the trivial collector to perform a mark-and-sweep collection
; To avoid worrying about fragmentation issues primitives are promoted to
; the same size as cons cells

(define heap-ptr 'uninitialized-heap-ptr)

(define (cell:width)
  3)

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

(define (cell:first cell)
  (gc:first cell))

(define (cell:rest cell)
  (gc:rest cell))

(define (free:init)
  (let ([real-heap-size (- (heap-size) (remainder (heap-size) (cell:width)))])
    (define (init-iter cell)
      (let ([next (+ (cell:width) cell)])
        (if (= next real-heap-size)
            (cell:set! cell 'free null null)
            (begin
              (cell:set! cell 'free next null)
              (init-iter next)))))
    (init-iter heap-ptr)))

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
  (let* ([real-heap-size (- (heap-size) (remainder (heap-size) (cell:width)))]
        [last-cell (- real-heap-size (cell:width))]
        [first-cell 0])
    (define (sweep-cell cell)
      (if (< cell first-cell)
          null
          (begin
            (cond
              [(eq? (cell:tag cell) 'live:prim) (cell:set-tag! cell 'prim)]
              [(eq? (cell:tag cell) 'live:cons) (cell:set-tag! cell 'cons)]
              [else (free:push cell)])
            (sweep-cell (- cell (cell:width))))))
    (begin
      (set! heap-ptr null)
      (sweep-cell last-cell))))

(define (mark-and-sweep roots)
  (define (root-addresses roots)
    (map read-root roots))
  (begin
    (map mark (root-addresses roots))
    (sweep)))

(define (init-allocator)
  ; calling heap-offset before init-allocator is called gives 'undefined
  ; need to be able to at least store 1 cell
  (if (< (heap-size) (cell:width))
      null
      (begin
        (set! heap-ptr 0)
        (free:init))))

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
        (mark-and-sweep (get-roots tag f r))
        (when (not (free:available?))
          (error "out of memory"))
        (cell:set! (free:pop) tag f r))))
  
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

