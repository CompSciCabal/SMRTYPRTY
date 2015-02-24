#lang plai/collector

; Simple extension to the trivial collector so it works better
; Split memory in 2
; if an alloc fails 
;   trace roots and copy all live references into  new locations in the other half

; Advantage is I can keep using the linear allocation layout below
; but get a more functional collector
; Downside is you lose half your memory :(


; Some subtlety to the copying
; - When a value is copied, modify its tag to 'copied and store the new-addr
; - When copying cons cells, we need to split allocation and setting the new 
;   pointer addresses to account for circular references

(define (copy-live addr)
  (define (copy-primitive addr)
    (let ([new-addr (gc:alloc-flat (gc:deref addr))])
      (begin
        (heap-set! addr 'copied)
        (heap-set! (+ 1 addr) new-addr)
        new-addr)))
  
  (define (copy-cons addr)
    (let ([first-addr (gc:first addr)]
          [rest-addr (gc:rest addr)]
          [new-addr (gc:cons 'unset 'unset)])
      (begin
        (heap-set! addr 'copied)
        (heap-set! (+ 1 addr) new-addr)
        (let ([new-first (copy-live first-addr)]
              [new-rest (copy-live rest-addr)])
          (begin 
            (heap-set! (+ 1 new-addr) new-first)
            (heap-set! (+ 2 new-addr) new-rest))))
      new-addr))
  
  (let ([tag (heap-ref addr)])
    (cond
      [(eq? tag 'copied) (gc:deref addr)]
      [(eq? tag 'prim) (copy-primitive addr)]
      [(eq? tag 'cons) (copy-cons addr)])))

(define (swap-and-copy)
  (if (= effective-heap-size (heap-size))
      (begin 
        (set! heap-ptr 0)
        (set! effective-heap-size (/ (heap-size) 2)))
      (begin 
        (set! heap-ptr (/ (heap-size) 2))
        (set! effective-heap-size (heap-size))))
  (define (copy-root root)
    (let ([new-addr (copy-live (read-root root))])
      (set-root! root new-addr)))
  (map copy-root (get-root-set)))



(define heap-ptr 'uninitialized-heap-ptr)
(define effective-heap-size 'uninitialized-heap-size)
(define (init-allocator)
  ; calling heap-offset before init-allocator is called gives 'undefined
  (set! heap-ptr 0)
  (set! effective-heap-size (/ (heap-size) 2)))

(define (gc:alloc-flat p)
  (begin
    (when (> (+ heap-ptr 2) effective-heap-size)
      (swap-and-copy))
    (when (> (+ heap-ptr 2) effective-heap-size)
      (error "out of memory"))
    (heap-set! heap-ptr 'prim)
    (heap-set! (+ 1 heap-ptr) p)
    (set! heap-ptr (+ 2 heap-ptr))
    ; return the location of this flat data
    (- heap-ptr 2)))

(define (gc:cons f r)
  (begin
    (when (> (+ heap-ptr 3) effective-heap-size)
      (swap-and-copy))
    (when (> (+ heap-ptr 3) effective-heap-size)
      (error "out of memory"))
    (heap-set! heap-ptr 'cons)
    (heap-set! (+ 1 heap-ptr) f)
    (heap-set! (+ 2 heap-ptr) r)
    (set! heap-ptr (+ 3 heap-ptr))
    (- heap-ptr 3)))

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

