#lang racket
; Question
; Exercise 3.8.  When we defined the evaluation model in section 1.1.3,
; we said that the first step in evaluating an expression is to evaluate
; its subexpressions. But we never specified the order in which the 
; subexpressions should be evaluated (e.g., left to right or right to left). 
; When we introduce assignment, the order in which the arguments to a procedure 
; are evaluated can make a difference to the result. Define a simple procedure 
; f such that evaluating (+ (f 0) (f 1)) will return 0 if the arguments to + 
; are evaluated from left to right but will return 1 if the arguments are 
; evaluated from right to left.
;
; Remarks
; If f has not been called before, return the number f has been called with
; If f has been called before, return zero
;
; Use make-monitored to wrap a function that accepts the symbols the ints 1 and
; zero. make-monitored dispatches the correct return value based on if 

; Wrap the ident function with a make-monitored variant. Make-monitored dispatches
; the correct value based on the number of times the function has been called and 
; what it has been called with

; 
;
; Surprisingly hard for me to answer -- perhaps because of the two week gap between 
; the sections. Wrong first attempts:

(define (f number)
  (let ((called? #f))
    (begin 
      (if called? 0
          number)
      (set! called? #t))))

(define (g num)
  (let ((calls 0))
    (if (= calls 0) num
        (begin (set! calls (+ calls 1))
               0))))

(define (h num)
  (let ((call-counter 0))
    (define (inner)
      (set! call-counter (+ call-counter 1))
      (if (= call-counter 1) num
          0))(inner)))
  
;
; After these, decided to take a break, do dishes, have dinner, go dancing at The
; Steady. I suspected there was a way to do it with make-monitored. Next morning I 
; could see the outline on waking, after we got back from some errands, this was 
; fairly simple to implement:

(define (make-monitored watch-func)
  (let ((call-counter 0))
    (define (dispatch number)
      (cond ((= call-counter 0) 
             (begin (set! call-counter (+ call-counter 1))
                    (watch-func number)))
            (else (begin (set! call-counter (+ call-counter 1))
                         (watch-func 0))))
      )dispatch
    )
  )
(define (ident x) x)

(define f (make-monitored ident))

; I don't know if this really qualifies as a 'simple' procedure. It does reveal
; that racket evals left to right, but it only does so on the first execution. 


