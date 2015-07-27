#lang racket

;;; Implementation of the scheme interpreter from Chapter 4 of sicp
;;; Done in racket instead of scheme, I've basically combined the 
;;; approach in sicp with the 100 lines minischeme from Matt Might:
;;; http://matt.might.net/articles/implementing-a-programming-language/

;;; Still a work in progress, but its functional.  It will be nice to
;;; add more primitives, let statements, and the and/or operations.

;;; Predicates to test for different types of things
(define (self-evaluating? exp)
  (or (number? exp) (string? exp) (boolean? exp)))

(define variable? symbol?)

; eval: unlike in sicp we keep some of the language here in eval
;       the first list element is the symbol to match 
;       then however many arguments are expected get matched
; The quasiquotes make for really concise code, but took me a while 
; to get used to.  The key for those unfamiliar, is that ` starts 
; something like a regular quote and each comma , will escape the 
; next datum it reads.
; So  `(0 ,(+ 1 2) (+ 3 4)) -> (0 3 (+ 3 4))
; but `(0 ,(+ 1 2) ,(+ 3 4)) -> (0 3 7)
; It makes some of these rules super concise which is nice.
; The application matching also confused me, the single dot isn't
; described on the pattern matching page of the docs, rather it is
; the dot notation for cons cells.
; the f matches the first list element as a datum, then the dot is quoted, then
; the args match the rest which could be any number of args
; so `(,f ., args) 
; would match (1 2 3) as f = 1, args = (2 3)
; Note also the order of the match is important, as the last function application
; match would match any of the previous matches.
(define (eval exp env)
  (match exp
    [(? self-evaluating?) exp]
    [(? variable?) (lookup-variable-value env exp)]
    [`(quote, e) e]
    [`(define , var, e) (eval-definition env var e)]
    [`(set! , var, e) (eval-assignment env var e)]
    [`(if, pred, et, ef) (eval-if env pred et ef)] 
    [`(lambda, params, body) `(closure, exp, env)]
    [`(begin, actions ...) (eval-sequence env actions)]
    [`(cond, exps ...) (eval-cond env exps)]
    [`(,f ., args) (apply-proc (eval f env)
                          (list-of-values args env))]
    [_           "unmatched"]))


; evaluate all args, this determines our order of evaluation
; I'm using map, though in sicp they don't
(define (list-of-values args env)
  (define (eval-with exp) 
    (eval exp env))
  (map eval-with args))

; applying the procedure, we name it apply-proc to not clash with the racket base apply
(define (apply-proc proc args)
  (match proc
    [`(closure (lambda, params, body) , env) (eval body (extend-env* env params args))]
    [`(primitive, p) (apply p args)]
    [_ "Error: Unknown proc type"]))

(define (eval-if env pred et ef)
  (if (eval pred env)
      (eval et env)
      (eval ef env)))

; just noticed we never eval var, so it must be a symbol but we don't check for it
; yeah this is a bit weird, e.g. (define (define a 5) 6) slips through
(define (eval-assignment env var exp)
  (let ([val (eval exp env)])
    (set-variable-value! env var val)))
 
; disallowing redefinition, just to make assignment and definition different
(define (eval-definition env var exp)
  (let ([val (eval exp env)])
    (define-variable! env var val)))

; Eval sequence doesn't need to check if actions is null, because 
; the begin clause won't match in the eval pattern matching
(define (eval-sequence env actions)
  (match actions
    [`(,last) (eval last env)]
    [`(,next, rest ...) (begin 
                    (eval next env)
                    (eval-sequence env rest))]))

;; In sicp they implement cond in terms of if, I'm not doing that.
;; Hmm, the else keyword basically gets defined here in the middle 
;; of the source.  Thats not ideal.  
(define (eval-cond env exps)
  (match exps
    ['() "Error: cond expression bottomed out with no match"]
    [`(else, exp) (eval exp env)]
    [`((,pred, exp), rest ...) (if (eval pred env)
                                   (eval exp env)
                                   (eval-cond env rest))]))

;;; Environment, implementing it as hashtables

;; TODO:: We only insert symbols in the environment so it is safe to use a string 
;         to identify the parent env.
;         Only the root env will have '() as its parent environment
(define (enclosing-env env) 
  (hash-ref env "parent:environment" '()))


;; TODO:: Need to raise errors
(define (lookup-variable-value env var)
  (match env
    ['() "Error: lookup-variable -> Variable Undefined"]
    [(? hash?) (hash-ref env var (lookup-variable-value (enclosing-env env) var))]))

;; define doesn't care what was defined in any parent environments, it just tries to
;  define it in the current environment.  Doesn't overwrite.
(define (define-variable! env var val)
  (if (hash-has-key? env var)
      "Error: define-variable -> Variable already defined"
      (hash-set! env var val)))

;; set variable will traverse back up the environment tree until it finds the variable
;  its an error if it can't be found.
(define (set-variable-value! env var val)
  (match env
    ['() "Error: set-variable -> Variable Undefinied"]
    [(? hash?) (if (hash-has-key? env var)
                   (hash-set! env var val)
                   (set-variable-value! (enclosing-env env) var val))]))

;; Makes a new environment and inserts args-values into it, then sets env
;; to the parent of the returned env
;; TODO:: I don't like the imperative way I did this.
(define (extend-env* env args values)
  (begin
    (define child-env (make-hash))
    (for ([arg args]
          [value values])
      (hash-set! child-env arg value))
    (hash-set! child-env "parent:environment" env)
    child-env))

;; Extend an initial environment '() to have primitives
(define (env-initial)
  (extend-env* 
   '()
   '(+  -  /  *  <=  void  display  newline)
   (map (lambda (s) (list 'primitive s))
   `(,+ ,- ,/ ,* ,<= ,void ,display ,newline))))

; The environment definition could be moved inside the driver loop
; but this is convenient to check things afterward
(define the-global-environment (env-initial))

(define (print-input-prompt) 
  (printf "S-Eval >"))

;; TODO:: modify this, so that it doesn't print out the env associated with every lambda
(define (print-output output) 
  (printf "S-Eval >> ~a~n" output))
  
;;; The driver-loop will handle input until it hits eof
;;; I don't like the boolean input thing but didn't find a more elegant way.
(define (driver-loop)
  (begin
    (print-input-prompt)
    (define continue #t)
    (let ([input (read)])
      (if (eof-object? input)
          (set! continue #f)
          (let ([output (eval input the-global-environment)])
            (print-output output))))
    (if continue
        (driver-loop)
        (printf "~nGoodbye Friend!"))))