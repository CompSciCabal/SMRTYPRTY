#lang racket

(define (random-elt xs) (list-ref xs (random (length xs))))

;; Basic solution
(define (one-of xs)     (list (random-elt xs)))
(define (sentence)    (append (noun-phrase) (verb-phrase)))
(define (noun-phrase) (append (Article) (Adj*) (Noun) (PP*)))
(define (verb-phrase) (append (Verb) (noun-phrase)))
(define (Article)     (one-of '(the a)))
(define (Noun)        (one-of '(man ball woman table)))
(define (Verb)        (one-of '(hit took saw liked)))

(define (PP)          (append (Prep) (noun-phrase)))
(define (Adj)         (one-of '(big little blue green adiabatic)))
(define (Prep)        (one-of '(to in by with on)))

(define (Adj*)
  (if (= (random 3) 0)
    (append (Adj) (Adj*))
    '()))

(define (PP*)
  (if (= (random 3) 0)
    (append (PP) (PP*))
    '()))

;; Rule-based solution
(define simple-grammar
  '((sentence    -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article     -> the a)
    (Noun        -> man ball woman table)
    (Verb        -> hit took saw liked)))

(define *grammar* simple-grammar)

(define (rule-lhs rule)     (car rule))
(define (rule-rhs rule)     (cddr rule))
(define (rewrites category)
  ;(define kv (assoc category *grammar*))
  ;(and kv (rule-rhs kv))
  (rule-rhs (or (assoc category *grammar*)
                (error "invalid non-terminal:" category))))

;(define (generate phrase)
  ;(cond ((list? phrase)                     (append* (map generate phrase)))
        ;((rewrites phrase) => (lambda (rws) (generate (random-elt rws))))
        ;(else                               (list phrase))))

;; Assume non-terminals appear in nested lists, and terminals do not.
(define (generate phrase)
  (cond ((list? phrase)       (append* (map generate phrase)))
        ((rewrites phrase) => (lambda (rws)
                                (define elt (random-elt rws))
                                (if (list? elt)
                                  (generate elt)
                                  (list elt))))
        (else                 (list phrase))))


(define bigger-grammar
  '((sentence    -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP*         -> () (PP PP*))
    (Adj*        -> () (Adj Adj*))
    (PP          -> (Prep noun-phrase))
    (Prep        -> to in by with on)
    (Adj         -> big little blue green adiabatic)
    (Article     -> the a)
    (Name        -> Pat Kim Lee Terry Robin)
    (Noun        -> man ball woman table)
    (Verb        -> hit took saw liked)
    (Pronoun     -> he she it these those that)))

(set! *grammar* bigger-grammar)

(define (generate-tree phrase)
  (cond ((list? phrase)       (map generate-tree phrase))
        ((rewrites phrase) => (lambda (rws)
                                (define elt (random-elt rws))
                                (if (list? elt)
                                  (cons phrase (generate-tree elt))
                                  (list elt))))
        (else                 (list phrase))))
;
;; NOTE: do not use this with recursive grammar rules like in bigger-grammar.
(define (generate-all phrase)
  (cond ((null? phrase) '(()))
        ((list? phrase) (combine-all (generate-all (car phrase))
                                     (generate-all (cdr phrase))))
        ((rewrites phrase) => (lambda (rws)
                                (append* (map (lambda (elt)
                                                ;(if (list? elt)
                                                  (generate-all elt)
                                                  ;(list (list elt))
                                                  ;)
                                              )
                                              rws))))
        (else (list (list phrase)))))

(define (cross-product f xs ys)
  (append* (map (lambda (y)
                  (map (lambda (x) (f x y)) xs))
                ys)))

(define (combine-all xs ys) (cross-product append xs ys))


;; A more expressive rule-based approach
(define (rewrite e)
  (match e
    (`(quote ,terminal) terminal)
    (non-terminal       (gen (random-elt (rewrites non-terminal))))))

(define (gen phrase)
  (match phrase
    (`(,'unquote ,e)                 (rewrite e))
    (`((,'unquote-splicing ,e) . ,q) (append (rewrite e) (gen q)))
    (`(,a . ,b)                      (cons (gen a) (gen b)))
    (terminal                        terminal)))

(define tedious-lc-grammar
  ;; Factoring e out of E decreases probability of recursion, converging.
  '((E       -> ,e (lambda ,e) (app ,e ,e) (if ,e ,e ,e) ;(app ,E ,E ,@E*)
                (,Op1 ,e) (,Op2 ,e ,e) (,Op3 ,e ,e ,e))
    (e       -> (var ,Peano) (quote ,S) ,E)
    ;(E*      -> () (,E . ,E*))
    (Peano   -> () (,Peano))
    (Op1     -> null? pair? number? symbol? boolean? procedure? not car cdr
                append* symbol-append*)
    (Op2     -> cons equal? - / = < > <= >= map + * append symbol-append)
    (Op3     -> foldl foldr foldN)
    (digit   -> 0 1 2 3 4 5 6 7 8 9)
    (alpha   -> a b c d e f g h i j k l m n o p q r s t u v w x y z)
    (keyword -> lambda var app quote if)
    (S       -> (,S . ,S) () #t #f ,digit ,alpha ,keyword ,Op1 ,Op2 ,Op3)))

;(set! *grammar* tedious-lc-grammar)

;; This morally flexible eval is likely to find an informative way to interpret
;; an expression, kind of like a saner PHP.
(define (tedious-eval env E)
  (let ev ((x E))
    (define (lookup env index)
      (cond ((null? env)   'unbound-variable)
            ((null? index) (car env))
            (else          (lookup (cdr env) (car index)))))
    (define (tedious-op table op rands)
      (tedious-apply-op op (cdr (assoc op table)) (map ev rands)))
    (match x
      (`(quote ,datum)     datum)
      (`(var ,index)       (lookup env index))
      (`(lambda ,body)     (vector 'closure body env))
      (`(app ,rator ,rand) (tedious-apply (ev rator) (ev rand)))
      (`(if ,c ,t ,f)      (if (ev c) (ev t) (ev f)))
      (`(,op ,r1)          (tedious-op op1 op (list r1)))
      (`(,op ,r1 ,r2)      (tedious-op op2 op (list r1 r2)))
      (`(,op ,r1 ,r2 ,r3)  (tedious-op op3 op (list r1 r2 r3))))))
(define (tedious-apply proc arg)
  (if (closure? proc)
    (tedious-eval (cons arg (closure-env proc)) (closure-body proc))
    `(invalid-application: (,proc ,arg))))

(define (closure? d)
  (and (vector? d) (= 3 (vector-length d)) (eq? 'closure (vector-ref d 0))))
(define (closure-body d) (vector-ref d 1))
(define (closure-env d)  (vector-ref d 2))
(define (symbol-append s1 s2) (symbol-append* (list s1 s2)))
(define (symbol-append* sym*)
  (string->symbol (string-append* (map symbol->string sym*))))
(define (tedious-map f xs)
  (map (lambda (x) (tedious-apply f x)) xs))
(define (tedious-foldl f acc xs)
  (foldl (lambda (x acc) (tedious-apply (tedious-apply f x) acc) acc xs)))
(define (tedious-foldr f acc xs)
  (foldr (lambda (x acc) (tedious-apply (tedious-apply f x) acc) acc xs)))
(define (tedious-foldN f acc n)
  (if (= 0 n) acc
    (tedious-foldN f (tedious-apply (tedious-apply f n) acc) (- n 1))))
(define (tedious-apply-op opname op args)
  (with-handlers
    (((lambda _ #t) (lambda _ `(invalid-operation: (,opname . ,args)))))
    (apply op args)))

(define op1
  `((null?          . ,null?)
    (pair?          . ,pair?)
    (number?        . ,number?)
    (symbol?        . ,symbol?)
    (boolean?       . ,boolean?)
    (procedure?     . ,closure?)
    (not            . ,not)
    (car            . ,car)
    (cdr            . ,cdr)
    (append*        . ,append*)
    (symbol-append* . ,symbol-append*)))
(define op2
  `((cons          . ,cons)
    (equal?        . ,equal?)
    (-             . ,-)
    (/             . ,/)
    (=             . ,=)
    (<             . ,<)
    (>             . ,>)
    (<=            . ,<=)
    (>=            . ,>=)
    (map           . ,tedious-map)
    (+             . ,+)
    (*             . ,*)
    (append        . ,append)
    (symbol-append . ,symbol-append)))
(define op3
  `((foldl . ,tedious-foldl)
    (foldr . ,tedious-foldr)
    (foldN . ,tedious-foldN)))

(define (tedious-gen-eval)
  (define old-grammar *grammar*)
  (set! *grammar* tedious-lc-grammar)
  (define expr (gen ',E))
  (define result (tedious-eval '() expr))
  (set! *grammar* old-grammar)
  `(,expr ===> ,result))
