#lang planet neil/sicp

;; Special forms provided by the evaluator
(define (amb) 'this-is-placeholder-do-not-evaluate)

;; -------------------------------------------------------
;; Helper functions (provided by the evaluator)
;; -------------------------------------------------------

(define (require p)
  (if (not p) (amb)))

(define (amb-list items)
  (if (null? items)
      (amb)
      (amb (car items) (amb-list (cdr items)))))

(define (!= a b) (not (= a b)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (foldr f init seq)
  (if (null? seq)
      init
      (f (car seq) (foldr f init (cdr seq)))))

(define (flatmap f items)
  (foldr append '() (map f items)))

;; -------------------------------------------------------
;; Examples of Nondeterministic Programs
;; -------------------------------------------------------

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ 1 n))))

;; Exercise 4.35, p.417

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ 1 low) high)))

;; Exercise 4.36, p.417

(define (pythagorean-triple)
  (let ((j (an-integer-starting-from 1)))
    (let ((i (an-integer-between 1 j)))
      (let ((k (an-integer-between j (* 2 j))))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

;; Exercise 4.37, p.418
;; Assuming sqrt and integer? procedures are fast,
;; Ben is correct. His solution is triangular, O(n^2)
;; Exercise 4.35 solution is tetrahedral, O(n^3).

;; Exercise 4.38, p.419
;; In addition to (3 2 4 5 1) there are 4 others:
;; (1 2 4 3 5) (1 2 4 5 3) (1 4 2 5 3) (3 4 2 5 1)

;; Exercise 4.39, p.419
;; distinct? is slow, it should be the last condition.

(define (multiple-dwelling-2)
  (let ((baker (amb 1 3))
        (cooper (amb 2 4))
        (fletcher (amb 2 4))
        (miller (amb 3 5))
        (smith (amb 1 3 5)))
    (require (> miller cooper))
    (require (not (= (abs (- fletcher smith)) 1)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

;; Exercise 4.40, p.419

(define (multiple-dwelling-3)
  (let ((cooper (amb 2 4))
        (fletcher (amb 2 4)))
    (require (not (= cooper fletcher)))
    (let ((baker (amb 1 3))
          (miller (amb 3 5)))
      (require (not (= baker miller)))
      (require (> miller cooper))
      (let ((smith (amb 1 3 5)))
        (require (not (= smith miller)))
        (require (not (= smith baker)))
        (require (not (= (abs (- smith fletcher)) 1)))
        (list (list 'baker baker)
              (list 'cooper cooper)
              (list 'fletcher fletcher)
              (list 'miller miller)
              (list 'smith smith))))))

;; Exercise 4.41, p.420
;; Multiple Dwelling in ordinary Scheme

(define (remove x items)
  (cond ((null? items) '())
        ((eq? x (car items)) (cdr items))
        (else (cons (car items) (remove x (cdr items))))))

(define (shuffle items)
  (map (lambda (i)
         (cons i (remove i items)))
       items))

(define (permutations items)
  (cond ((null? items) '())
        ((null? (cdr items)) (list items))
        (else (flatmap (lambda (ls)
                         (map (lambda (x) (cons (car ls) x))
                              (permutations (cdr ls))))
                       (shuffle items)))))

(define (filter p items)
  (cond ((null? items) '())
        ((p (car items)) (cons (car items) (filter p (cdr items))))
        (else (filter p (cdr items)))))

(define (caddddr items) (car (cddddr items)))

(define (solves? perm)
  (let ((b (car perm))
        (c (cadr perm))
        (f (caddr perm))
        (m (cadddr perm))
        (s (caddddr perm)))
    (and (!= b 5)
         (!= c 1)
         (!= f 5)
         (!= f 1)
         (> m c)
         (!= (abs (- s f)) 1)
         (!= (abs (- f c)) 1))))

(define (multiple-dwelling-4)
  (filter solves? (permutations '(1 2 3 4 5))))

(equal? (list '(3 2 4 5 1)) (multiple-dwelling-4))

;; Exercise 4.42, p.420

(define (xor p1 p2)
  (or (and p1 (not p2))
      (and (not p1) p2)))

(define (either p1 p2)
  (require (xor p1 p2)))

(define (schoolgirls)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (either (= kitty 2) (= betty 3))
    (either (= ethel 1) (= joan 2))
    (either (= joan 3) (= ethel 5))
    (either (= kitty 2) (= mary 4))
    (either (= mary 4) (= betty 1))
    (require
     (distinct? (list betty ethel joan kitty mary)))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))

;= ((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))

;; Exercise 4.43, p.420

(define (impl a b) (or (not a) b))

(define (daughters)
  (let ((mary-ann (amb 'moore))
        (melissa (amb 'hood))
        (gabrielle (amb 'downing 'hall 'moore))
        (rosalind (amb 'downing 'moore 'parker))
        (lorna (amb 'downing 'hall 'parker)))
    (require (impl (eq? gabrielle 'downing) (eq? 'hood 'parker)))
    (require (impl (eq? gabrielle 'hall) (eq? rosalind 'parker)))
    (require
     (distinct? (list mary-ann gabrielle lorna rosalind melissa)))
    (list (list 'mary-ann mary-ann)
          (list 'melissa melissa)
          (list 'gabrielle gabrielle)
          (list 'rosalind rosalind)
          (list 'lorna lorna))))

;= (lorna downing)

;; Exercise 4.44, p.420
;; Eight-queens puzzle

;; Load: require, map, foldr, append, remove, shuffle, flatmap, permutations, !=

(define (safe-pos? pos others)
  (define (safe-right? idx right-cells)
    (or (null? right-cells)
        (let ((i (car idx))
              (d (cdr idx))
              (r (car right-cells)))
          (and (!= i r)
               (!= d r)
               (safe-right? (cons (+ i 1) (- d 1))
                            (cdr right-cells))))))
  (safe-right? (cons pos pos) (cons 0 others)))

(define (safe? positions)
  (cond ((null? positions) true)
        ((null? (cdr positions)) true)
        (else (and (safe-pos? (car positions) (cdr positions))
                   (safe? (cdr positions))))))

; this implementation builds all permutations first, ~30sec
; lazy evaluator would be very handy here
(define (8-queens)
  (let ((positions (amb-list (permutations '(1 2 3 4 5 6 7 8)))))
    (require (safe? positions))
    positions))

;= (1 5 8 6 3 7 2 4)
;= (1 6 8 3 7 4 2 5)
;= (1 7 4 6 8 2 5 3)
;= (1 7 5 8 2 4 6 3)
;...

;; -------------------------------------------------------
;; Parsing Natural Language, p.420
;; -------------------------------------------------------

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define *unparsed* nil)

(define (parse-word words)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr words)))
  (let ((found (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car words) found)))

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

; (parse '(the cat eats))
; (parse '(the professor lectures to the student with the cat))
