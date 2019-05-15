(load "faster-miniKanren/mk-vicare.scm")
(load "faster-miniKanren/mk.scm")
(load "faster-miniKanren/numbers.scm")
(load "faster-miniKanren/test-check.scm")

;;; William E. Byrd

;;; Exercises from Peter Norvig's 'Paradigms of Artificial Intelligence Programming'

;;; Load this file in Chez Scheme (https://cisco.github.io/ChezScheme/)

;;; "Exercise 1.4 [m] Write a function that counts the number of times an expression occurs anywhere within another expression.
;;; Example: (count-anywhere 'a '(a ((a) b) a)) ⇒ 3."


;;; In plain Scheme:

(define count-anywhere
  (lambda (x y)
    (cond
      ((equal? x y) 1)
      ((pair? y)
       (+ (count-anywhere x (car y))
          (count-anywhere x (cdr y))))
      (else 0))))

(test "count-anywhere 1"
  (count-anywhere 'a '(a b a))
  2)

(test "count-anywhere 2"
  (count-anywhere 'a '(a ((a) b) a))
  3)

(test "count-anywhere 3"
  (count-anywhere '(x y) '((z x y) ((x y z) b) x y))
  2)


;;; miniKanren version

;;; Uncomment this definition of 'pluso' to use Greg's approach that
;;; builds up arithmetic expressions, rather than evaluating to a
;;; concrete number (represented as little-endian binary lists) using
;;; Oleg's approach to relational arithmetic.
;;;
#;(define (pluso a b a+b)
  (== `(+ ,a ,b) a+b))

(define count-anywhereo
  (lambda (x y count)
    (conde
      ((== x y) (== '(1) count))
      ((=/= x y)
       (== '() count)
       (conde
         ((== '() y))
         ((== #t y))
         ((== #f y))
         ((symbolo y))
         ((numbero y))))
      ((=/= x y)
       (fresh (a d a-count d-count)
         (== (cons a d) y)
         (count-anywhereo x a a-count)
         (count-anywhereo x d d-count)
         (pluso a-count d-count count))))))


;;; generative grammar

;;; helper
(define membero
  (lambda (x ls)
    (fresh (a d)
      (== (cons a d) ls)
      (conde
        ((== a x))
        ((=/= a x)
         (membero x d))))))

(define Adj
  (lambda (x)
    (membero
     x
     '(big little green blue adiabatic))))

(define Prep
  (lambda (x)
    (membero
     x
     '(to in by with on))))

(define Adj*
  (lambda (x)
    (conde
      ((== '() x))
      ((fresh (adj adj*)
         (Adj adj)
         (appendo `(,adj) adj* x)
         (Adj* adj*))))))

(define PP
  (lambda (x)
    (fresh (pr np)
      (Prep pr)
      (noun-phrase np)
      (appendo pr np x))))

(define sentence
  (lambda (x)
    (fresh (np vp)
      (noun-phrase np)
      (verb-phrase vp)
      (appendo np vp x))))

(define noun-phrase
  (lambda (x)
    (fresh (a n)
      (== `(,a ,n) x)
      (Article a)
      (Noun n))))

(define verb-phrase
  (lambda (x)
    (fresh (v np)
      (== `(,v . ,np) x)
      (Verb v)
      (noun-phrase np))))

(define Article
  (lambda (x)
    (membero x '(the a))))

(define Noun
  (lambda (x)
    (membero x '(man ball woman table))))

(define Verb
  (lambda (x)
    (membero x '(hit took saw liked))))





