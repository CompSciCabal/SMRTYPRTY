(load "faster-miniKanren/mk-vicare.scm")
(load "faster-miniKanren/mk.scm")
(load "faster-miniKanren/numbers.scm")
(load "faster-miniKanren/test-check.scm")

;;; William E. Byrd

;;; Exercises from Peter Norvig's 'Paradigms of Artificial Intelligence Programming'

;;; All code released under MIT licence.



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

;;; Examples of relational arithmetic using "Oleg Numerals"
;;; (little-endian binary lists):

(test "build-num 1"
  (build-num 6)
  '(0 1 1))

(test "pluso 1"
  (run 1 (q)
    (pluso (build-num 6) (build-num 7) q))
  '((1 0 1 1)))

(test "pluso 2"
  (run* (x y)
    (pluso x y (build-num 5)))
  '(((1 0 1) ())
    (() (1 0 1))
    ((1) (0 0 1))
    ((0 0 1) (1))
    ((1 1) (0 1))
    ((0 1) (1 1))))


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

(test "count-anywhereo 1"
  (run* (q)
    (count-anywhereo
     q
     '((z x y) ((x y z) b) x y)
     (build-num 2)))
  '((x y) z (y)))

(test "count-anywhereo 2"
  (run 1 (q)
    (count-anywhereo
     '(x y)
     '((z x y) ((x y z) b) x y)
     q))
  '((0 1)))

(test "count-anywhereo 3"
  (run 1 (q)
    (count-anywhereo
     q
     '((z x y) ((x y z) b) x y)
     (build-num 2)))
  '((x y)))

(test "count-anywhereo 4"
  (run 2 (q)
    (count-anywhereo
     q
     '((z x y) ((x y z) b) x y)
     (build-num 2)))
  '((x y) z))

(test "count-anywhereo 5"
  (run* (q)
    (count-anywhereo
     q
     '((z x y) ((x y z) b) x y)
     (build-num 2)))
  '((x y) z (y)))

(test "count-anywhereo 6"
  (run 1 (e1 e2)
    (count-anywhereo
     e1
     e2
     (build-num 3)))
  '((_.0 (_.0 _.0 . _.0))))

(test "count-anywhereo 7"
  (run 10 (e1 e2)
    (count-anywhereo
     e1
     e2
     (build-num 3)))
  '((_.0 (_.0 _.0 . _.0))
    ((_.0 (_.0 _.0 _.0))
     (=/= ((_.0 ()))))
    ((_.0 (_.0 _.0 () . _.0))
     (=/= ((_.0 ()))))
    ((_.0 (_.0 _.0 _.0 . #t))
     (=/= ((_.0 #t))))
    ((_.0 (_.0 _.0 _.0 . #f))
     (=/= ((_.0 #f))))
    ((_.0 (_.0 () _.0 . _.0))
     (=/= ((_.0 ()))))
    ((_.0 (() _.0 _.0 . _.0))
     (=/= ((_.0 ()))))
    ((_.0 (_.0 _.0 _.0 . _.1))
     (=/= ((_.0 _.1)))
     (sym _.1))
    ((_.0 (_.0 _.0 #t . _.0))
     (=/= ((_.0 #t))))
    ((_.0 (_.0 _.0 _.0 . _.1))
     (=/= ((_.0 _.1)))
     (num _.1))))



;;; Generative grammar time!
;;;
;;; This code is inspired by Chapter 2 of PAIP, starting with the code
;;; in section 2.2.

;;; 'membero' is the standard list membership miniKanren helper relation.
;;;
;;; Succeeds if 'x' is a member of 'ls'.
;;;
;;; Both 'x' and 'ls' can be logic variables, or pairs that contain logic variables.
(define membero
  (lambda (x ls)
    (fresh (a d)
      (== (cons a d) ls)
      (conde
        ((== a x))
        ((=/= a x)
         (membero x d))))))

(test "membero 1"
  (run* (q)
    (membero 'y '(x y z)))
  '(_.0))

(test "membero 2"
  (run* (q)
    (membero 'w '(x y z)))
  '())

(test "membero 3"
  (run* (q)
    (membero q '(x y z)))
  '(x y z))

(test "membero 4"
  (run 3 (q)
    (membero 'y q))
  '((y . _.0)
    ((_.0 y . _.1)
     (=/= ((_.0 y))))
    ((_.0 _.1 y . _.2)
     (=/= ((_.0 y)) ((_.1 y))))))

(test "membero 5"
  (run* (q)
    (membero 'y `(x ,q z)))
  '(y))


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


(test "sentence 1"
  (run 10 (q) (sentence q))
  '((the man hit the man)
    (the man hit the ball)
    (the man hit a man)
    (the man took the man)
    (the ball hit the man)
    (the man hit the woman)
    (the man hit a ball)
    (the man hit the table)
    (the man hit a woman)
    (the man took the ball)))

(test "sentence 1"
  (run* (q) (sentence '(a woman liked a table)))
  '(_.0))

(test "sentence 2"
  (run* (q) (sentence '(woman a liked a table)))
  '())

(test "sentence 2"
  (run* (x y) (sentence '(a ,x liked . ,y)))
  '())

(test "sentence 3"
  (run 10 (x y) (sentence `(a ,x liked . ,y)))
  '((man (the man))
    (man (the ball))
    (man (a man))
    (man (the woman))
    (man (a ball))
    (man (the table))
    (man (a woman))
    (man (a table))
    (ball (the man))
    (ball (the ball))))

(test "sentence 4"
  (run 1 (s)
    (fresh (x y z)
      (== `(,x ,y liked . ,z) s)
      (sentence s)))
  '((the man liked the man)))

(test "sentence 5"
  (run 1 (s)
    (fresh (x y z)
      (== `(,x ,y liked . ,z) s)
      (absento 'man s)
      (sentence s)))
  '((the ball liked the ball)))

(test "sentence 6"
  (run 1 (s)
    (fresh (x y z)
      (== `(,x ,y liked . ,z) s)
      (absento 'man s)
      (absento 'ball s)
      (sentence s)))
  '((the woman liked the woman)))

(test "Adj* 1"
  (run 100 (adj*)
    (Adj* adj*))
  '(()
    (big)
    (little)
    (big big)
    (green)
    (big little)    
    (little big)
    (big big big)
    (blue)
    (big green)
    (little little)
    (big big little)
    (green big)
    (big little big)
    (little big big)
    (big big big big)
    (adiabatic)
    (big blue)
    (little green)
    (big big green)
    (green little)
    (big little little)
    (little big little)
    (big big big little)
    (blue big)
    (big green big)
    (little little big)
    (big big little big)
    (green big big)
    (big little big big)
    (little big big big)
    (big big big big big)
    (adiabatic big)
    (big adiabatic)
    (little blue)
    (big big blue)
    (green green)
    (big little green)
    (little big green)
    (big big big green)
    (blue little)
    (big green little)
    (little little little)
    (big big little little)
    (green big little)
    (big little big little)
    (little big big little)
    (big big big big little)
    (big blue big)
    (adiabatic little)
    (little green big)
    (big big green big)
    (green little big)
    (big little little big)
    (little big little big)
    (big big big little big)
    (blue big big)
    (big green big big)
    (little little big big)
    (big big little big big)
    (green big big big)
    (big little big big big)
    (little big big big big)
    (big big big big big big)
    (adiabatic big big)
    (big adiabatic big)
    (little adiabatic)
    (big big adiabatic)
    (green blue)
    (big little blue)
    (little big blue)
    (big big big blue)
    (blue green)
    (big green green)
    (little little green)
    (big big little green)
    (green big green)
    (big little big green)
    (little big big green)
    (big big big big green)
    (big blue little)
    (adiabatic green)
    (little green little)
    (big big green little)
    (green little little)
    (big little little little)
    (little big little little)
    (big big big little little)
    (blue big little)
    (big green big little)
    (little little big little)
    (big big little big little)
    (green big big little)
    (big little big big little)
    (little big big big little)
    (big big big big big little)
    (adiabatic big little)
    (little blue big)
    (big big blue big)
    (green green big)))
