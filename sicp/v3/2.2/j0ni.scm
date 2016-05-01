(require-extension numbers)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items)
                (- n 1))))

(define squares
  (list 1 4 9 16 25))

(print (list-ref squares 3))

(define (length-recursive items)
  (if (null? items)
      0
      (+ 1 (length-recur (cdr items)))))

(define odds
  (list 1 3 5 7))

(define (length-iterative items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a)
                     (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1)
                    list2))))

;; Exercise 2.17: Define a procedure last-pair that returns the list that
;; contains only the last element of a given (nonempty) list:

;; (last-pair (list 23 72 149 34))
;; (34)

(define (last-pair l)
  (let ((rest (cdr l)))
    (if (null? rest)
        (car l)
        (last-pair rest))))

;; Exercise 2.18: Define a procedure reverse that takes a list as argument and
;; returns a list of the same elements in reverse order:

;; (reverse (list 1 4 9 16 25))
;; (25 16 9 4 1)

(define (reverse l)
  (define (reverse-iter acc l)
    (if (null? l)
        acc
        (reverse-iter (cons (car l) acc)
                      (cdr l))))
  (reverse-iter '() l))

;; Exercise 2.19: Consider the change-counting program of 1.2.2. It would be
;; nice to be able to easily change the currency used by the program, so that we
;; could compute the number of ways to change a British pound, for example. As
;; the program is written, the knowledge of the currency is distributed partly
;; into the procedure first-denomination and partly into the procedure
;; count-change (which knows that there are five kinds of U.S. coins). It would
;; be nicer to be able to supply a list of coins to be used for making change.

;; We want to rewrite the procedure cc so that its second argument is a list of
;; the values of the coins to use rather than an integer specifying which coins
;; to use. We could then have lists that defined each kind of currency:

(define us-coins
  (list 50 25 10 5 1))

(define uk-coins
  (list 100 50 20 10 5 2 1 0.5))

;; We could then call cc as follows:

;; (cc 100 us-coins)
;; 292

;; To do this will require changing the program cc somewhat. It will still have
;; the same form, but it will access its second argument differently, as
;; follows:

(define (cc amount coin-values)
  (cond ((= amount 0)
         1)
        ((or (< amount 0)
             (no-more? coin-values))
         0)
        (else
         (+ (cc
             amount
             (except-first-denomination
              coin-values))
            (cc
             (- amount
                (first-denomination
                 coin-values))
             coin-values)))))

;; Define the procedures first-denomination, except-first-denomination and
;; no-more? in terms of primitive operations on list structures. Does the order
;; of the list coin-values affect the answer produced by cc? Why or why not?

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

;; Exercise 2.20: The procedures +, *, and list take arbitrary numbers of
;; arguments. One way to define such procedures is to use define with
;; dotted-tail notation. In a procedure definition, a parameter list that has a
;; dot before the last parameter name indicates that, when the procedure is
;; called, the initial parameters (if any) will have as values the initial
;; arguments, as usual, but the final parameter’s value will be a list of any
;; remaining arguments. For instance, given the definition

;; (define (f x y . z) ⟨body⟩)

;; the procedure f can be called with two or more arguments. If we evaluate

;; (f 1 2 3 4 5 6)

;; then in the body of f, x will be 1, y will be 2, and z will be the list (3 4
;; 5 6). Given the definition

;; (define (g . w) ⟨body⟩)

;; the procedure g can be called with zero or more arguments. If we evaluate

;; (g 1 2 3 4 5 6)

;; then in the body of g, w will be the list (1 2 3 4 5 6).

;; Use this notation to write a procedure same-parity that takes one or more
;; integers and returns a list of all the arguments that have the same even-odd
;; parity as the first argument. For example,

;; (same-parity 1 2 3 4 5 6 7)
;; (1 3 5 7)

;; (same-parity 2 3 4 5 6 7)
;; (2 4 6)

(define (same-parity . l)
  (define (filter acc pred l)
    (if (null? l)
        acc
        (let ((x (car l)))
          (filter (if (pred x)
                      (cons x acc)
                      acc)
                  pred
                  (cdr l)))))
  (if (odd? (car l))
      (reverse (filter '() odd? l))
      (reverse (filter '() even? l))))
