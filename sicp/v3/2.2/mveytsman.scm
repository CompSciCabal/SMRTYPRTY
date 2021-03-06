(require-extension sicp)

;; Exercise 2.17: Define a procedure last-pair that returns the list that
;; contains only the last element of a given (nonempty) list:


(define (last-pair lst)
  (if (null? (cdr lst))
      (car lst)
      (last-pair (cdr lst))))

(last-pair (list 23 72 149 34))
;; => 34

;; ------------------------------------------

;; Exercise 2.18: Define a procedure reverse that takes a list as argument and
;; returns a list of the same elements in reverse order:

(define (reverse lst)
  (define (iter a b)
    (if (null? b)
        a
        (iter (cons (car b) a)(cdr b))))
  (iter (list) lst))

(reverse (list 1 4 9 16 25))
;; => (25 16 9 4 1)


;; -------------------------------------------

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
;; => 292

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


(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

(cc 100 us-coins)
;; => 292


;; I don't think changing the order of the coin values affects the answer
;; produced because it's still a tree recursion.

;; ----------------------------------------------

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
;; then in the body of f, x will be 1, y will be 2, and z will be the list (3 4 5 6). Given the definition

;; (define (g . w) ⟨body⟩)
;; the procedure g can be called with zero or more arguments. If we evaluate

;; (g 1 2 3 4 5 6)
;; then in the body of g, w will be the list (1 2 3 4 5 6).77

;; Use this notation to write a procedure same-parity that takes one or more integers and returns a list of all the arguments that have the same even-odd parity as the first argument. For example,

(define (same-parity fst . rst)
  (define (predicate x)
    (or (and (even? fst) (even? x))
        (and (odd? fst) (odd? x))))
  (define (helper lst)
    (cond ((null? lst)
           nil)
          ((predicate (car lst))
           (cons (car lst) (helper (cdr lst))))
          (else (helper (cdr lst)))))
  (cons fst (helper rst)))

(same-parity 1 2 3 4 5 6 7)
;; => (1 3 5 7)

(same-parity 2 3 4 5 6 7)
;; =>  (2 4 6)


;; -------------------------------------------------------

;; Exercise 2.21: The procedure square-list takes a list of numbers as argument
;; and returns a list of the squares of those numbers.

;; (square-list (list 1 2 3 4))
;; (1 4 9 16)

;; Here are two different definitions of square-list. Complete both of them by filling in the missing expressions:

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;; --------------------------------------------------------

;; Exercise 2.22: Louis Reasoner tries to rewrite the first square-list
;; procedure of Exercise 2.21 so that it evolves an iterative process:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))
;; Unfortunately, defining square-list this way produces the answer list in the reverse order of the one desired. Why?

;; ANSWER: We're consing the list up from the front, each step conses the next element onto the list of prevous elements and since linked lists are built back to front our list is backwards

;; Louis then tries to fix his bug by interchanging the arguments to cons:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square 
                     (car things))))))
  (iter items nil))

;; This doesn’t work either. Explain.

;; ANSWER: In this case each step conses a list to the front of the next element, making a deeply nested list like (((() . 1) . 4) . 9)
;; Lists with cons are built back to front, but we're iterating front to back, so we can't build the answer we need unless we first reverse the argument to the iterative procedure.

(square-list (list 1 2 3))

;; ------------------------------------------------------------

;; Exercise 2.23: The procedure for-each is similar to map. It takes as
;; arguments a procedure and a list of elements. However, rather than forming a
;; list of the results, for-each just applies the procedure to each of the
;; elements in turn, from left to right. The values returned by applying the
;; procedure to the elements are not used at all—for-each is used with
;; procedures that perform an action, such as printing. For example,

(for-each 
 (lambda (x) (newline) (display x))
 (list 57 321 88))

;; 57
;; 321
;; 88

;; The value returned by the call to for-each (not illustrated above) can be something arbitrary, such as true. Give an implementation of for-each.

(define (for-each f lst)
  (if (null? lst)
      #t
      (begin
        (f (car lst))
        (for-each f (cdr lst)))))

;; I cheated above and used begin so we can handle the empty list case, even though I don't think we hit begin in the book yet.

;; If we assume we always have at least one element in the list, we can do it without begin(define (for-each f lst)


(define (for-each f lst)
  (f (car lst))
  (if (null? (cdr lst))
      #t
      (for-each f (cdr lst))))

;; -------------------------------------------------------

;; Exercise 2.24: Suppose we evaluate the expression (list 1 (list 2 (list 3
;; 4))). Give the result printed by the interpreter, the corresponding
;; box-and-pointer structure, and the interpretation of this as a tree (as in
;; Figure 2.6).

;; The interpreter gives (1 (2 (3 4)))

;; (1 (2 (3 4)  (2 (3 4) (3 4)
;;      +        +        +
;;      |        |        |
;;      |        |        |
;;      +---+    +---+    +---+    +---+
;;      |.|.+--->+.|.+--->+.|.+--->+.|/|
;;      ++--+    ++--+    ++--+    ++--+
;;      |        |        |        |
;;      v        v        v        v
;;      +++      +++      +++      +++
;;      |1|      |2|      |3|      |4|
;;      +-+      +-+      +-+      +-+


;;; -----------------------------

;; Exercise 2.25: Give combinations of cars and cdrs that will pick 7 from each of the following lists:


(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
;; => 7

(car (car '((7))))
;; => 7

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))
;; => 7

;; -----------------------------------

;; Exercise 2.26: Suppose we define x and y to be two lists:

(define x (list 1 2 3))
(define y (list 4 5 6))

;; What result is printed by the interpreter in response to evaluating each of the following expressions:

(append x y)
;; => (1 2 3 4 5 6)

(cons x y)
;; => ((1 2 3) 4 5 6)

(list x y)
;; => ((1 2 3) (4 5 6))

;; Exercise 2.27: Modify your reverse procedure of Exercise 2.18 to produce a
;; deep-reverse procedure that takes a list as argument and returns as its value
;; the list with its elements reversed and with all sublists deep-reversed as
;; well. For example,


(define (deep-reverse lst)
  (define (iter a b)
    (if (null? b)
        a
        (iter (cons (deep-reverse (car b)) a)(cdr b))))
  (if (pair? lst)
      (iter nil lst)
      lst))


(define x 
  (list (list 1 2) (list 3 4)))

x
;; => ((1 2) (3 4))

(reverse x)
;; => ((3 4) (1 2))

(deep-reverse x)
;; => ((4 3) (2 1))

;; Exercise 2.28: Write a procedure fringe that takes as argument a tree
;; (represented as a list) and returns a list whose elements are all the leaves
;; of the tree arranged in left-to-right order. For example,

(define (fringe tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

(define x 
  (list (list 1 2) (list 3 4)))

(fringe x)
(1 2 3 4)

(fringe (list x x))
(1 2 3 4 1 2 3 4)

;; ---------------------------------

;; Exercise 2.29: A binary mobile consists of two branches, a left branch and a
;; right branch. Each branch is a rod of a certain length, from which hangs
;; either a weight or another binary mobile. We can represent a binary mobile
;; using compound data by constructing it from two branches (for example, using
;; list):

(define (make-mobile left right)
  (list left right))

;; A branch is constructed from a length (which must be a number) together with a structure, which may be either a number (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

;; 1. Write the corresponding selectors left-branch and right-branch, which return
;; the branches of a mobile, and branch-length and branch-structure, which
;; return the components of a branch.

(define left-branch car))

(define right-branch cadr)

(define branch-length car)

(define branch-structure cadr)
;; 2. Using your selectors, define a procedure total-weight that returns the
;; total weight of a mobile.

(define mobile? pair?)

(define (branch-weight branch)
  (if (mobile? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))
(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

;; 3. A mobile is said to be balanced if the torque applied by its top-left
;; branch is equal to that applied by its top-right branch (that is, if the
;; length of the left rod multiplied by the weight hanging from that rod is
;; equal to the corresponding product for the right side) and if each of the
;; submobiles hanging off its branches is balanced. Design a predicate that
;; tests whether a binary mobile is balanced.

(define (balanced? mobile)
  (define (torque branch)
    (* (branch-length branch)
       (branch-weight branch)))
  (if (mobile? mobile)
      (and
       (= (torque (left-branch mobile))
          (torque (right-branch mobile)))
       (balanced? (left-branch mobile))
       (balanced (right-branch mobile)))
      #t))


;; 4. Suppose we change the representation of mobiles so that the constructors are
;; (define (make-mobile left right)
;;   (cons left right))

;; (define (make-branch length structure)
;;   (cons length structure))
;; How much do you need to change your programs to convert to the new representation?

;; We'd have to rewrite right-branch and branch-structure to be cdr instead of cadr. Everything will then work.

;; --------------------------------

;; Exercise 2.30: Define a procedure square-tree analogous to the square-list procedure of Exercise 2.21. That is, square-tree should behave as follows:

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;; => (1 (4 (9 16) 25) (36 49))

;;Define square-tree both directly (i.e., without using any higher-order procedures) and also by using map and recursion.

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree))
         (square tree))
        (else
         (cons (square-tree (car tree))
               (square-tree (cdr tree))))))


(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

;; -------------------------------------------------
;; Exercise 2.31: Abstract your answer to Exercise 2.30 to produce a procedure
;; tree-map with the property that square-tree could be defined as

(define (square-tree tree)
  (tree-map square tree))

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

;; ------------------------------------------------------

;; Exercise 2.32: We can represent a set as a list of distinct elements, and we
;; can represent the set of all subsets of the set as a list of lists. For
;; example, if the set is (1 2 3), then the set of all subsets is (() (3) (2) (2
;; 3) (1) (1 3) (1 2) (1 2 3)). Complete the following definition of a procedure
;; that generates the set of subsets of a set and give a clear explanation of
;; why it works:

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x)) rest)))))


;; It works by induction. The subsets of an empty set is empty. If the S' is the
;; set S with the element x added, then the subsets of S' are all the subsets of
;; S, along with each of those subsets with x included in them.


;; -----------------------------------------------
;; Exercise 2.33: Fill in the missing expressions to complete the following
;; definitions of some basic list-manipulation operations as accumulations:

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x acc) (inc acc)) 0 sequence))

;; ------------------------------------------------

;; Exercise 2.34: Evaluating a polynomial in xx at a given value of xx can be
;; formulated as an accumulation. We evaluate the polynomial

;; a_n^x + a_{n-1}x^{n-1} + ... + a_1x + a_0
;; using a well-known algorithm called Horner’s rule, which structures the computation as
;; (... (a_nx+a_{n-1}x+...+a_1)x+a_0

;; In other words, we start with anan, multiply by xx, add a_{n-1}, multiply by
;; x, and so on, until we reach a0.

;; Fill in the following template to produce a procedure that evaluates a
;; polynomial using Horner’s rule. Assume that the coefficients of the
;; polynomial are arranged in a sequence, from a_0 through a_n.

(define
  (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* higher-terms x)))
   0
   coefficient-sequence))

For example, to compute 1+3x+5x^3+x^5 at x=2 you would evaluate

(horner-eval 2 (list 1 3 0 5 0 1))


;; -------------------------------
;; Exercise 2.35: Redefine count-leaves from 2.2.2 as an accumulation:

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (if (pair? x)
                             (count-leaves x)
                             1)) t)))


;; -------------------------------

;; Exercise 2.36: The procedure accumulate-n is similar to accumulate except
;; that it takes as its third argument a sequence of sequences, which are all
;; assumed to have the same number of elements. It applies the designated
;; accumulation procedure to combine all the first elements of the sequences,
;; all the second elements of the sequences, and so on, and returns a sequence
;; of the results. For instance, if s is a sequence containing four sequences,
;; ((1 2 3) (4 5 6) (7 8 9) (10 11 12)), then the value of (accumulate-n + 0 s)
;; should be the sequence (22 26 30). Fill in the missing expressions in the
;; following definition of accumulate-n:

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
;; => (22 26 30)

;; -------------------------------------------------------

;; Exercise 2.37: Suppose we represent vectors v = (v_i) as sequences of
;; numbers, and matrices m = (m_{ij}) as sequences of vectors (the rows of the
;; matrix). For example, the matrix
;; 1 2 3 4
;; 4 5 6 6
;; 6 7 8 9

;; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this
;; representation, we can use sequence operations to concisely express the basic
;; matrix and vector operations. These operations (which are described in any
;; book on matrix algebra) are the following: (dot-product v w)(matrix-*-vector
;; m v)(matrix-*-matrix m n)(transpose m)returns the sumΣiviwi;returns the
;; vectort,whereti=Σjmijvj;returns the matrixp,wherepij=Σkmiknkj;returns the
;; matrixn,wherenij=mji. (dot-product v w)returns the sumΣiviwi;(matrix-*-vector
;; m v)returns the vectort,whereti=Σjmijvj;(matrix-*-matrix m n)returns the
;; matrixp,wherepij=Σkmiknkj;(transpose m)returns the matrixn,wherenij=mji. We
;; can define the dot product as83

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; Fill in the missing expressions in the following procedures for computing the other matrix operations. (The procedure accumulate-n is defined in Exercise 2.36.)

(define (matrix-*-vector m v)
  (map (lambda (row)
         (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n (lambda (row m)
                  (cons row m)) nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col)
                  (dot-product row col))
                cols)) m)))


;; ---------------------------------------------

;; Exercise 2.38: The accumulate procedure is also known as fold-right, because
;; it combines the first element of the sequence with the result of combining
;; all the elements to the right. There is also a fold-left, which is similar to
;; fold-right, except that it combines elements working in the opposite
;; direction:

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


;; What are the values of

(fold-right / 1 (list 1 2 3))
;; => 1.5
(fold-left  / 1 (list 1 2 3))
;; => 0.166666666666667
(fold-right list nil (list 1 2 3))
;; => (1 (2 (3 ())))
(fold-left  list nil (list 1 2 3))
;; => (((() 1) 2) 3)

;; Give a property that op should satisfy to guarantee that fold-right and fold-left will produce the same values for any sequence.

;; op has to be commutative (and possibly associative)


;; -----------------------------------------------------

;; Exercise 2.39: Complete the following definitions of reverse (Exercise 2.18)
;; in terms of fold-right and fold-left from Exercise 2.38:

(define (reverse2 sequence)
  (fold-right 
   (lambda (x y) (append y (list x))) nil sequence))

(reverse2 '(1 2 3 5))

(define (reverse2 sequence)
  (fold-left 
   (lambda (x y) (cons y x)) nil sequence))

;; --------------------------------------------------------------

;; Exercise 2.40: Define a procedure unique-pairs that, given an integer n,
;; generates the sequence of pairs (i,j) with 1≤j<i≤n. Use
;; unique-pairs to simplify the definition of prime-sum-pairs given above.

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) 
            (list i j))
          (enumerate-interval 
           1
           (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter 
        prime-sum?
        (unique-pairs n))))

;; Exercise 2.41: Write a procedure to find all ordered triples of distinct
;; positive integers i, j, and k less than or equal to a given integer n
;; that sum to a given integer s.

(define (unique-triples n)
  (flatmap
   (lambda (i)
     (map (lambda (j)
            (cons i j))
          (unique-pairs (- i 1))))
   (enumerate-interval 1 n)))

(define (sum seq)
  (accumulate + 0 seq))

(define (bounded-sum-triples n s)
  (filter
   (lambda (triple) (= s (sum triple)))
   (unique-triples n)))


;; --------------------------------------------------------------

;; Exercise 2.42: The “eight-queens puzzle” asks how to place eight queens on a
;; chessboard so that no queen is in check from any other (i.e., no two queens
;; are in the same row, column, or diagonal). One possible solution is shown in
;; Figure 2.8. One way to solve the puzzle is to work across the board, placing
;; a queen in each column. Once we have placed k−1k−1 queens, we must place the
;; kthkth queen in a position where it does not check any of the queens already
;; on the board. We can formulate this approach recursively: Assume that we have
;; already generated the sequence of all possible ways to place k−1k−1 queens in
;; the first k−1k−1 columns of the board. For each of these ways, generate an
;; extended set of positions by placing a queen in each row of the kthkth
;; column. Now filter these, keeping only the positions for which the queen in
;; the kthkth column is safe with respect to the other queens. This produces the
;; sequence of all ways to place kk queens in the first kk columns. By
;; continuing this process, we will produce not only one solution, but all
;; solutions to the puzzle.

;; We implement this solution as a procedure queens, which returns a sequence of
;; all solutions to the problem of placing nn queens on an n×nn×n chessboard.
;; Queens has an internal procedure queen-cols that returns the sequence of all
;; ways to place queens in the first kk columns of the board.

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerate-interval 
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; In this procedure rest-of-queens is a way to place k−1k−1 queens in the first
;; k−1k−1 columns, and new-row is a proposed row in which to place the queen for
;; the kthkth column. Complete the program by implementing the representation
;; for sets of board positions, including the procedure adjoin-position, which
;; adjoins a new row-column position to a set of positions, and empty-board,
;; which represents an empty set of positions. You must also write the procedure
;; safe?, which determines for a set of positions, whether the queen in the
;; kthkth column is safe with respect to the others. (Note that we need only
;; check whether the new queen is safe—the other queens are already guaranteed
;; safe with respect to each other.)

(define empty-board '())

(define (make-pos row col)
  (cons row col))

(define row car)
(define col cdr)

(define (adjoin-position row col positions)
  (cons (make-pos row col) positions))


(define (same-row? pos1 pos2)
  (= (row pos1) (row pos2)))

(define (same-col? pos1 pos2)
  (= (col pos1) (col pos2)))

(define (same-diag? pos1 pos2)
  (or (= (+ (row pos1) (col pos1))
         (+ (row pos2) (col pos2)))
      (= (- (row pos1) (col pos1))
         (- (row pos2) (col pos2)))))

(define (safe? k positions)
  ;; We assume the kth one is the first one anyway
  (let ((new-queen (car positions)))
    (accumulate (lambda (pos safe)
                  (and safe
                       (and (not (same-row? pos new-queen))
                            (not (same-col? pos new-queen))
                            (not (same-diag? pos new-queen)))))
                #t (cdr positions))))


;; Exercise 2.43: Louis Reasoner is having a terrible time doing Exercise 2.42.
;; His queens procedure seems to work, but it runs extremely slowly. (Louis
;; never does manage to wait long enough for it to solve even the 6×66×6 case.)
;; When Louis asks Eva Lu Ator for help, she points out that he has interchanged
;; the order of the nested mappings in the flatmap, writing it as

;; (flatmap
;;  (lambda (new-row)
;;    (map (lambda (rest-of-queens)
;;           (adjoin-position 
;;            new-row k rest-of-queens))
;;         (queen-cols (- k 1))))
;;  (enumerate-interval 1 board-size))

;; Explain why this interchange makes the program run slowly. Estimate how long
;; it will take Louis’s program to solve the eight-queens puzzle, assuming that
;; the program in Exercise 2.42 solves the puzzle in time T.

;; Instead of solving the puzzle for k-1 before solving the k case, Louis'
;; program grows the solution backwards and solves the k-1 case 8 times for
;; every row, ad nauseam. It will take 8!*T time to solve (I am not sure of
;; this!)
