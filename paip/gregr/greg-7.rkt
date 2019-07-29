#lang racket
(require "tools.rkt")
(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)
(define-syntax example
  (syntax-rules ()
    ((_ e) (begin (newline)
                  (pretty-print 'e)
                  (displayln "==>")
                  (pretty-print e)))))


(define (rule pattern response) (list pattern response))
(define (rule-pattern r)        (car r))
(define (rule-response r)       (cadr r))

(define (expr lhs op rhs) (list op lhs rhs))
(define (expr? x)         (pair? x))
(define (expr-op x)       (car x))
(define (expr-lhs x)      (cadr x))
(define (expr-rhs x)      (caddr x))
(define (expr-args x)     (cdr x))
(define (binary-expr? x)  (and (expr? x) (= (length (expr-args x)) 2)))
(define (in-expr? x e)
  (or (eqv? x e)
      (and (expr? e) (or (in-expr? x (expr-lhs e))
                         (in-expr? x (expr-rhs e))))))
(define (prefix->infix e)
  (if (expr? e)
    (map prefix->infix
         (if (binary-expr? e) (list (expr-lhs e) (expr-op e) (expr-rhs e)) e))
    e))

(define student-rules
  '((((?* ?x) |.|)                                ?x)
    (((?* ?x) |.| (?* ?y))                    (?x ?y))
    ((if (?* ?x) |,| then (?* ?y))            (?x ?y))
    ((if (?* ?x) then (?* ?y))                (?x ?y))
    ((if (?* ?x) |,| (?* ?y))                 (?x ?y))
    (((?* ?x) |,| and (?* ?y))                (?x ?y))
    ((find (?* ?x) and (?* ?y))               ((= to-find-1 ?x) (= to-find-2 ?y)))
    ((find (?* ?x))                           (= to-find-1 ?x))
    (((?* ?x) equals (?* ?y))                 (= ?x ?y))
    (((?* ?x) same as (?* ?y))                (= ?x ?y))
    (((?* ?x) = (?* ?y))                      (= ?x ?y))
    (((?* ?x) is equal to (?* ?y))            (= ?x ?y))
    (((?* ?x) is (?* ?y))                     (= ?x ?y))
    (((?* ?x) has (?* ?y))                    (= ?x ?y))
    (((?* ?x) can (?* ?y))                    (= ?x ?y))
    (((?* ?x) - (?* ?y))                      (- ?x ?y))
    (((?* ?x) minus (?* ?y))                  (- ?x ?y))
    ((difference between (?* ?x) and (?* ?y)) (- ?x ?y))
    ((difference (?* ?x) and (?* ?y))         (- ?x ?y))
    (((?* ?x) + (?* ?y))                      (+ ?x ?y))
    (((?* ?x) plus (?* ?y))                   (+ ?x ?y))
    ((sum (?* ?x) and (?* ?y))                (+ ?x ?y))
    ((product (?* ?x) and (?* ?y))            (* ?x ?y))
    (((?* ?x) * (?* ?y))                      (* ?x ?y))
    (((?* ?x) times (?* ?y))                  (* ?x ?y))
    (((?* ?x) / (?* ?y))                      (/ ?x ?y))
    (((?* ?x) per (?* ?y))                    (/ ?x ?y))
    (((?* ?x) divided by (?* ?y))             (/ ?x ?y))
    ((half (?* ?x))                           (/ ?x 2))
    ((one half (?* ?x))                       (/ ?x 2))
    ((twice (?* ?x))                          (* 2 ?x))
    ((square (?* ?x))                         (* ?x ?x))
    (((?* ?x) % less than (?* ?y))            (* ?y (/ (- 100 ?x) 100)))
    (((?* ?x) % more than (?* ?y))            (* ?y (/ (+ 100 ?x) 100)))
    (((?* ?x) % (?* ?y))                      (* (/ ?x 100) ?y))
    ))

(define (student words)
  (solve-equations
    (create-list-of-equations
      (translate-to-expression (filter-not noise-word? words)))))

;; TODO: import rule-based-translator.
(define (translate-to-expression words)
  (or (rule-based-translator
        (rule-system
          pat-match rule-pattern rule-response
          (lambda (bindings response)
            (sub* (map translate-pair bindings) response)))
        words student-rules)
      (make-variable words)))

(define (translate-pair pair)
  (cons (car pair) (translate-to-expression (cdr pair))))

(define (create-list-of-equations e)
  (cond ((null? e)             '())
        ((not (pair? e))       '())
        ((not (pair? (car e))) (list e))
        (else (append (create-list-of-equations (car e))
                      (create-list-of-equations (cdr e))))))

(define (make-variable words)
  ;; TODO: do better for exercise 7.3.
  (car words))

(define (noise-word? word)
  (member word '(a an the this number of $)))

;; Exercise 7.1
(define (print-equations header equations)
  (printf "~a\n" header)
  (for-each (lambda (e) (printf "  ~s\n" e)) (map prefix->infix equations)))

(define (solve-equations equations)
  (print-equations "The equations to be solved are: " equations)
  (define solution (solve equations '()))
  (print-equations "The solution is: " solution)
  solution)

(define (solve equations known)
  (or (ormap (lambda (equation)
               (define x (one-unknown equation))
               (and x (let ((answer (solve-arithmetic (isolate equation x))))
                        (solve (sub* (list (cons (expr-lhs answer)
                                                 (expr-rhs answer)))
                                     (remove equation equations))
                               (cons answer known)))))
             equations)
      known))

(define operator-inverses
  '((+ . -)
    (- . +)
    (* . /)
    (/ . *)
    (= . =)))

(define (inverse-op op) (cdr (assoc op operator-inverses)))

(define (commutative? op) (member op '(+ * =)))

(define (eval-arithmetic e)
  (define op (and (expr? e) (expr-op e)))
  (define (? o) (eqv? op o))
  (define (@ proc) (apply proc (map eval-arithmetic (expr-args e))))
  (cond ((not op)   e)
        ((? '+) (@ +))
        ((? '-) (@ -))
        ((? '*) (@ *))
        ((? '/) (@ /))
        (error "unknown operator:" op)))

(define (solve-arithmetic e)
  (expr (expr-lhs e) '= (eval-arithmetic (expr-rhs e))))

(define (unknown? e) (symbol? e))

(define (no-unknown? e)
  (and (not (unknown? e))
       (or (not (expr? e))
           (and (no-unknown? (expr-lhs e)) (no-unknown? (expr-rhs e))))))

;; Exercise 7.9
(define (unknowns e)
  (if (unknown? e) e
    (and (expr? e)
         (let ((l (unknowns (expr-lhs e))))
           (cond ((eqv? l 2) 2)
                 ((not l) (unknowns (expr-rhs e)))
                 ((no-unknown? (expr-rhs e)) l)
                 (else 2))))))
(define (one-unknown e)
  (define u (unknowns e))
  (and (not (eqv? u 2)) u))

(define (isolate e x)
  (cond ((eq? (expr-lhs e) x)
         ;; X = A -> X = n
         e)
        ((in-expr? x (expr-rhs e))
         ;; A = f(X) -> f(X) = A
         (isolate (expr (expr-rhs e) '= (expr-lhs e)) x))
        ((in-expr? x (expr-lhs (expr-lhs e)))
         ;; f(X)*A = B -> f(X) = B/A
         (isolate (expr (expr-lhs (expr-lhs e)) '=
                        (expr (expr-rhs e)
                              (inverse-op (expr-op (expr-lhs e)))
                              (expr-rhs (expr-lhs e)))) x))
        ((commutative? (expr-op (expr-lhs e)))
         ;; A*f(X) = B -> f(X) = B/A
         (isolate (expr (expr-rhs (expr-lhs e)) '=
                        (expr (expr-rhs e)
                              (inverse-op (expr-op (expr-lhs e)))
                              (expr-lhs (expr-lhs e)))) x))
        (else
          ;; A/f(X) = B -> f(x) = A/B
          (isolate (expr (expr-rhs (expr-lhs e)) '=
                         (expr (expr-lhs (expr-lhs e))
                               (expr-op (expr-lhs e))
                               (expr-rhs e))) x))))

(example (solve-equations '((= (+ 3 4) (* (- 5 (+ 2 x)) 7))
                            (= (+ (* 3 x) y) 12))))
(example (student '(if the number of customers Tom gets is twice the square of
                    20 % of the number of advertisements he runs |,|
                    and the number of advertisements is 45 |,|
                    then what is the number of customers Tom gets ?)))
(example (student '(the daily cost of living for a group is the overhead cost plus
                    the running cost for each person times the number of people in
                    the group |.|  this cost for one group equals $ 100 |,|
                    and the number of people in the group is 40 |.|
                    if the overhead cost is 10 times the running cost |,|
                    find the overhead and running cost for each person |.|)))
(example (student '(Fran's age divided by Robin's height is one half Kelly's IQ |.|
                    Kelly's IQ minus 80 is Robin's height |.|
                    if Robin is 4 feet tall |,| how old is Fran ?)))
(example (student '(Fran's age divided by Robin's height is one half Kelly's IQ |.|
                    Kelly's IQ minus 80 is Robin's height |.|
                    if Robin is 0 feet tall |,| how old is Fran ?)))
;(example (student '(Fran's age times Robin's height is one half Kelly's IQ |.|
                    ;Kelly's IQ minus 80 is Robin's height |.|
                    ;if Robin is 0 feet tall |,| how old is Fran ?)))

;; Exercise 7.6
(example (student '(the price of a radio is 69.70 dollars |.|
                    if this price is 15 % less than the marked price |,| find
                    the marked price |.|)))
(example (student '(the number of soldiers the Russians have is one half of the
                    number of guns they have |.| the number of guns they have
                    is 7000 |.| what is the number of soldiers they have ?)))
(example (student '(if the number of customers Tom gets is twice the square of
                    20 % of the number of advertisements he runs |,| and the
                    number of advertisements is 45 |,| and the profit Tom
                    receives is 10 times the number of customers he gets |,|
                    then what is the profit ?)))
(example (student '(the average score is 73 |.| the maximum score is 97 |.|
                    what is the square of the difference between the average
                    and the maximum ?)))
(example (student '(Tom is twice Mary's age |,| and Jane's age is half the
                    difference between Mary and Tom |.| if Mary is 18 years
                    old |,| how old is Jane ?)))
(example (student '(what is 4 + 5 * 14 / 7 ?)))
(example (student '(x * b = c + d |.| b * c = x |.| x = b + b |.| b = 5 |.|)))

(example (student '(Werner has 3 apples |.|
                    if the number of apples barfed into Werner's lap by a surfacing whale is 18 |,|
                    how many apples can Werner eat ?)))
