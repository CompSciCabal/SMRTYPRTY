#lang racket
(print-as-expression #f)
;(pretty-print-abbreviate-read-macros #f)
(define-syntax example
  (syntax-rules ()
    ((_ e) (begin (newline)
                  (pretty-print 'e)
                  (displayln "==>")
                  (pretty-print e)))))

(define (list-permutations xs)
  (define (insert x ys)
    (cons (cons x ys)
          (if (null? ys) '()
            (map (lambda (zs) (cons (car ys) zs)) (insert x (cdr ys))))))
  (define (insert* x yss)
    (append* (map (lambda (ys) (insert x ys)) yss)))
  (if (null? xs) '(())
    (insert* (car xs) (list-permutations (cdr xs)))))

(example (list-permutations '()))
(example (list-permutations '(1)))
(example (list-permutations '(1 2 3)))

(define (orderings xs)
  (if (and (pair? xs) (pair? (cdr xs)))
    (list xs (reverse xs))
    (list xs)))
(define (subset? xs ys) (andmap (lambda (x) (member x ys)) xs))

(struct op (action preconds add-list del-list) #:prefab)

(define (executing? x)
  (and (pair? x) (eq? 'executing (car x))))

(define (gps state goals ops)
  (define state2 (achieve-all ops state goals '()))
  (and state2 (filter executing? state2)))

(define (achieve-all ops state goals goal-stack)
  (ormap (lambda (goals) (achieve-each ops state goals goal-stack))
         (orderings goals)))

;(define (achieve-each ops state goals goal-stack)
  ;(define state2 (foldl (lambda (g state)
                          ;(and state (achieve ops state g #f goal-stack)))
                        ;state goals))
  ;(and state2 (subset? goals state2) state2))
;; For exercise 4.4.
(define (achieve-each ops state goals goal-stack)
  (define state2 (if (null? goals) state
                   (achieve ops state (car goals) (cdr goals) goal-stack)))
  (and state2 (subset? goals state2) state2))

(define (achieve ops state goal goals goal-stack)
  (cond ((member goal goal-stack) #f)
        ;((member goal state)      state)
                                  ;; For exercise 4.4.
        ((member goal state)      (achieve-all ops state goals goal-stack))
        (else (ormap (lambda (op)
                       (and
                         ;; This new condition solves exercise 4.3 part 3.
                         (not (ormap (lambda (g) (member g goal-stack))
                                     (op-add-list op)))
                         (let ((state (apply-op ops state goal op goal-stack)))

                           (and state
                                ;; This solves the taxi problem for exercise 4.4.
                                (achieve-all ops state goals goal-stack)
                                ))))
                     (appropriate-ops ops goal state)))))

(define (appropriate-ops ops goal state)
  (define aops (filter (lambda (op) (appropriate? goal op)) ops))
  (define (unfulfilled op)
    (apply + (map (lambda (pc) (if (member pc state) 0 1))
                  (op-preconds op))))
  (map cdr (sort (map (lambda (op) (cons (unfulfilled op) op)) aops)
                 (lambda (a b) (< (car a) (car b))))))

(define (appropriate? goal op) (member goal (op-add-list op)))

(define (apply-op ops state goal op goal-stack)
  (define state2 (achieve-all ops state (op-preconds op)
                              (cons goal goal-stack)))
  (and state2
       (append (filter-not (lambda (x) (member x (op-del-list op))) state2)
               (cons (list 'executing (op-action op)) (op-add-list op)))))

(define school-ops
  (list (op 'drive-son-to-school
            '(son-at-home car-works)
            '(son-at-school)
            '(son-at-home))
        (op 'shop-installs-battery
            '(car-needs-battery shop-knows-problem shop-has-money)
            '(car-works)
            '())
        (op 'tell-shop-problem
            '(in-communication-with-shop)
            '(shop-knows-problem)
            '())
        (op 'telephone-shop
            '(know-phone-number)
            '(in-communication-with-shop)
            '())
        (op 'look-up-number
            '(have-phone-book)
            '(know-phone-number)
            '())
        (op 'give-shop-money
            '(have-money)
            '(shop-has-money)
            '(have-money))))

(example (gps '(son-at-home car-needs-battery have-money have-phone-book)
              '(son-at-school)
              school-ops))
(example (gps '(son-at-home car-needs-battery have-money have-phone-book)
              '(have-money son-at-school)
              school-ops))
(example (gps '(son-at-home car-needs-battery have-money)
              '(son-at-school)
              school-ops))
(example (gps '(son-at-home car-works)
              '(son-at-school)
              school-ops))

(define banana-ops
  (list (op 'climb-on-chair
            '(chair-at-middle-room at-middle-room on-floor)
            '(at-bananas on-chair)
            '(at-middle-room on-floor))
        (op 'push-chair-from-door-to-middle-room
            '(chair-at-door at-door)
            '(chair-at-middle-room at-middle-room)
            '(chair-at-door at-door))
        (op 'walk-from-door-to-middle-room
            '(at-door on-floor)
            '(at-middle-room)
            '(at-door))
        (op 'grasp-bananas
            '(at-bananas empty-handed)
            '(has-bananas)
            '(empty-handed))
        (op 'drop-ball
            '(has-ball)
            '(empty-handed)
            '(has-ball))
        (op 'eat-bananas
            '(has-bananas)
            '(empty-handed not-hungry)
            '(has-bananas hungry))))

(example (gps '(at-door on-floor has-ball hungry chair-at-door)
              '(not-hungry)
              banana-ops))

(define (make-maze-ops graph)
  (define (maze-op start end)
    (op `(move from ,start to ,end)
        `((at ,start))
        `((at ,end))
        `((at ,start))))
  (define (maze-ops/edge edge)
    (list (maze-op (car edge)  (cadr edge))
          (maze-op (cadr edge) (car edge))))
  (append* (map maze-ops/edge graph)))

(define maze-ops
  (make-maze-ops
    '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
      (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
      (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))

(example (gps '((at 1)) '((at 25)) maze-ops))

(define (find-path start end ops)
  (define plan (gps `((at ,start)) `((at ,end)) ops))
  (and plan
       (cons start (map (lambda (action) (list-ref (cadr action) 4)) plan))))

(example (find-path 1 25 maze-ops))
(example (find-path 1 1 maze-ops))
(example (equal? (find-path 1 25 maze-ops)
                 (reverse (find-path 25 1 maze-ops))))

;; This is a domain-specific solution for exercise 4.5.
(define (augment-block-goals goals)
  (define overs (append* (map (lambda (g) (match g
                                            (`(,x on ,y) (list x))
                                            (_           '())))
                              goals)))
  (define unders (append* (map (lambda (g) (match g
                                             (`(,x on ,y) (list y))
                                             (_           '())))
                               goals)))
  (define tabled (filter-not (lambda (b) (member b overs)) unders))
  (append (map (lambda (b) `(,b on table)) tabled) goals))

(define (make-block-ops blocks)
  (define (move-op a b c)
    (op `(move ,a from ,b to ,c)
        `((space on ,a) (space on ,c) (,a on ,b))
        (move-ons a b c)
        (move-ons a c b)))
  (define (move-ons a b c)
    (if (eq? b 'table)
      `((,a on ,c))
      `((,a on ,c) (space on ,b))))
  (;reverse  ;; Yeah, some of the (a b c) problems used to fail without this.
   (lambda (xs) xs)
    (append*
      (map (lambda (a)
             (append*
               (map (lambda (b)
                      (if (equal? a b) '()
                        (append*
                          (cons (list (move-op a b 'table)
                                      (move-op a 'table b))
                                (map (lambda (c)
                                       (append (if (or (equal? c a)
                                                       (equal? c b))
                                                 '()
                                                 (list (move-op a b c)))))
                                     blocks))))) blocks))) blocks))))

(example (gps '((a on table) (b on table) (space on a) (space on b) (space on table))
              '((a on b) (b on table))
              (make-block-ops '(a b))))
(example (gps '((a on b) (b on table) (space on a) (space on table))
              '((b on a))
              (make-block-ops '(a b))))
(example (gps '((a on b) (b on c) (c on table) (space on a) (space on table))
              '((b on a) (c on b))
              (make-block-ops '(a b c))))
(example (gps '((a on b) (b on c) (c on table) (space on a) (space on table))
              '((c on b) (b on a))
              (make-block-ops '(a b c))))
(example (gps '((c on a) (a on table) (b on table)
                (space on c) (space on b) (space on table))
              '((c on table))
              (make-block-ops '(a b c))))
(example (gps '((c on a) (a on table) (b on table)
                (space on c) (space on b) (space on table))
              '((c on table) (a on b))
              (make-block-ops '(a b c))))
;; Sussman anomaly.
(example (gps '((c on a) (a on table) (b on table) (space on c)
                (space on b) (space on table))
              '((a on b) (b on c))
              (make-block-ops '(a b c))))
(example (gps '((c on a) (a on table) (b on table) (space on c)
                (space on b) (space on table))
              (augment-block-goals '((a on b) (b on c)))
              (make-block-ops '(a b c))))
;(pretty-print (make-block-ops '(a b)))
;(pretty-print (make-block-ops '(a b c)))

(define school-ops/taxi
  (cons (op 'taxi-son-to-school
            '(son-at-home have-money)
            '(son-at-school)
            '(son-at-home have-money))
        school-ops))

(example (gps '(son-at-home have-money car-works)
              '(son-at-school)
              school-ops/taxi))
(example (gps '(son-at-home have-money car-works)
              '(son-at-school have-money)
              school-ops/taxi))

(define dessert-ops
  (list (op 'eat-ice-cream
            '(have-ice-cream)
            '(ate-dessert)
            '(have-ice-cream))
        (op 'eat-cake
            '(have-cake)
            '(ate-dessert ate-cake)
            '(have-cake))
        (op 'buy-cake
            '(have-money)
            '(bought-cake have-cake)
            '(have-money))
        (op 'get-free-ice-cream
            '(bought-cake ate-cake)
            '(have-ice-cream)
            '())))

(example (gps '(have-money)
              '(ate-dessert)
              dessert-ops))
(example (gps '(have-money)
              '(ate-dessert)
              (reverse dessert-ops)))
