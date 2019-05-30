#lang racket
(print-as-expression #f)
;(pretty-print-abbreviate-read-macros #f)
(define-syntax example
  (syntax-rules ()
    ((_ e) (begin (newline)
                  (pretty-print 'e)
                  (displayln "==>")
                  (pretty-print e)))))

(define (sub* env d)
  (define (loop d) (sub* env d))
  (cond ((assoc d env) => (lambda (old-new) (cdr old-new)))
        ((pair? d)     (cons (loop (car d)) (loop (cdr d))))
        ((vector? d)   (vector-map loop d))
        (else          d)))

(define (var? x)
  (and (symbol? x) (string-prefix? (symbol->string x) "?")))
(define (var-match env pattern input)
  (define existing (assoc pattern env))
  (if existing
    (and (equal? (cdr existing) input) env)
    (cons (cons pattern input) env)))

(define (segment-pattern? pattern)
  (and (pair? pattern) (pair? (car pattern))
       (assoc (car (car pattern)) segment-match-table)))
(define (segment-match env pattern input)
  ((cdr (assoc (car (car pattern)) segment-match-table))
   env pattern input))

(define (single-pattern? pattern)
  (and (pair? pattern)
       (assoc (car pattern) single-match-table)))
(define (single-match env pattern input)
  ((cdr (assoc (car pattern) single-match-table))
   env (cdr pattern) input))

(define (pat-match/env env pattern input)
  (and env
       (cond ((var? pattern)             (var-match env pattern input))
             ((segment-pattern? pattern) (segment-match env pattern input))
             ((single-pattern? pattern)  (single-match env pattern input))
             ((and (pair? pattern) (pair? input))
              (pat-match/env (pat-match/env env (car pattern) (car input))
                             (cdr pattern) (cdr input)))
             (else (and (equal? pattern input) env)))))
(define (pat-match pattern input) (pat-match/env '() pattern input))

(define (match-literal env quoted input)
  (and (equal? (car quoted) input) env))

(define (match-is env var-and-pred input)
  (define vr  (car var-and-pred))
  (define is? (cadr var-and-pred))
  (define e2  (pat-match/env env vr input))
  (and e2 ((eval is? (make-base-namespace)) input) e2))

(define (match-and env patterns input)
  (and env (if (null? patterns) env
             (match-and (pat-match/env env (car patterns) input)
                        (cdr patterns) input))))

(define (match-or env patterns input)
  (and (pair? patterns)
       (or (pat-match/env env (car patterns) input)
           (match-or env (cdr patterns) input))))

(define (match-not env patterns input)
  (and (not (match-or env patterns input)) env))

(define (segment-match-*+ env pattern input min-count)
  (define seg-var (cadar pattern))
  (define pat (cdr pattern))
  (define binding (assoc seg-var env))
  (if binding
    ;; For efficiency as mentioned in exercise 5.13.
    (and (list? binding) (<= min-count (length binding))
         (let loop ((input input) (binding binding))
           (cond ((null? binding) (pat-match/env env pat input))
                 ((pair? binding) (and (pair? input)
                                       (equal? (car binding) (car input))
                                       (loop (cdr input) (cdr binding))))
                 (else            #f))))
    (let loop ((count min-count) (input input) (seg '()))
      (cond ((= 0 count)
             (let loop ((input input) (seg seg))
               (define e2 (pat-match/env env pat input))
               (cond ((and e2       (var-match e2 seg-var (reverse seg))))
                     ((pair? input) (loop (cdr input) (cons (car input) seg)))
                     (else          #f))))
            ((pair? input)
             (loop (- count 1) (cdr input) (cons (car input) seg)))
            (else #f)))))

(define (segment-match-* env pattern input)
  (segment-match-*+ env pattern input 0))

(define (segment-match-+ env pattern input)
  (segment-match-*+ env pattern input 1))

(define (segment-match-? env pattern input)
  (define seg-var (cadar pattern))
  (define pat (cdr pattern))
  ;; Swap these to minimize greed.
  (or (pat-match/env env (cons seg-var pat) input)
      (pat-match/env env pat input)))

(define (match-if env pattern input)
  (and (eval `(let ,(map (lambda (kv) `(,(car kv) ,(cdr kv))) env)
                ,(cadar pattern))
             (make-base-namespace))
       (pat-match/env env (cdr pattern) input)))

(define single-match-table
  `((?quote . ,match-literal)
    (?is    . ,match-is)
    (?or    . ,match-or)
    (?and   . ,match-and)
    (?not   . ,match-not)))
(define segment-match-table
  `((?*  . ,segment-match-*)
    (?+  . ,segment-match-+)
    (??  . ,segment-match-?)
    (?if . ,match-if)))

(example (pat-match '(x = (?is ?n number?)) '(x = 34)))
(example (pat-match '(x = (?is ?n number?)) '(x = x)))
(example (pat-match '(?x (?or < = >) ?y) '(3 < 4)))
(example (pat-match '(x = (?and (?is ?n number?) (?is ?n odd?)))
                    '(x = 3)))
(example (pat-match '(x = (?and (?is ?n number?) (?is ?n odd?)))
                    '(x = 2)))
(example (pat-match '(x = (?and (?is ?n number?) (?is ?n odd?)))
                    '(x = x)))

(example (pat-match '(?x /= (?not ?x)) '(3 /= 4)))
(example (pat-match '(?x > ?y (?if (> ?x ?y))) '(4 > 3)))
(example (pat-match '(a (?* ?x) d) '(a b c d)))
(example (pat-match '(a (?* ?x) (?* ?y) d) '(a b c d)))
(example (pat-match '(a (?* ?x) (?* ?y) ?x ?y) '(a b c d (b c) (d))))

(example (pat-match '(?x ?op ?y is ?z (?if (equal? (?op ?x ?y) ?z)))
                    '(3 + 4 is 7)))
(example (pat-match '(?x ?op ?y (?if (?op ?x ?y)))
                    '(3 > 4)))
(example (pat-match '(?x ?op ?y (?if (?op ?x ?y)))
                    '(3 < 4)))

(define (rule-system rule-match rule-if rule-then action)
  (list rule-match rule-if rule-then action))

(define (rule-based-translator system input rules)
  (define rule-match (car system))
  (define rule-if    (cadr system))
  (define rule-then  (caddr system))
  (define action     (cadddr system))
  (ormap (lambda (rule)
           (define result (rule-match (rule-if rule) input))
           (and result (action result (rule-then rule))))
         rules))

;; Exercise 6.6
(example (pat-match '(?one ?two ?three)          '(?one ?two ?three)))
(example (pat-match '(?one (?quote ?two) ?three) '(?one ?two ?three)))
(example (pat-match '(?one (?quote ?two) ?three) '(?one two ?three)))

;; TODO: use this in chapter 5.
;(define eliza-rule-system
  ;(rule-system pat-match car cdr
               ;(lambda (env responses)
                 ;(sub* (switch-viewpoint env)
                       ;(random-elt responses)))))
;(define (use-eliza-rules input)
  ;(rule-based-translator eliza-rule-system input eliza-rules))

(define debug-search? #f)
;; (set! debug-search? #t) wherever you'd like to step through search examples.
(define debug-proj #f)
(define (debug-search states)
  (when debug-search? (printf ";; States: ~a\n"
                              (if debug-proj (map debug-proj states) states))
    (read-line)))
(define (tree-search states goal? successors combine)
  (debug-search states)
  (cond ((null? states)       #f)
        ((goal? (car states)) (car states))
        (else (tree-search (combine (successors (car states)) (cdr states))
                           goal? successors combine))))

(define (prepend xs ys) (append ys xs))

(define (depth-first-search states goal? successors)
  (tree-search states goal? successors append))
(define (breadth-first-search states goal? successors)
  (tree-search states goal? successors prepend))

(define (binary-tree x) (list (* 2 x) (+ 1 (* 2 x))))
(define (is      value)        (lambda (x) (equal? x value)))
(define (is/proj value proj ?) (lambda (x) (? (proj x) value)))
;(example (depth-first-search '(1) (is 12) binary-tree))
(example (breadth-first-search '(1) (is 12) binary-tree))

(define (finite-binary-tree n)
  (lambda (x)
    (filter-not (lambda (child) (> child n)) (binary-tree x))))
(example (depth-first-search '(1) (is 12) (finite-binary-tree 15)))

(define (diff n) (lambda (x) (abs (- x n))))
;; Exercise 6.9
(define (merge cost ps qs)
  (cond
    ((null? ps)          qs)
    ((null? qs)          ps)
    ((< (cost (car qs))
        (cost (car ps))) (cons (car qs) (merge cost ps (cdr qs))))
    (else                (cons (car ps) (merge cost (cdr ps) qs)))))
(define (sorter cost)
  (lambda (new old)
    ;(sort (append new old) (lambda (a b) (< (cost a) (cost b))))
    (merge cost (sort new (lambda (a b) (< (cost a) (cost b)))) old)))

(define (best-first-search states goal? successors cost)
  (tree-search states goal? successors (sorter cost)))
(example (best-first-search '(1) (is 12) binary-tree (diff 12)))

(define max-fixnum (- (expt 2 62) 1))
(define (price-is-right price)
  (lambda (x) (if (> x price) max-fixnum (- price x))))
(example (best-first-search '(1) (is 12) binary-tree (price-is-right 12)))

(define (beam-search states goal? successors cost beam-width)
  (tree-search states goal? successors
               (lambda (new old)
                 (define sorted ((sorter cost) new old))
                 (if (> beam-width (length sorted))
                   sorted
                   (take sorted beam-width)))))
(example (beam-search '(1) (is 12) binary-tree (price-is-right 12) 2))
;(set! debug-search? #t)
;(example (beam-search '(1) (is 12) binary-tree (diff 12) 2))
(example (beam-search '(1) (is 12) binary-tree (diff 12) 3))

(define (city-name c) (car c))
(define (city-long c) (cadr c))
(define (city-lat c)  (caddr c))
(define cities
  ;; (name longitude latitude)
  '((Atlanta       84.23 33.45) (Los-Angeles   118.15 34.03)
    (Boston        71.05 42.21) (Memphis        90.03 35.09)
    (Chicago       87.37 41.50) (New-York       73.58 40.47)
    (Denver       105.00 39.45) (Oklahoma-City  97.28 35.26)
    (Eugene       123.05 44.03) (Pittsburgh     79.57 40.27)
    (Flagstaff    111.41 35.13) (Quebec         71.11 46.49)
    (Grand-Jct    108.37 39.05) (Reno          119.49 39.30)
    (Houston      105.00 34.00) (San-Francisco 122.26 37.47)
    (Indianapolis  86.10 39.46) (Tampa          82.27 27.57)
    (Jacksonville  81.40 30.22) (Victoria      123.21 48.25)
    (Kansas-City   94.35 39.06) (Wilmington     77.57 34.14)))

(define (neighbors city)
  (filter (lambda (c) (and (not (equal? c city))
                           (< (air-distance c city) 1000.0)))
          cities))

(define (city name) (assoc name cities))

(define (trip1 start dest)
  (beam-search (list start) (is dest) neighbors
               (lambda (c) (air-distance c dest)) 1))

(define earth-diameter 12765.0)
(define (air-distance c1 c2)
  (define d (distance (xyz-coords c1) (xyz-coords c2)))
  (* earth-diameter (asin (/ d 2))))
(define (xyz-coords city)
  (define psi (deg->radians (city-lat city)))
  (define phi (deg->radians (city-long city)))
  (list (* (cos psi) (cos phi))
        (* (cos psi) (sin phi))
        (sin psi)))
(define (distance p1 p2)
  (sqrt (foldl + 0 (map (lambda (a b) (expt (- a b) 2)) p1 p2))))
(define (deg->radians deg)
  (define trunc (truncate deg))
  (* (+ trunc (* (- deg trunc) 100/60)) pi 1/180))

(example (trip1 (city 'San-Francisco) (city 'Boston)))
(example (trip1 (city 'Boston) (city 'San-Francisco)))

(define (path state previous cost-so-far total-cost)
  (vector state previous cost-so-far total-cost))
(define (path-state       p) (vector-ref p 0))
(define (path-previous    p) (vector-ref p 1))
(define (path-cost-so-far p) (vector-ref p 2))
(define (path-total-cost  p) (vector-ref p 3))
(define (path/state state)   (vector state #f 0 0))

(define (trip2 start dest beam-width)
  (beam-search (list (path/state start))
               (is/proj dest path-state equal?)
               (path-saver neighbors air-distance
                           (lambda (c) (air-distance c dest)))
               path-total-cost
               beam-width))

(define (path-saver successors cost cost-remaining)
  (lambda (old-path)
    (define old-state (path-state old-path))
    (map (lambda (new-state)
           (define old-cost (+ (path-cost-so-far old-path)
                               (cost old-state new-state)))
           (path new-state old-path old-cost
                 (+ old-cost (cost-remaining new-state))))
         (successors old-state))))
(define (show-path path)
  (format "#<Path to ~a cost ~a>" (path-state path)
          (~r (path-total-cost path) #:precision 1)))
(define (show-city-path path)
  (define names (reverse (map-path city-name path)))
  (format "#<Path ~a km: ~a~a>" (path-total-cost path)
          (car names)
          (string-append*
            (map (lambda (n) (string-append " - " (symbol->string n)))
                 (cdr names)))))
(define (map-path f path)
  (if path (cons (f (path-state path))
                 (map-path f (path-previous path)))
    '()))

;(set! debug-search? #t)
(set! debug-proj show-path)
(example (show-city-path (trip2 (city 'San-Francisco) (city 'Boston) 1)))
(example (show-city-path (trip2 (city 'Boston) (city 'San-Francisco) 1)))
(example (show-city-path (trip2 (city 'Boston) (city 'San-Francisco) 3)))
(set! debug-proj #f)

(define (iter-wide-search states goal? successors cost width max-width)
  (when debug-search? (printf "; Width: ~a\n" width))
  (unless (> width max-width)
    (or (beam-search states goal? successors cost width)
        (iter-wide-search states goal? successors cost
                          (+ width 1) max-width))))

(example (iter-wide-search '(1) (is 12) (finite-binary-tree 15) (diff 12)
                           1 100))

(define (adjoin x xs =?)
  (if (memf (lambda (y) (=? x y)) xs)
    xs
    (cons x xs)))

(define (graph-search states goal? successors combine state=? old-states)
  (debug-search states)
  (cond ((null? states)       #f)
        ((goal? (car states)) (car states))
        (else (graph-search
                (combine (new-states states successors state=? old-states)
                         (cdr states))
                goal? successors combine state=?
                (adjoin (car states) old-states state=?)))))

(define (new-states states successors state=? old-states)
  (define (mem state states) (memf (lambda (s) (state=? s state)) states))
  (filter-not (lambda (state) (or (mem state states) (mem state old-states)))
              (successors (car states))))

(define (next2 x) (list (+ x 1) (+ x 2)))

(define (a*-search paths goal? successors cost cost-left state=? old-paths)
  (debug-search (map show-path paths))
  (cond ((null? paths)                    #f)
        ((goal? (path-state (car paths))) (car paths))
        (else
          (let* ((p (car paths)) (state (path-state p)))
            (let loop0 ((states (successors state))
                        (paths (cdr paths))
                        (old-paths (insert-path p old-paths)))
              (define (loop paths old-paths)
                (loop0 (cdr states) paths old-paths))
              (if (null? states) (a*-search paths goal? successors
                                            cost cost-left state=? old-paths)
                (let* ((state2 (car states))
                       (cost (+ (path-cost-so-far p) (cost state state2)))
                       (cost2 (cost-left state2))
                       (p2 (path state2 p cost (+ cost cost2))))
                  (cond
                    ((find-path state2 paths state=?)
                     => (lambda (old)
                          (if (path<? p2 old)
                            (loop (insert-path p2 (remove old paths))
                                  old-paths)
                            (loop paths old-paths))))
                    ((find-path state2 old-paths state=?)
                     => (lambda (old)
                          (if (path<? p2 old)
                            (loop (insert-path p2 paths)
                                  (remove old old-paths))
                            (loop paths old-paths))))
                    (else (loop (insert-path p2 paths) old-paths))))))))))

(define (find-path state paths state=?)
  (findf (lambda (p) (state=? (path-state p) state)) paths))

(define (path<? p1 p2) (< (path-total-cost p1) (path-total-cost p2)))

(define (merge-paths ps qs)
  (cond
    ((null? ps)                 qs)
    ((null? qs)                 ps)
    ((path<? (car qs) (car ps)) (cons (car qs) (merge-paths ps (cdr qs))))
    (else                       (cons (car ps) (merge-paths (cdr ps) qs)))))

(define (insert-path p paths) (merge-paths (list p) paths))

(define (path-states p) (map-path (lambda (x) x) p))

(example (tree-search  '(1) (is 6) next2 prepend))
(example (graph-search '(1) (is 6) next2 prepend equal? '()))
(example (path-states (a*-search (list (path/state 1)) (is 6) next2
                                 (lambda (x y) 1) (diff 6) equal? '())))

(define (search-all start goal? successors cost beam-width)
  (define solutions '())
  (beam-search (list start)
               (lambda (x)
                 (when (goal? x) (set! solutions (cons x solutions)))
                 #f)
               successors cost beam-width)
  solutions)


(define (subset? xs ys) (andmap (lambda (x) (member x ys)) xs))

(define (executing? x) (and (pair? x) (eq? 'executing (car x))))

(struct op (action preconds add-list del-list) #:prefab)
(define *ops* #f)

(define (search-gps ops start goal beam-width)
  (define (cost state)
    (+ (length (filter executing? state))
       (length (filter (lambda (con) (not (member con state)))
                       goal))))
  (define old-ops *ops*)
  (set! *ops* ops)
  (define result
    (beam-search (list start) (lambda (state) (subset? goal state))
                 gps-successors cost beam-width))
  (set! *ops* old-ops)
  (and result (filter executing? result)))

(define (gps-successors state)
  (map (lambda (op)
         (append (filter-not (lambda (x) (member x (op-del-list op))) state)
                 (cons (list 'executing (op-action op)) (op-add-list op))))
       (applicable-ops state)))

(define (applicable-ops state)
  (filter (lambda (op) (subset? (op-preconds op) state)) *ops*))

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
                                   blocks))))) blocks))) blocks)))

(define start '((c on a) (a on table) (b on table) (space on c)
                (space on b) (space on table)))
(example (search-gps (make-block-ops '(a b c)) start '((a on b) (b on c)) 10))
(example (search-gps (make-block-ops '(a b c)) start '((b on c) (a on b)) 10))

;; Exercise 6.11
(define (beam-search/n n start goal? successors cost beam-width)
  (define solutions '())
  (when (< 0 n)
    (beam-search (list start)
                 (lambda (x)
                   (and (goal? x)
                        (set! n (- n 1))
                        (set! solutions (cons x solutions))
                        (= 0 n)
                        x))
                 successors cost beam-width))
  solutions)

(example (beam-search/n 1 1 (is 12) binary-tree (price-is-right 12) 2))
;(example (beam-search/n 2 1 (is 12) binary-tree (price-is-right 12) 2))
(example (beam-search/n 5 1 (is 6) next2 (price-is-right 6) 20))

;; Exercise 6.15
(define (search-gps-backwards ops start goal beam-width)
  (define (cost state)
    (+ (length (filter (lambda (con) (not (member con state))) start))
       (- (length state)
          (length (filter (lambda (con) (member con state)) start)))))
  (define old-ops *ops*)
  (set! *ops* ops)
  (define result
    (beam-search (list goal)
                 (lambda (state)
                   (define non-actions (filter-not executing? state))
                   (and (subset? start non-actions)
                        (subset? non-actions start)))
                 gps-backwards-successors cost beam-width))
  (set! *ops* old-ops)
  (and result (filter executing? result)))

(define (gps-backwards-successors state)
  (map (lambda (op)
         (append (cons (list 'executing (op-action op))
                       (filter-not (lambda (x) (member x (op-add-list op)))
                                   state))
                 (foldl (lambda (p acc) (if (member p acc) acc (cons p acc)))
                        (op-del-list op) (op-preconds op))))
       (applicable-ops-backwards state)))

(define (applicable-ops-backwards state)
  (filter (lambda (op)
            (null? (filter (lambda (x) (member x state)) (op-del-list op))))
          *ops*))

(example (search-gps-backwards
           (make-block-ops '(a b c)) start '((a on b) (b on c)) 10))
(example (search-gps-backwards
           (make-block-ops '(a b c)) start '((b on c) (a on b)) 10))
