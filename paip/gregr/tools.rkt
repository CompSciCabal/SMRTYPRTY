#lang racket/base
(provide
  subset?
  adjoin
  sub*
  pat-match
  rule-system
  rule-based-translator
  is
  is/proj
  tree-search
  depth-first-search
  breadth-first-search
  best-first-search
  beam-search
  beam-search/n
  search-all
  iter-wide-search
  graph-search
  a*-search
  )
(require racket/format racket/list racket/string racket/vector)

(define (subset? xs ys) (andmap (lambda (x) (member x ys)) xs))

;; Pattern matching
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

;; Search
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

(define (is      value)        (lambda (x) (equal? x value)))
(define (is/proj value proj ?) (lambda (x) (? (proj x) value)))

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

(define (beam-search states goal? successors cost beam-width)
  (tree-search states goal? successors
               (lambda (new old)
                 (define sorted ((sorter cost) new old))
                 (if (> beam-width (length sorted))
                   sorted
                   (take sorted beam-width)))))

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

(define (search-all start goal? successors cost beam-width)
  (define solutions '())
  (beam-search (list start)
               (lambda (x)
                 (when (goal? x) (set! solutions (cons x solutions)))
                 #f)
               successors cost beam-width)
  solutions)

(define (path state previous cost-so-far total-cost)
  (vector state previous cost-so-far total-cost))
(define (path-state       p) (vector-ref p 0))
(define (path-previous    p) (vector-ref p 1))
(define (path-cost-so-far p) (vector-ref p 2))
(define (path-total-cost  p) (vector-ref p 3))
(define (path/state state)   (vector state #f 0 0))
(define (map-path f path)
  (if path (cons (f (path-state path))
                 (map-path f (path-previous path)))
    '()))
(define (show-path path)
  (format "#<Path to ~a cost ~a>" (path-state path)
          (~r (path-total-cost path) #:precision 1)))

(define (iter-wide-search states goal? successors cost width max-width)
  (when debug-search? (printf "; Width: ~a\n" width))
  (unless (> width max-width)
    (or (beam-search states goal? successors cost width)
        (iter-wide-search states goal? successors cost
                          (+ width 1) max-width))))

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
