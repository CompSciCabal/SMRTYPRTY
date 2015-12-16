; Data directed dispatch table
; ============================

; See section 3.3 https://sarabander.github.io/sicp/html/3_002e3.xhtml#DOCF152 and
; Chapter 2 for details. 

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
                       (if record (cdr record) #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
                       (if record
                           (set-cdr! record value)
                           (set-cdr! subtable (cons (cons key-2 value) (cdr subtable)))))
            (set-cdr! local-table (cons (list key-1 (cons key-2 value)) (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'get) lookup)
            ((eq? m 'put) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

(define table (make-table))

(define operation-table (make-table))
(define get (operation-table 'get))
(define put (operation-table 'put))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

; Stream stuff
; ============

(define the-empty-stream '())

(define (display-line x)
  (newline)
  (display x))

(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))

(define (cons-stream a b) (cons a (delay b)))

(define (stream-car stream) 
  (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define (stream-null? s) (equal? s the-empty-stream))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))
(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream 
       (proc (stream-car s))
       (stream-map proc (stream-cdr s)))))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (stream-append (stream-cdr s1) s2))))

(define (display-stream s) (stream-for-each display-line s))

; 4.4.4.1 The driver loop
; =======================

(define input-prompt "? ")
(define output-prompt "> ")

#| "When the query is instantiated, any variables that remain unbound are transformed
back to theinput representation before being printed. These transformations are
performed by the two procedures query-syntax-process and contract-question-mark
(4.4.4.7)."

^ This maybe the variable name munging that Ableson mentions in the video, to
prevent variable name collisions. |#
(define (query-driver-loop)
  (prompt-for-input input-prompt)
  ; "Before doing any processing on an input expression, the driver loop transforms
  ; it syntactically into a form that makes the processing more efficient. "
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q) (add-rule-or-assertion! (add-assertion-body q))
                                      (newline)
                                      (display "Assertion added to database.")
                                      (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             ; this lambda instantiates the query with a frame from the stream? 
             (lambda (frame) (instantiate q frame (lambda (var frame) (contract-question-mark var)))) ; 
             ; this kicks off the process by evaling the query without any constraints: 
             (qeval q (singleton-stream '()))))
           (query-driver-loop)))))

(define (prompt-for-input string)
  (newline) (newline) 
  (display string) (newline))

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp) (cons (copy (car exp))
                             (copy (cdr exp))))
          (else exp)))
  (copy exp))

; 4.4.4.2: The Evaluator
; ======================

#| The qeval procedure, called by the query-driver-loop, is the basic evaluator
of the query system. It takes as inputs a query and a stream of frames, and
it returns a stream of extended frames. It identifies special forms by a
data-directed dispatch using get and put. Any query that is not identified
as a special form is assumed to be a simple query, to be processed by simple-query. |#
(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

#| Simple-query takes as arguments a simple query (a pattern) together with a stream of frames,
and it returns the stream formed by extending each frame by all data-base matches
of the query. |#
(define (simple-query query-pattern frame-stream)
  (stream-flatmap (lambda (frame)
                    (stream-append-delayed (find-assertions query-pattern frame)
                                           (delay (apply-rules query-pattern frame))))
                  frame-stream))

#| Conjoin processes the stream of frames to find the stream of all possible frame extensions
that satisfy the first query in the conjunction. Then, using this as the new frame stream,
it recursively applies conjoin to the rest of the queries. |#
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts) frame-stream))))

; The expression
(put 'and 'qeval conjoin)
;sets up qeval to dispatch to conjoin when an and form is encountered.

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts) frame-stream)))))
(put 'or 'qeval disjoin)

#|We attempt to extend each frame in the input stream to satisfy the query
being negated, and we include a given frame in the output stream only if
it cannot be extended. |#
(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null?
          (qeval (negated-query operands) (singleton-stream frame)))
         (singleton-stream frame)
          the-empty-stream))
   frame-stream))
(put 'not 'qeval negate)

#| Each frame in the stream is used to instantiate the variables in the pattern,
the indicated predicate is applied, and the frames for which the predicate returns
false are filtered out of the input stream. |#
(define (lisp-value test frame-stream)
  ; test will be something like `> ?amount 30000` 
  (stream-flatmap
   (lambda (frame)
     (if (execute (instantiate test frame (lambda (var frame) (error "Unknow pattern var: LISP-VALUE" var))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))
(put 'lisp-value 'qeval lisp-value)

#| "Execute must eval the predicate expression to get the procedure to apply. However,
it must not evaluate the arguments, since they are already the actual arguments, not
expressions whose evaluation (in Lisp) will produce the arguments."

Ie: `> ?amount 30000` |#
(define user-initial-environment (scheme-report-environment 5))

(define (execute exp)
  (apply
   (eval (predicate exp) user-initial-environment)
   (args exp)))

#| always-true is used by the rule-body selector (4.4.4.7) to provide bodies for rules
that were defined without bodies |#
(define (always-true ignore frame-stream) frame-stream)
(put 'always-true 'qeval always-true)

; 4.4.4.3: Finding assertions/pattern matching
; ============================================

#| Find-assertions, called by simple-query, takes as input a pattern and a frame. It returns
a stream of frames, each extending the given one by a data-base match of the given pattern.
It uses fetch-assertions to get a stream of all the assertions in the data base that should
be checked for a match against the pattern and the frame. The reason for fetch-assertions
here is that we can often apply simple tests that will eliminate many of the entries in the
data base from the pool of candidates for a successful match. |#
(define (find-assertions pattern frame)
  (stream-flatmap
   (lambda (datum) (check-an-assertion datum pattern frame))
   (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ; If the pattern and the data are both pairs, we (recursively) match the car of the 
        ; pattern against the car of the data to produce a frame; in this frame we then
        ; match the cdr of the pattern against the cdr of the data.
        ;
        ; Eg: match the list ((a b) c (a b)) against (?x c ?x); or (?x a ?y) and (?y ?z a)
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat) (cdr dat) (pattern-match (car pat) (car dat) frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  ; Is there a binding for this variable?
  (let ((binding (binding-in-frame var frame)))
    (if binding
        #| Match, in the frame, the data against the value of the variable in the frame:
        For example, suppose we have a frame in which ?x is bound to (f ?y) and ?y is
        unbound, and we wish to augment this frame by a binding of ?x to (f b). We
        look up ?x and find that it is bound to (f ?y). This leads us to match (f ?y)
        against the proposed new value (f b) in the same frame. This match extends the
        frame by adding a binding of ?y to b. ?X remains bound to (f ?y).

        *We never modify a stored binding and we never store more than one binding for a
        given variable.* |#
        (pattern-match (binding-value binding) dat frame)
        ; No binding: add the binding of the variable to the data:
        (extend var dat frame))))

; 4.4.4.4: Rules and Unification
; ==============================

#| Apply-rules is the rule analog of find-assertions (4.4.4.3). It takes as input a
pattern and a frame, and it forms a stream of extension frames by applying rules from
the data base. Stream-flatmap maps apply-a-rule down the stream of possibly applicable
rules (selected by fetch-rules, 4.4.4.5) and combines the resulting streams of frames. |#
(define (apply-rules pattern frame)
  (stream-flatmap
   (lambda (rule) (apply-a-rule rule pattern frame))
   (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result (unify-match query-pattern (conclusion clean-rule) query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule) (singleton-stream unify-result))))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp) (make-new-variable exp rule-application-id))
            ((pair? exp) (cons (tree-walk (car exp)) (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

#| The unification algorithm is implemented as a procedure that takes as inputs two patterns
and a frame and returns either the extended frame or the symbol failed. The unifier is like
the pattern matcher except that it is symmetrical—variables are allowed on both sides of the
match. Unify-match is basically the same as pattern-match, except that there is extra code to
handle the case where the object on the right side of the match is a variable. |#

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame))
        ((and (pair? p1) (pair? p2)) (unify-match
                                      (cdr p1)
                                      (cdr p2)
                                      (unify-match (car p1) (car p2) frame)))
        (else 'failed)))

#| In unification, as in one-sided pattern matching, we want to accept a proposed extension
of the frame only if it is consistent with existing bindings. The procedure extend-if-possible
used in unification is the same as the extend-if-consistent used in pattern matching except for
two special checks, marked “***” in the program below. In the first case, if the variable we are
trying to match is not bound, but the value we are trying to match it with is itself a (different)
variable, it is necessary to check to see if the value is bound, and if so, to match its value.
If both parties to the match are unbound, we may bind either to the other. 

|#

(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding (unify-match (binding-value binding) val frame))
          ((var? val) (let ((binding (binding-in-frame val frame)))  ; ***  
                        (if binding
                            (unify-match var (binding-value binding frame))
                            (extend var val frame))))
          ((depends-on? val var frame) 'failed) ; ***  This is for binding that imply a fixed-point equation
          (else (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e) (if (equal? var e)
                        #t
                        (let ((b (binding-in-frame e frame)))
                          (if b
                              (tree-walk (binding-value b))
                              #f))))
          ((pair? e) (or (tree-walk (car e)) (tree-walk (cdr e))))
          (else #f)))
  (tree-walk exp))


; 4.4.4.5: Maintaining the database
; =================================

(define THE-ASSERTIONS the-empty-stream)

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s
        s
        the-empty-stream)))

(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS (cons-stream assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream (get-stream key 'assertion-stream)))
          (put key 'assertion-stream (cons-stream assertion current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream (get-stream key 'rule-stream)))
            (put key 'rule-stream (cons-stream rule current-rule-stream)))))))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat)
  (constant-symbol? (car pat)))

; 4.4.4.6: Stream Operations
; ==========================

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed (stream-cdr s1) delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed (force delayed-s2) (delay (stream-cdr s1))))))

#| Stream-flatmap, which is used throughout the query evaluator to map a procedure over
a stream of frames and combine the resulting streams of frames, is the stream analog of
the flatmap procedure introduced for ordinary lists in 2.2.3. Unlike ordinary flatmap,
however, we accumulate the streams with an interleaving process, rather than simply
appending them (see Exercise 4.72 and Exercise 4.73).

Question: What is flatten-stream doing? |#

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

(define (singleton-stream x)
  (cons-stream x the-empty-stream))


; 4.4.4.7: Query Syntax Procedures
; ================================

(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
  (car (contents exp)))

; The syntax definitions for the and, or, not, and lisp-value special forms:
(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))
(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

; The syntax of rules:
(define (rule? statement) (tagged-list? statement 'rule))
(define (conclusion rule) (cadr rule))
(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

; query-syntax-process transform a pattern from, eg:
; (job ?x ?y) into (job (? x) (? y))
(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp) (cons
                      (map-over-symbols proc (car exp))
                      (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol) 
  ; a symbol: 'foo
  ; a string: "foo"
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '? (string->symbol (substring chars 1 (string-length chars))))
        symbol)))

(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))

; Rule counter, incremented each time a rule is applied, to provide unique
; variable names, preventing name collision:
(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?" (if (number? (cadr variable))
                          (string-append
                           (symbol->string (caddr variable))"-"(number->string (cadr variable)))
                          (symbol->string (cadr variable))))))

; 4.4.4.8 Frames and Bindings
; ===========================

; Frames are represented as lists of bindings, which are variable-value pairs: 
(define (make-binding variable value) (cons variable value))
(define (binding-variable binding) (car binding))
(define (binding-value binding) (cdr binding))
(define (binding-in-frame variable frame) (assoc variable frame))
(define (extend variable value frame) (cons (make-binding variable value) frame))




; Add assertions to DB
; ====================

(define (add assertion)
  (let ((q (query-syntax-process assertion)))
    (add-rule-or-assertion! q))
  (newline)
  (display "Startup assertion added")
  )
(add '(address (Bitdiddle Ben) 
                  (Slumerville (Ridge Road) 10)))
(add '(job (Bitdiddle Ben) (computer wizard)))                  
(add '(salary (Bitdiddle Ben) 60000))

(add '(address (Hacker Alyssa P) 
                  (Cambridge (Mass Ave) 78)))
(add '(job (Hacker Alyssa P) (computer programmer)))
(add '(salary (Hacker Alyssa P) 40000))
(add '(supervisor (Hacker Alyssa P) (Bitdiddle Ben)))

(add '(address (Fect Cy D) 
                  (Cambridge (Ames Street) 3)))

(add '(rule (lives-near ?person-1 ?person-2)
               (and (address ?person-1 
                             (?town . ?rest-1))
                    (address ?person-2 
                             (?town . ?rest-2))
                    (not (same ?person-1 ?person-2)))))
(add '(rule (same ?x ?x)))

(add '(job (Fect Cy D) (computer programmer)))
(add '(salary (Fect Cy D) 35000))
(add '(supervisor (Fect Cy D) (Bitdiddle Ben)))

(add '(address (Tweakit Lem E) 
                  (Boston (Bay State Road) 22)))
(add '(job (Tweakit Lem E) (computer technician)))
(add '(salary (Tweakit Lem E) 25000))
(add '(supervisor (Tweakit Lem E) (Bitdiddle Ben)))
(add '(address (Reasoner Louis) 
                  (Slumerville (Pine Tree Road) 80)))
(add '(job (Reasoner Louis) 
              (computer programmer trainee)))
(add '(salary (Reasoner Louis) 30000))
(add '(supervisor (Reasoner Louis) 
                     (Hacker Alyssa P)))

(add '(supervisor (Bitdiddle Ben) (Warbucks Oliver)))
(add '(address (Warbucks Oliver) 
                  (Swellesley (Top Heap Road))))
(add '(job (Warbucks Oliver) 
              (administration big wheel)))
(add '(salary (Warbucks Oliver) 150000))

(add '(address (Scrooge Eben) 
                  (Weston (Shady Lane) 10)))
(add '(job (Scrooge Eben) 
              (accounting chief accountant)))
(add '(salary (Scrooge Eben) 75000))
(add '(supervisor (Scrooge Eben) (Warbucks Oliver)))

(add '(address (Cratchet Robert) 
                  (Allston (N Harvard Street) 16)))
(add '(job (Cratchet Robert) (accounting scrivener)))
(add '(salary (Cratchet Robert) 18000))
(add '(supervisor (Cratchet Robert) (Scrooge Eben)))

(add '(address (Aull DeWitt) 
                  (Slumerville (Onion Square) 5)))
(add '(job (Aull DeWitt) (administration secretary)))
(add '(salary (Aull DeWitt) 25000))
(add '(supervisor (Aull DeWitt) (Warbucks Oliver)))

(add '(can-do-job (computer wizard) 
                     (computer programmer)))

(add '(can-do-job (computer wizard) 
                     (computer technician)))

(add '(can-do-job (computer programmer)
                     (computer programmer trainee)))

(add '(can-do-job (administration secretary)
                     (administration big wheel)))

(add '(rule (wheel ?person)
      (and (supervisor ?middle-manager 
                       ?person)
           (supervisor ?x ?middle-manager))))

(add '(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person 
                           ?middle-manager)
               (outranked-by ?middle-manager 
                             ?boss)))))

; GO
; ==
(query-driver-loop)
