#lang racket
(print-as-expression #f)
;(pretty-print-abbreviate-read-macros #f)
(define-syntax example
  (syntax-rules ()
    ((_ e) (begin (newline)
                  (pretty-print 'e)
                  (displayln "==>")
                  (pretty-print e)))))

(define (random-elt xs) (list-ref xs (random (length xs))))
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
  (and (pair? pattern) (pair? (car pattern)) (eq? (caar pattern) '?*)))
(define (segment-match env pattern input)
  (define seg-var (cadar pattern))
  (define pat (cdr pattern))
  (define binding (assoc seg-var env))
  (if binding
    ;; For efficiency as mentioned in exercise 5.13.
    (let loop ((input input) (binding binding))
      (cond ((null? binding) (pat-match/env env pat input))
            ((pair? binding) (and (pair? input)
                                  (equal? (car binding) (car input))
                                  (loop (cdr input) (cdr binding))))
            (else            #f)))
    (let loop ((input input) (seg '()))
      (define e2 (pat-match/env env pat input))
      (cond ((and e2       (var-match e2 seg-var (reverse seg))))
            ((pair? input) (loop (cdr input) (cons (car input) seg)))
            (else          #f)))))

(define (pat-match/env env pattern input)
  (and env
       (cond ((var? pattern)             (var-match env pattern input))
             ((segment-pattern? pattern) (segment-match env pattern input))
             ((and (pair? pattern) (pair? input))
              (pat-match/env (pat-match/env env (car pattern) (car input))
                             (cdr pattern) (cdr input)))
             (else (and (equal? pattern input) env)))))
(define (pat-match pattern input) (pat-match/env '() pattern input))

(example (pat-match '(I need a ?X) '(I need a vacation)))
(example (pat-match '(I need a ?X) '(I really need a vacation)))
(example (sub* (pat-match '(I need a ?X) '(I need a vacation))
               '(What would it mean to you if you got a ?X ?)))
(example (pat-match '(?X is ?X) '((2 + 2) is 4)))
(example (pat-match '(?X is ?X) '((2 + 2) is (2 + 2))))
(example (pat-match '(?P need . ?X) '(I need a long vacation)))
(example (pat-match '((?* ?p) need (?* ?x))
                    '(Mr Hulot and I need a vacation)))
(example (pat-match '((?* ?x) is a (?* ?y))
                    '(What he is is a fool)))
(example (pat-match '((?* ?x) a b (?* ?x))
                    '(1 2 a b a b 1 2 a b)))

(define eliza-rules
  '((((?* ?x) hello (?* ?y))
     (How do you do.  Please state your problem.))

    (((?* ?x) computer (?* ?y))
     (Do computers worry you?)
     (What do you think about machines?)
     (Why do you mention computers?)
     (What do you think machines have to do with your problem?))

    (((?* ?x) name (?* ?y))
     (I am not interested in names))

    (((?* ?x) sorry (?* ?y))
     (Please do not apologize)
     (Apologies are not necessary)
     (What feelings do you have when you apologize?))

    (((?* ?x) I remember (?* ?y))
     (Do you often think of ?y)
     (Does thinking of ?y bring anything else to mind?)
     (What else do you remember?)
     (Why do you recall ?y right now?)
     (What in the present situation reminds you of ?y ?)
     (What is the connection between me and ?y ?))

    (((?* ?x) do you remember (?* ?y))
     (Did you think I would forget ?y ?)
     (Why do you think I should recall ?y now?)
     (What about ?y ?)
     (You mentioned ?y))

    (((?* ?x) if (?* ?y))
     (Do you really think it is likely that ?y)
     (Do you wish that ?y)
     (What do you think about ?y)
     (Really-- if ?y))

    (((?* ?x) I dreamt (?* ?y))
     (Really-- ?y)
     (Have you ever fantasized ?y while you were awake?)
     (Have you dreamt ?y before?))

    (((?* ?x) dream about (?* ?y))
     (how do you feel about ?y in reality?))

    (((?* ?x) dream (?* ?y))
     (What does this dream suggest to you?)
     (Do you dream often?)
     (What persons appear in your dreams?)
     (Do you believe that dream has to do with your problem?))

    (((?* ?x) my mother (?* ?y))
     (Who else in your family ?y ?)
     (Tell me more about your family))

    (((?* ?x) my father (?* ?y))
     (Your father)
     (Does he influence you strongly?)
     (What else comes to mind when you think of your father?))

    (((?* ?x) I want (?* ?y))
     (What would it mean if you got ?y)
     (Why do you want ?y)
     (Suppose you got ?y soon))

    (((?* ?x) I am glad (?* ?y))
     (How have I helped you to be glad?)
     (What makes you happy just now?)
     (Can you exlpain why you are suddenly glad?))

    (((?* ?x) I am sad (?* ?y))
     (I am sorry to hear you are depressed)
     (I am sure it is not pleasant to be sad))

    (((?* ?x) are like (?* ?y))
     (What resemblance do you see between ?x and ?y ?))

    (((?* ?x) is like (?* ?y))
     (In what way is it that ?x is like ?y ?)
     (What resemblance do you see?)
     (Could there really be some connection?)
     (How?))

    (((?* ?x) alike (?* ?y))
     (In what way?)
     (what similarities are there?))

    (((?* ?x) same (?* ?y))
     (what other connections do you see?))

    (((?* ?x) I was (?* ?y))
     (Were you really?)
     (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))

    (((?* ?x) was I (?* ?y))
     (What if you were ?y ?)
     (Do you think you were ?y ?)
     (What would it mean if you were ?y ?))

    (((?* ?x) I am (?* ?y))
     (In what way are you ?y ?)
     (Do you want to be ?y ?))

    (((?* ?x) am I (?* ?y))
     (Do you believe you are ?y ?)
     (Would you want to be ?y ?)
     (You wish I would tell you you are ?y)
     (What would it mean if you were ?y ?))

    (((?* ?x) am (?* ?y))
     (Why do you say "AM?")
     (I do not understand that))

    (((?* ?x) are you (?* ?y))
     (Why are you interested in whether I am ?y or not?)
     (Would you prefer if I were not ?y ?)
     (Perhaps I am ?y in your fantasies))

    (((?* ?x) you are (?* ?y))
     (What makes you think I am ?y ?))

    (((?* ?x) because (?* ?y))
     (Is that the real reason?)
     (What other reasons might there be?)
     (Does that reason seem to explain anything else?))

    (((?* ?x) were you (?* ?y))
     (Perhaps I was ?y)
     (What do you think?)
     (What if I had been ?y ?))

    (((?* ?x) I cannot (?* ?y))
     (Maybe you could ?y now)
     (What if you could ?y ?))

    (((?* ?x) I was (?* ?y))
     (Were you really?)
     (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))

    (((?* ?x) I feel (?* ?y))
     (Do you often feel ?y ?))

    (((?* ?x) I felt (?* ?y))
     (What other feelings do you have?))

    (((?* ?x) I (?* ?y) you (?* ?z))
     (Perhaps in your fantasy we ?y each other))

    (((?* ?x) why do you not (?* ?y))
     (Should you ?y yourself?)
     (Do you believe I do not ?y)
     (Perhaps I will ?y in good time))

    (((?* ?x) yes (?* ?y))
     (You seem quite positive)
     (You are sure)
     (I understand))

    (((?* ?x) no (?* ?y))
     (Why not?)
     (You are being a bit negative)
     (Are you saying "NO" just to be negative?))

    (((?* ?x) someone (?* ?y))
     (Can you be more specific?))

    (((?* ?x) everyone (?* ?y))
     (Surely not everyone)
     (Can you think of anyone in particular?)
     (Who for example?)
     (You are thinking of a special person))

    (((?* ?x) always (?* ?y))
     (Can you think of a specific example?)
     (When?)
     (What incident are you thinking of?)
     (Really-- always))

    (((?* ?x) what (?* ?y))
     (Why do you ask?)
     (Does that question interest you?)
     (What is it you really wnat to know?)
     (What do you think?)
     (What comes to your mind when you ask that?))

    (((?* ?x) perhaps (?* ?y))
     (You do not seem quite certain))

    (((?* ?x) are (?* ?y))
     (Did you think they might not be ?y ?)
     (Possibly they are ?y))

    (((?* ?x))
     (Very interesting)
     (I am not sure I understand you fully)
     (What does that suggest to you?)
     (Please continue)
     (Go on)
     (Do you feel strongly about discussing such things?))))

(define (eliza/script script)
  (for-each (lambda (input)
              (display "eliza> ") (pretty-print input)
              (pretty-print (flatten (use-eliza-rules input))))
            script))

(define (eliza)
  (display "eliza> ")
  (define line (read-line))
  (unless (eof-object? line)
    (define input (with-input-from-string (string-append "(" line ")") read))
    (for-each (lambda (word) (printf "~a " word))
              (flatten (use-eliza-rules input)))
    (newline)
    (eliza)))

(define (use-eliza-rules input)
  (ormap (lambda (rule)
           (define pattern (car rule))
           (define responses (cdr rule))
           (define env (pat-match pattern input))
           (and env (sub* (switch-viewpoint env)
                          (random-elt responses))))
         eliza-rules))

(define (switch-viewpoint words)
  (sub* '((I . you) (you . I) (me . you) (am . are) (my . your) (your . my)) words))

(eliza/script
  '((hello there)
    (I want to test this program)
    (I could see if it works)
    (no not really)
    (no)
    (forget it-- I was wondering how general the program is)
    (I felt like it)
    (I feel this is enough)))

(eliza)
