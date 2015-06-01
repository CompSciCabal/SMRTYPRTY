#lang planet neil/sicp

;; -------------------------------------------------------
;; Exercise 4.49, p.426
;; -------------------------------------------------------

(define nouns '(noun student professor cat class))
(define adjectives '(adjective lazy smart cute empty))
(define verbs '(verb studies lectures eats sleeps))
(define adverbs '(adverb slowly))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

(define (parse-sentence)
  (append (parse-noun-phrase)
          (parse-verb-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (append verb-phrase
                               (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-verb-phrase)))

(define (parse-simple-verb-phrase)
  (amb (list (parse-word verbs))
       (list (parse-word verbs)
             (parse-word adverbs))))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (append noun-phrase
                               (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-simple-noun-phrase)
  (amb (list (parse-word articles)
             (parse-word nouns))
       (list (parse-word articles)
             (parse-word adjectives)
             (parse-word nouns))))

(define (parse-prepositional-phrase)
  (list (parse-word prepositions)
        (parse-noun-phrase)))

(define *unparsed* nil)

(define (parse-word words)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr words)))
  (let ((found (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (ramb-list (cdr words))))

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))


(parse '(a smart professor lectures to the student with a cute cat))

;(the lazy student studies for (the student) for (the lazy student))
;(the lazy student studies for (the student) for (the lazy professor))
;(the lazy student studies for (the student) for (the lazy cat))
;(the lazy student studies for (the student) for (the lazy class))
;(the lazy student studies for (the student) for (the smart student))
;(the lazy student studies for (the student) for (the smart professor))
;(the lazy student studies for (the student) for (the smart cat))
;(the lazy student studies for (the student) for (the smart class))
;(the lazy student studies for (the student) for (the cute student))
;(the lazy student studies for (the student) for (the cute professor))
;(the lazy student studies for (the student) for (the cute cat))
;...

;; Exercise 4.50.b, p.436

;(the empty class lectures with (the cat) with (the empty professor))
;(the empty class lectures with (the cat) with (the empty cat))
;(the empty class lectures with (the cat) with (the empty student))
;(the empty class lectures with (the cat) with (the empty class))
;(the empty class lectures with (the cat) with (the cute student))
;(the empty class lectures with (the cat) with (the cute professor))
;(the empty class lectures with (the cat) with (the cute class))
;(the empty class lectures with (the cat) with (the cute cat))
;(the empty class lectures with (the cat) with (the lazy professor))
;(the empty class lectures with (the cat) with (the lazy cat))
;(the empty class lectures with (the cat) with (the lazy student))
;(the empty class lectures with (the cat) with (the lazy class))
;(the empty class lectures with (the cat) with (the smart cat))
;(the empty class lectures with (the cat) with (the smart professor))
;(the empty class lectures with (the cat) with (the smart student))
;(the empty class lectures with (the cat) with (the smart class))
;(the empty class lectures with (the cat) with (a lazy professor))
;(the empty class lectures with (the cat) with (a lazy cat))
;(the empty class lectures with (the cat) with (a lazy class))
;(the empty class lectures with (the cat) with (a lazy student))
;(the empty class lectures with (the cat) with (a smart student))
;...
