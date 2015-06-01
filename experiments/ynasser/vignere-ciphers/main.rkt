#lang racket

;; work in progress

;; Vignere cipher: a substitution cipher. You take the message you want 
;; encrypted and assign each character a in the message a number based on some
;; scale (usually 'a' is 1, 'b' is 2, and so on). Then you select a key (the 
;; longer the key, the better) and "add" it it to the message. Examples:
;; Message: "helloworld" key:"dog"
;;  helloworld  
;;+ dogdogdogd  
;;  ----------
;;  ksroccrfrg
;;
;; note: a's value is zero
;; 
;; explanation of the first letters being added:
;; h = 7, d = 4
;; h + d mod 25 => 7 + 4 mod 25 => 11 = k

;; a = 1, z = 26
(define alphabet (string->list "abcdefghijklmnopqrstuvwxyz"))

;; letter and determine-num can be abstracted
(define (letter n)
  (define (helper lst count)
    (cond
      [(equal? count n) (car lst)]
      [else (helper (cdr lst) (+ 1 count))]))
  (helper alphabet 1))

(define (determine-num letter)
  (define (helper lst count)
    (cond
      [(eq? letter (car lst)) count]
      [else (helper (cdr lst) (+ 1 count))]))
  (helper alphabet 1))

(define (extend-key original new len)
  ;(display "new ")(display new)(newline)
  (cond
    [(<= len (length new)) new]
    [else 
     (extend-key original (append original new) len)]))

(define (add-letters l1 l2)
  (letter (remainder (+ (determine-num l1) (determine-num l2)) 25)))

;; message (list char) + key (list char) => string
;; note: messages must only contain lowercase letters, no whitespace, and no 
;; punctuation.
(define (encode message key)
  (define (helper lst new-message k)
    (cond 
      [(empty? lst) new-message]
      [else 
       (helper (rest lst) (cons 
                           (add-letters (first lst) (first k)) 
                           new-message) 
               (rest k))]))    
  (list->string 
   (reverse (helper message null (extend-key key null (length message))))))
  
(define key-1 (string->list "aaa"))
(define key-2 (string->list "abc"))
(define message-1 (string->list "dog"))

;; Decoding

;; It is not always possible to decode a message encrypted by a Vignere cipher.
;; if the key is "short enough" relative to the message, the message can be
;; cracked via an application of probability theory, frequency analysis, and 
;; brute force. If the key length is known, then the problem degenerates into 
;; cracking a simple substitution cipher. See the PDF in this directory for a
;; mathematical derivation of the cracking procedure.

;; However, if the key is at least as long as the message and never reused,
;; then it is essentially a one-time pad and cannot be cracked.*
;; 
;; * key must be random

;; I shall assume the key length is shorter than the message.

;; Step 1: Estimating the key length
(define letter-freq "etaoinshrdlucmfwypvbgkjqxz")
(define let-freq-lst (string->list letter-freq))
(define k-r 0.0385) ;; the coincidence of random selection for english (1/26)
(define k-p 0.067) ;; probability that any two randomly selected letters are
;; the same ... (without the normalizing denominator)
;; to calculate this yourself, use (sum-sqr-frequencies eng-letter-freq)
;; k-0 is defined below

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) 
          (accumulate op initial (cdr sequence)))))


(define (observed-letter-freq message letter)
  (length (filter (lambda (x) (equal? x letter)) message)))

;; test this
;; determining the index of coincidence
(define (ic message)
  (define (helper alph total)
    (cond
      [(empty? alph) 
       (/ total (* (length message) (- (length message) 1)) total)]
      [else (helper 
             (rest alph) 
             (+ total (* (observed-letter-freq (first alph)) 
                         (- (observed-letter-freq (first alph)) 1))))]))
  (helper alphabet 0))

(define (estimate-key-length message)
  (/ (- k-p k-r) (-  k-r)))

(define (decode message)
  'dog)

;; in general, values are older
(define eng-letter-freq
  ;;etnoairshldcfumygpbvwkqjxz
  (list .12 .093 .082 .081 .081 .08 .07 .054 .051 .046 .037 .034 .026 .023 .022 .019 .019 .018 .013 .011 .0086 .0021 .0018 .0017
        .00081 .00046))
        
(define (sum-sqr-frequencies freq)
  (accumulate + 0 (map sqr freq)))

;; msg: list of chars, letter: char
(define (determine-frequency msg letter)
  (/ (count (lambda (x) (equal? x letter)) msg) (length msg)))

;; determing k-0
;; this is wrong
(define (k-0 message)
  (sum-sqr-frequencies 
   
   (build-list (lambda (x) (determine-frequency message x)) (string->list message))))