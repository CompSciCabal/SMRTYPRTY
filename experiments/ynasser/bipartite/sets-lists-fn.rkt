#lang racket
(provide (all-defined-out))

;; This module contains set functions for lists

;; member?
(define (member? key lst)
  (cond
    [(not (member key lst)) #f]
    [else #t]))

;; empty? ehh sicp lisp is kind of irritating
(define (empty? lst)
  (cond
    [(eq? 0 (length lst)) #t]
    [else #f]))

(define (union a b)
  (union-helper a b null))

(define (union-helper a b lst)
  (cond
    [(and (empty? a) (empty? b)) lst]
    [(and (empty? a) (not (member? (first b) lst)))
     (union-helper a (rest b) (cons (first b) lst))]
    [(and (empty? b) (not (member? (first a) lst))) 
     (union-helper (rest a) b (cons (first a) lst))]
    [(empty? a) (union-helper a (rest b) lst)]
    [(empty? b) (union-helper (rest a) b lst)]
    [else (union-helper (rest a) b (cons (first a) lst))]))

;; same as union, but preserves order
(define (combine a b)
  (union null (union a b)))

(define (intersection a b) (intersection-helper a b null))

(define (intersection-helper a b lst)
  (cond
    [(empty? a) lst]
    [(member? (first a) b) (intersection-helper (rest a) b (cons (first a) lst))]
    [else (intersection-helper (rest a) b lst)]))
