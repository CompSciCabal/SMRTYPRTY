;;; 2.73
;; a)

;; We're pulling some of the expressions out and using their operators as "type tags" to help us do table-based dispatch on them.
;; In principle, we could assimilate the `number?` and `variable?` checks, but we'd have to start tagging the appropriate expressions to do it
;; with our current approach (which would be odd amounts of work for native types)

;; b)
(define (install-symbolic-deriv-package)
  (define (deriv ...) ...)
  (define (addend ...) ...)
  (define (augend ...) ...)
  (define (multiplier ...) ...)
  (define (multiplicand ...) ...)

  (define (+ exp var)
    (+ (deriv (addend exp) var)
       (deriv (augend exp) var)))

  (define (* exp var)
    (+ (* (multiplier exp)
	  (deriv (multiplicand exp) var))
       (* (deriv (multiplier exp) var)
	  (multiplicand exp))))

  (put 'deriv '(+) +)
  (put 'deriv '(*) *))


;; c)

;; d)
;; The corresponding put statements would have to be shuffled around. That seems like it would do it though.

;; 2.75
(define (make-from-mag-ang ))
