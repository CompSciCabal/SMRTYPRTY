;;; 2.53

(list 'a 'b 'c)
> (a b c)

(list (list 'george))
> ((george))

(cdr '((x1 y1) (x2 y2)))
> ((x2 y2))

(cadr '((x1 y1) (x2 y2)))
> (x2 y2)

(pair? (car '(a short list)))
> #f

(memq 'red '((red shoes) (blue socks)))
> #f

(memq 'red '(red shoes blue socks))
> #t

;;; 2.54
(define (my-equal? a b)
  (or (and (symbol? a) (symbol? b) (eq? a b))
      (and (my-equal? (car a) (car b))
           (my-equal? (cdr a) (cdr b)))))

;;; 2.55

(car ''abracadabra)
;; apply read macros
(car (quote (quote abracadabra)))
;; apply special forms; the first quote supresses evaluation on the second, which becomes an element of a list.
;; basically
(car ('quote 'abracadabra))
;; finally, take the car of that two-element list
quote