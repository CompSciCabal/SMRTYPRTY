#lang racket
;; From Previous exercises
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; prerequisites
(define dispatch-lookup '())
(define (new-dispatch!)
	(set! dispatch-lookup (make-hash)))

(define (put type sig proc)
	(hash-set! dispatch-lookup (cons type sig) proc))

(define (get type sig)
	(hash-ref dispatch-lookup (cons type sig)))

(define (defined? type sig)
  (hash-has-key? dispatch-lookup (cons type sig)))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(displayln "exercise 2.73")

;; a. The derivation functions were moved into the "method dispatch" hash
;;    which allows us to define derivations for as many different kinds of
;;    mathematical expressions we want
;;    Numbers or variables cannot have operators applied to them; as a result
;;    we cannot assimilate number? and same-variable?

(displayln "exercise 2.73 b")

(define (install-derivative-package)
  
  ;; Addition Module
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (sum? x)
    (and (pair? x) (eq? (car x) '+)))
  (define (addend x) (car x))
  (define (augend x) (cadr x))
  
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  ;; /Addition Module
  
  ;; Multiplication Module
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))
  (define (multiplier x) (car x))
  (define (multiplicand x) (cadr x))
  
  (define (deriv-prod exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))
  
  ;; /Multiplication Module
  
  ;; Exponentiation Module
  (define (make-exponent base power)
    (cond [(eq? 0 power) 1]
          [(eq? 1 power) base]
          [else (list '** base power)]))
  
  (define (exponent? x)
    (and (pair? x) (eq? (car x) '**)))
  
  (define (base e) (car e))
  (define (exponent e) (cadr e))
  
  (define (deriv-exp exp var)
    (make-exponent
     (make-product (exponent exp) (base exp))
     (make-sum (exponent exp) -1)))
  ;; /Exponentiation Module
  
  ;; Installing modules
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-prod)
  (put 'deriv '** deriv-exp)
  #t)

(define (deriv exp var)
  (cond [(number? exp) 0]
        [(variable? exp)
         (if (same-variable? exp var) 1 0)]
        [else ((get 'deriv (operator exp)) (operands exp)
                                            var)]))

(new-dispatch!)
(install-derivative-package)
(deriv '(+ x (* 3 (+ x (+ y 2)))) 'x)

(displayln "exercise 2.73 c.")
(displayln "Added and installed derivatives of exponents")
(deriv '(** x 3) 'x)

(displayln "exercise 2.73 d.")
;; The only changes that we'd have to make are to
;; how we register our expressions with the dispatcher.
;; Simply put, we'd need to change the `put` evaluations
;; from (put 'dispatch 'expr proc) to (put 'expr 'dispatch proc)
;; The overall changes would be pretty minimal


(displayln "exercise 2.74")
(define (make-tagged tag data) (cons tag data))
(define (tag record) (car record))
(define (data record) (cdr record))

;; 2.74 a.
;; The division should provide the necessary attribute
;; accessors that are required by the overarching organization.
;; All procedures except for get-record will require tagged data
;; in order to dispatch the methods off to the appropriate packages.
;; Thankfully, we can manage that tagging within out get-record procedure.
(define (get-record division name file)
  (make-tagged division ((get 'record division) name file)))

(define (get-name record)
  ((get 'name (tag record)) (data record)))

;; 2.74 b.
;; Because of the way data is being tagged on exit from get-record
;; the internal representation of the record really doesn't matter.
;; The data should probably be stored in some kind of key-value association
;; to help reduce confusion over what various fields mean, but otherwise that
;; can be left as an implementation detail to the division.
(define (get-salary record)
  ((get 'salary (tag record)) (data record)))

;; 2.74 c. 
;; The big thing for this is we need a way to know which department
;; our data belongs to. Using pairs we can store that department information
;; such that we don't need to read to guess what departments data we are
;; working with
(define (find-employee-record name dept-records)
  (if (empty? dept-records)
      'record-not-found
      (let* [(records (cdar dept-records))
             (department (caar dept-records))
             (record (get-record department name))]
        (if (eq? 'record-not-found (data record))
            'record-not-found
            (find-employee-record name (cdr dept-records))))))

;; 2.74 d.
;; When a package gets installed it's return value should be the department
;; of the package. With that returned we could have our system validate it
;; and ensure that all the required methods have been defined. Though, this
;; does not mean that we are validating the actual methods arguments. That
;; would require some additional work
(define (validate package)
  (define (error-message errors)
    (if (empty? errors)
        ""
        (string-append (symbol->string (car errors)) ", " (error-message (cdr errors)))))
  
  (define (validate-iter reqd-procs errs)
    (if (empty? reqd-procs)
        errs
        (let [(proc (car reqd-procs))
              (rest (cdr reqd-procs))]
          (cond [(defined? proc package)
                 (validate-iter rest errs)]
                [else (validate-iter rest (cons proc errs))]))))
  (let [(errors (validate-iter '(record name salary) '()))]
    (when (not (empty? errors))
      (error (string-append (symbol->string package) " is missing required procedures: " (error-message errors))))))

(define (install package-installer)
  (validate (package-installer)))
        
  

(define (sales-pkg)
  (define sales
  '(
    (name "John Smith"
     salary "50K"
     address "123 Fake St, Fake Town, Fakeprov")
    (name "Jane McTavish"
     salary "52K"
     address "321 Fake Ave, Fakeville, Fakeprov")))
  
  (define (get-value key record)
    (if (eq? (car record) key)
        (cadr record)
        (get-value key (cddr record))))
  
  (define (get-record name records)
    (cond
      [(empty? records) 'record-not-found]
      [(eq? (get-value 'name (car records)) name) (car records)]
      [else (get-record (cdr records))]))
  
  ;; Install package functions
  (put 'record 'sales get-record)
  (put 'name 'sales (lambda (x) (get-value 'name x)))
  (put 'salary 'sales (lambda (x) (get-value 'salary x)))
  (put 'salary 'sales (lambda (x) (get-value 'address x)))
  'sales)

(define (marketing-pkg)
  (define marketing
    (list
     '("Jake Jones" "111 Somewhere, Someplace, Somestate" "45K")
     '("Beverly Michaels" "777 Lucky St, Luckycat, Luckystate" "65K")))
  
  (define (get-name record) (car record))
  (define (get-salary record) (caddr record))
  (define (get-address record) (cadr record))
  
  (define (get-record name file)
    (define (get-record-iter records)
      (cond [(empty? records) 'record-not-found]
            [(eq? (get-name (car records)) name) (car records)]
            [else (get-record-iter (cdr records))]))
    (get-record-iter marketing))
  
  (put 'record 'marketing get-record)
  (put 'name 'marketing get-name)
  (put 'salary 'marketing get-salary)
  (put 'address 'marketing get-address)
  'marketing)

(new-dispatch!)
(install sales-pkg)
(install marketing-pkg)