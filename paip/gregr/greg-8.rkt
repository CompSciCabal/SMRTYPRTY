#lang racket
(require "tools.rkt")
(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)
(define-syntax example
  (syntax-rules ()
    ((_ e) (begin (newline)
                  (pretty-print 'e)
                  (displayln "==>")
                  (time (pretty-print e))))))
(define-syntax examples
  (syntax-rules ()
    ((_ e ...) (begin (example e) ...))))


(define (rule pattern response) (list pattern response))
(define (rule-pattern r)        (car r))
(define (rule-response r)       (cadr r))

(define (expr lhs op rhs) (list op lhs rhs))
(define (expr? x)         (pair? x))
(define (expr-op x)       (car x))
(define (expr-lhs x)      (cadr x))
(define (expr-rhs x)      (caddr x))
(define (expr-args x)     (cdr x))
(define (binary-expr? x)  (and (expr? x) (= (length (expr-args x)) 2)))
(define (in-expr? x e)
  (or (eqv? x e)
      (and (expr? e) (or (in-expr? x (expr-lhs e))
                         (in-expr? x (expr-rhs e))))))
(define (prefix->infix e)
  (if (expr? e)
    (map prefix->infix
         (if (binary-expr? e) (list (expr-lhs e) (expr-op e) (expr-rhs e)) e))
    e))

;; Consider:
;(((a * (x ^ 2)) + (b * x)) + c)
;(a * x ^ 2 + b * x + c)
;(a x ^ 2 + b x + c)
;a x^2 + b*x+c

;; Hack for handling only fully-parenthesized expressions.
;(define (infix->prefix e) (prefix->infix e))

(define infix->prefix-rules
  '((((?+ ?x) = (?+ ?y)) (= ?x ?y))
    ((- (?+ ?x))         (- ?x))
    ((+ (?+ ?x))         ?x)  ;; This was originally (+ ?x) for some reason.
    (((?+ ?x) + (?+ ?y)) (+ ?x ?y))
    (((?+ ?x) - (?+ ?y)) (- ?x ?y))

    ((d (?+ ?y) / d ?x)  (d ?y ?x))
    ((Int (?+ ?y) d ?x)  (Int ?y ?x))

    (((?+ ?x) * (?+ ?y)) (* ?x ?y))
    (((?+ ?x) / (?+ ?y)) (/ ?x ?y))
    (((?+ ?x) ^ (?+ ?y)) (^ ?x ?y))))

(define (infix->prefix e)
  (cond ((not (expr? e))  e)
        ((= (length e) 1) (infix->prefix (car e)))
        ((rule-based-translator
           (rule-system
             pat-match rule-pattern rule-response
             (lambda (bindings response)
               (sub* (map (lambda (pair) (cons (car pair)
                                               (infix->prefix (cdr pair))))
                          bindings)
                     response)))
           e infix->prefix-rules))
        ((symbol? (car e)) (list (car e) (infix->prefix (cdr e))))
        (else              (error "illegal expression:" e))))

(define var-abbrevs
  (pat-match-abbrevs
    '()
    (map (lambda (v)
           (cons v (string->symbol (string-append "?" (symbol->string v)))))
         '(x y z m n o p q r s t u v w))))

(define full-abbrevs
  (pat-match-abbrevs
    var-abbrevs
    '((n . (?is n number?))
      (m . (?is m number?))
      (s . (?is s (lambda (x) (not (number? x))))))))

(define simplification-rules
  (append
    (map (lambda (e) (expand-pat-match-abbrev var-abbrevs (infix->prefix e)))
         '((x + 0       = x)
           (0 + x       = x)
           (x + x       = 2 * x)
           (x - 0       = x)
           (0 - x       = - x)
           (x - x       = 0)
           (- - x       = x)
           (x * 1       = x)
           (1 * x       = x)
           (x * 0       = 0)
           (0 * x       = 0)
           (x * x       = x ^ 2)
           (x / 0       = undefined)
           (0 / x       = 0)
           (x / 1       = x)
           (x / x       = 1)
           (0 ^ 0       = undefined)
           (x ^ 0       = 1)
           (0 ^ x       = 0)
           (1 ^ x       = 1)
           (x ^ 1       = x)
           (x ^ -1      = 1 / x)
           (x * (y / x) = y)
           ((y / x) * x = y)
           ((y * x) / x = y)
           ((x * y) / x = y)
           (x + - x     = 0)
           ((- x) + x   = 0)
           (x + y - x   = y)

           (log 1                     = 0)
           (log 0                     = undefined)
           (log e                     = 1)
           (sin 0                     = 0)
           (sin pi                    = 0)
           (cos 0                     = 1)
           (cos pi                    = -1)
           (sin (pi / 2)              = 1)
           (cos (pi / 2)              = 0)
           (log (e ^ x)               = x)
           (e ^ (log x)               = x)
           ((x ^ y) * (x ^ z)         = x ^ (y + z))
           ((x ^ y) / (x ^ z)         = x ^ (y - z))
           (log x + log y             = log (x * y))
           (log x - log y             = log (x / y))
           ((sin x) ^ 2 + (cos x) ^ 2 = 1)
           ))
    (map (lambda (e)
           (define x (infix->prefix e))
           (expr (expand-pat-match-abbrev full-abbrevs (expr-lhs x))
                 (expr-op x)
                 (expand-pat-match-abbrev var-abbrevs (expr-rhs x))))
         '((s * n       = n * s)
           (n * (m * x) = (n * m) * x)
           (x * (n * y) = n * (x * y))
           ((n * x) * y = n * (x * y))
           (n + s       = s + n)
           ((x + m) + n = x + n + m)
           (x + (y + n) = (x + y) + n)
           ((x + n) + y = (x + y) + n)

           (d x / d x       = 1)
           (d (u + v) / d x = (d u / d x) + (d v / d x))
           (d (u - v) / d x = (d u / d x) - (d v / d x))
           (d (- u) / d x   = - (d u / d x))
           (d (u * v) / d x = u * (d v / d x) + v * (d u / d x))
           (d (u / v) / d x = (v * (d u / d x) - u * (d v / d x))
                              / v ^ 2)
           (d (u ^ n) / d x = n * u ^ (n - 1) * (d u / d x))
           (d (u ^ v) / d x = v * u ^ (v - 1) * (d u / d x)
                              + u ^ v * (log u) * (d v / d x))
           (d (log u) / d x = (d u / d x) / u)
           (d (sin u) / d x = (cos u) * (d u / d x))
           (d (cos u) / d x = - (sin u) * (d u / d x))
           (d (e ^ u) / d x = (e ^ u) * (d u / d x))
           (d u / d x       = 0)
           ))))

(define (simplifier)
  (display "simplifier> ")
  (define line (read-line))
  (unless (eof-object? line)
    (define d (with-input-from-string (string-append "(" line ")") read))
    (printf "~s\n" (simp d))
    (simplifier)))

(define (simp inf) (prefix->infix (simplify (infix->prefix inf))))

(define (simplify e) (if (not (expr? e)) e
                       (simplify-expr (map simplify e))))

(define (simplify-expr e)
  (cond ((eval-symbolic e))
        ((rule-based-translator
           (rule-system
             pat-match expr-lhs expr-rhs
             (lambda (bindings response) (simplify (sub* bindings response))))
           e simplification-rules))
        ((evaluable? e) (eval-math e))
        (else           e)))

(define (evaluable? e)
  (and (andmap number? (expr-args e))
       (or (member (expr-op e) '(+ - * /))
           (and (eq? (expr-op e) '^)
                (integer? (cadr (expr-args e)))))))

(define (eval-math e)
  (define op (and (expr? e) (expr-op e)))
  (define (? o) (eqv? op o))
  (define (@ proc) (apply proc (map eval-math (expr-args e))))
  (cond ((not op)   e)
        ((? '+)   (@ +))
        ((? '-)   (@ -))
        ((? '*)   (@ *))
        ((? '/)   (@ /))
        ((? '^)   (@ expt))
        ((? 'log) (@ log))
        ((? 'sin) (@ sin))
        ((? 'cos) (@ cos))
        (error "unknown operator:" op)))

(define (eval-symbolic e)
  (define op (and (expr? e) (expr-op e)))
  (define (? o) (eqv? op o))
  (define result
    (and op (cond ((? 'Int) (unfactorize
                              (factorize
                                (integrate (expr-lhs e) (expr-rhs e)))))
                  (else     #f))))
  (and result (simplify result)))

(define (factorize e)
  (let ((factors '()) (constant 1))
    (let fac ((x e) (n 1))
      (cond ((number? x) (set! constant (* constant (expt x n))))
            ((starts-with? x '*) (fac (expr-lhs x) n) (fac (expr-rhs x) n))
            ((starts-with? x '/) (fac (expr-lhs x) n) (fac (expr-rhs x) (- n)))
            ((and (starts-with? x '-) (length=1 (expr-args x)))
             (set! constant (- constant))
             (fac (expr-lhs x) n))
            ((and (starts-with? x '^) (number? (expr-rhs x)))
             (fac (expr-lhs x) (* n (expr-rhs x))))
            (else (let loop ((fs factors) (passed '()))
                    (cond ((null? fs) (set! factors (cons `(^ ,x ,n) factors)))
                          ((equal? x (expr-lhs (car fs)))
                           (define factor (car fs))
                           (define new-factor `(^ ,x ,(+ (expr-rhs factor) n)))
                           (set! factors
                             (foldl cons (cons new-factor (cdr fs)) passed)))
                          (else (loop (cdr fs) (cons (car fs) passed))))))))
    (case constant
      ((0)  '((^ 0 1)))
      ((1)  factors)
      (else `((^ ,constant 1) . ,factors)))))

(define (unfactorize factors)
  (cond ((null? factors)    1)
        ((length=1 factors) (car factors))
        (else               `(* ,(car factors) ,(unfactorize (cdr factors))))))

(define (divide-factors numer denom)
  (foldl (lambda (d numer)
           (let loop ((factors numer) (passed '()))
             (cond ((null? factors)
                    (cons `(^ ,(expr-lhs d) ,(- (expr-rhs d))) numer))
                   ((equal? (expr-lhs d) (expr-lhs (car factors)))
                    (define factor (car factors))
                    (define exponent (- (expr-rhs factor) (expr-rhs d)))
                    (define new-factors
                      (if (= exponent 0) (cdr factors)
                        (cons `(^ ,(expr-lhs factor) ,exponent)
                              (cdr factors))))
                    (foldl cons new-factors passed))
                   (else (loop (cdr factors) (cons (car factors) passed))))))
         numer denom))

(define (free-of? tree v)
  (and (not (equal? v tree)) (or (not (pair? tree))
                                 (and (free-of? (car tree) v)
                                      (free-of? (cdr tree) v)))))

(define (length=1 x) (and (pair? x) (null? (cdr x))))

(define (starts-with? x v) (and (pair? x) (eqv? (car x) v)))

(define (integrate e x)
  (cond ((free-of? e x) `(* ,e ,x)) ;; Int c dx  = c*x
        ((starts-with? e '+)        ;; Int f + g = Int f + Int g
         `(+ ,(integrate (expr-lhs e) x) ,(integrate (expr-rhs e) x)))
        ((starts-with? e '-)
         (case (length (expr-args e))
           ((1) `(- ,(integrate (expr-lhs e) x)))  ;; Int - f   = - Int f
           ((2) `(- ,(integrate (expr-lhs e) x)    ;; Int f - g = Int f - Int g
                    ,(integrate (expr-rhs e) x)))
           (else (error "more than 2 arguments given to -:" e))))
        (else (define (free? factor) (free-of? factor x))
              (define factors (factorize e))
              (define const-factors (filter free? factors))
              (define x-factors (filter-not free? factors))
              (simplify
                `(* ,(unfactorize const-factors)
                    ;; And try to integrate:
                    ,(cond ((null? x-factors) x)
                           ((ormap (lambda (f) (deriv-divides f x-factors x))
                                   x-factors))
                           ;; TODO: try other methods here.
                           (else `(Int? ,(unfactorize x-factors) ,x))))))))

(define (deriv-divides factor factors x)
  (unless (starts-with? factor '^) (error "invalid factor:" factor))
  (let* ((u (expr-lhs factor))  ;; factor = u^n
         (n (expr-rhs factor))
         (k (divide-factors factors (factorize `(* ,factor ,(deriv u x))))))
    (cond ((free-of? k x)
           ;; Int k*u^n*du/dx dx = k*Int u^n du
           ;;                    = k*u^(n+1)/(n+1) for n =/= -1
           ;;                    = k*log(u) for n = -1
           (if (= n -1)
             `(* ,(unfactorize k) (log ,u))
             `(/ (* ,(unfactorize k) (^ ,u ,(+ n 1))) ,(+ n 1))))
          ((and (= n 1) (in-integral-table? u))
           ;; Int y'*f(y) dx = Int f(y) dy
           (define k2 (divide-factors
                        factors (factorize `(* ,u ,(deriv (expr-lhs u) x)))))
           (and (free-of? k2 x)
                `(* ,(integrate-from-table (expr-op u) (expr-lhs u))
                    ,(unfactorize k2))))
          (else #f))))

(define (deriv y x) (simplify `(d ,y ,x)))

(define integration-table
  (map (lambda (inf)
         (define rule (infix->prefix inf))
         (cons (expr-op (expr-lhs (expr-lhs rule))) rule))
       '((Int log  (x) d x = x * log (x) - x)
         (Int exp  (x) d x = exp (x))
         (Int sin  (x) d x = - cos (x))
         (Int cos  (x) d x = sin (x))
         (Int tan  (x) d x = - log (cos (x)))
         (Int sinh (x) d x = cosh (x))
         (Int cosh (x) d x = sinh (x))
         (Int tanh (x) d x = log (cosh (x))))))

(define (in-integral-table? e) (and (expr? e) (assoc (expr-op e) integration-table)))

(define (integrate-from-table op arg)
  (define rule (cdr (assoc op integration-table)))
  (sub* (list (cons (expr-lhs (expr-lhs (expr-lhs rule))) arg))
        (expr-rhs rule)))

(examples
  (simp '(2 + 2))
  (simp '(5 * 20 + 30 + 7))
  (simp '(5 * x - (4 + 1) * x))
  (simp '(y / z * (5 * x - (4 + 1) * x)))
  (simp '((4 - 3) * x + (y / y - 1) * z))
  (simp '(1 * f (x) + 0))
  (simp '(3 * 2 * x))
  (simp '(2 * x * x * 3))
  (simp '(2 * x * 3 * y * 4 * z * 5 * 6))
  (simp '(3 + x + 4 + x))
  (simp '(2 * x * 3 * x * 4 * (1 / x) * 5 * 6))

  (simp '(3 + x + 4 - x))
  (simp '(x + y + y + x))
  (simp '(3 * x + 4 * x))

  (simp '(d (x + x) / d x))
  (simp '(d (a * x ^ 2 + b * x + c) / d x))
  (simp '(d ((a * x ^ 2 + b * x + c) / x) / d x))

  (simp '(log ((d (x + x) / d x) / 2)))
  (simp '(log (x + x) - log x))
  (simp '(x ^ cos pi))
  (simp '(d (3 * x + (cos x) / x) / d x))
  (simp '(d ((cos x) / x) / d x))
  (simp '(d (3 * x ^ 2 + 2 * x + 1) / d x))
  (simp '(sin (x + x) ^ 2 + cos (d x ^ 2 / d x) ^ 2))
  (simp '(sin (x + x) * sin (d x ^ 2 / d x) +
          cos (2 * x) * cos (x * d 2 * y / d y)))

  (simp '(Int x * sin (x ^ 2) d x))
  (simp '(Int ((3 * x ^ 3) - 1 / (3 * x ^ 3)) d x))
  (simp '(Int (3 * x + 2) ^ -2/3 d x))
  (simp '(Int sin (x) ^ 2 * cos (x) d x))
  (simp '(Int sin (x) / (1 + cos (x)) d x))
  (simp '(Int (2 * x + 1) / (x ^ 2 + x - 1) d x))
  (simp '(Int 8 * x ^ 2 / (x ^ 3 + 2) ^ 3 d x)))

(simplifier)
