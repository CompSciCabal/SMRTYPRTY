;;;; Transcript from playing with Greg's Chapter 2 PAIP code in Racket
;;;; during the 17 May 02019 meeting.

;;; This transcript assumes you are starting Racket in Greg's directory.
;;;
;;; It also assumes the following code is added to the 'greg-2.rkt' file,
;;; immediately below the '#lang' racket line:
;;;
;;; (provide
;;;   (all-defined-out))

Welcome to Racket v7.2.
> (require "greg-2.rkt")
> (Adj*)
'(big)
> (PP*)
'(to a little blue adiabatic table to a little ball in the ball by the woman)
> (tedious-gen-eval)
'((symbol-append (app 'h '5) (var ()))
  ===>
  (invalid-operation:
   (symbol-append (invalid-application: (h 5)) unbound-variable)))
> (tedious-gen-eval)
'((if (var ()) (/ (var ()) '()) '())
  ===>
  (invalid-operation: (/ unbound-variable ())))
> (tedious-gen-eval)
'((app (var ()) (var (((())))))
  ===>
  (invalid-application: (unbound-variable unbound-variable)))
> (tedious-gen-eval)
'((foldr '() (var ()) '#f)
  ===>
  (invalid-operation: (foldr () unbound-variable #f)))
> (tedious-gen-eval)
'((lambda (var (()))) ===> #(closure (var (())) ()))
> (tedious-gen-eval)
'((cons (lambda (+ (var ((((((()))))))) '#f)) (var (())))
  ===>
  (#(closure (+ (var ((((((()))))))) '#f) ()) . unbound-variable))
> (tedious-gen-eval)
'((if (foldN
       (lambda (/ '4 'app))
       (foldN
        (var (()))
        (foldl 'foldr (foldl 'foldl (var ()) 'q) '#f)
        (var (())))
       '6)
    '6
    (map '#f (var (((()))))))
  ===>
  6)
> (tedious-gen-eval)
'((app '8 (var ())) ===> (invalid-application: (8 unbound-variable)))
> (tedious-gen-eval)
'((procedure?
   (app
    (foldr
     (> (foldN 'z '> (var (((((()))))))) (var (((())))))
     '#f
     (foldl 'lambda '((#f . #f) . 9) (var (()))))
    '(#t)))
  ===>
  #f)
> (tedious-gen-eval)
'((app (var ()) (var (((())))))
  ===>
  (invalid-application: (unbound-variable unbound-variable)))
> (tedious-gen-eval)
'((null? '#t) ===> #f)
> (tedious-gen-eval)
'((if '() (var ()) (number? (var ((((()))))))) ===> unbound-variable)
> 

Process scheme finished
Welcome to Racket v7.2.
> (require "greg-2.rkt")
> (tedious-gen-eval)
'('foldr ===> foldr)
> (tedious-gen-eval)
'((pair? 'foldN) ===> #f)
> (tedious-gen-eval)
'((foldl
   (var ())
   (app (foldN (>= (var (())) (var ((())))) (var (())) '#t) '())
   'equal?)
  ===>
  (invalid-operation:
   (foldl
    unbound-variable
    (invalid-application:
     ((invalid-operation:
       (foldN
        (invalid-operation: (>= unbound-variable unbound-variable))
        unbound-variable
        #t))
      ()))
    equal?)))
> (tedious-gen-eval)
'((if (foldr (var ()) (foldl (var ((((((()))))))) '() (var (()))) (var ((()))))
    (if (if (var (((((()))))))
          '#f
          (lambda (lambda
                   (if (=
                        (var ())
                        (symbol?
                         (<=
                          (car (var ()))
                          (foldl
                           (lambda (foldr
                                    (lambda (var ()))
                                    (var ())
                                    (symbol-append* (pair? 'foldl))))
                           (var ())
                           (var ())))))
                     (> 'y 'foldN)
                     (lambda (var ()))))))
      (lambda (app (var (())) (var ())))
      (var (())))
    (lambda (if (app (var ()) (var ())) (lambda (var ((())))) (var (())))))
  ===>
  unbound-variable)
> (tedious-gen-eval)
'((if (foldN
       (if '#t
         (lambda (app (var ((()))) '((j . #t) . quote)))
         (number? (var (()))))
       (cdr 'o)
       (lambda (app (var (())) (var (())))))
    '()
    (lambda (var ())))
  ===>
  ())
> (tedious-gen-eval)
'((null? '(if . cdr)) ===> #f)
> (tedious-gen-eval)
'((if (var ()) 'd '(#f . #t)) ===> d)
> (tedious-gen-eval)
'((symbol-append (var ()) (app 'd '#f))
  ===>
  (invalid-operation:
   (symbol-append unbound-variable (invalid-application: (d #f)))))
> (tedious-gen-eval)
'((null?
   (foldN (if (app '(+) (cdr 'number?)) '#t (var ())) (var ()) (var (((()))))))
  ===>
  #f)
> (tedious-gen-eval)
'((>=
   (var (((((()))))))
   (foldr
    (app 'append* 'cons)
    (var ())
    (app (var ((()))) (foldl (var ()) (var ()) 'w))))
  ===>
  (invalid-operation:
   (>=
    unbound-variable
    (invalid-operation:
     (foldr
      (invalid-application: (append* cons))
      unbound-variable
      (invalid-application:
       (unbound-variable
        (invalid-operation: (foldl unbound-variable unbound-variable w)))))))))
> (tedious-gen-eval)
'((append (foldl (symbol? (var ())) '#t '#t) '#f)
  ===>
  (invalid-operation: (foldl #t #t #t) . #f))
> (tedious-gen-eval)
'((foldl (var (((())))) (var (())) 'quote)
  ===>
  (invalid-operation: (foldl unbound-variable unbound-variable quote)))
> (tedious-gen-eval)
'((> 'append (var ())) ===> (invalid-operation: (> append unbound-variable)))
> (tedious-gen-eval)
'((foldr
   (lambda (if '() 'quote (not (append* (app (var ()) 'symbol-append*)))))
   (foldr (var ()) '#t (var (((((())))))))
   (app (var ()) (var ((((())))))))
  ===>
  (invalid-operation:
   (foldr
    #(closure
      (if '() 'quote (not (append* (app (var ()) 'symbol-append*))))
      ())
    (invalid-operation: (foldr unbound-variable #t unbound-variable))
    (invalid-application: (unbound-variable unbound-variable)))))
> (tedious-gen-eval)
'((if (var (())) 'foldr 'cdr) ===> foldr)
> (tedious-gen-eval)
'((number? '1) ===> #t)
> (tedious-gen-eval)
'((* (var ((()))) (var ((()))))
  ===>
  (invalid-operation: (* unbound-variable unbound-variable)))
> (tedious-gen-eval)
'((app
   (if (procedure? (var ()))
     (app '#f (var ((((()))))))
     (if (var ())
       (pair? '(foldr . app))
       (foldr (lambda (foldN '#f (var ((()))) 'map)) (var (((())))) (var ()))))
   '#f)
  ===>
  (invalid-application: (#t #f)))
> (let ((x 5) (y 6)) (* (var ()) (var (()))))
; stdin:20:27: #%app: missing procedure expression;
;  probably originally (), which is an illegal empty application
;   in: (#%app)
; [,bt for context]
> (tedious-apply proc arg)
; proc: undefined;
;  cannot reference an identifier before its definition
;   in module: top-level
; [,bt for context]
> (tedious-eval '() '(quote foo))
'foo
> (tedious-eval '() '(lambda (x) x))
; cdr: contract violation
;   expected: pair?
;   given: #f
; [,bt for context]
> (tedious-eval '() '(lambda (var ())))
'#(closure (var ()) ())
> (tedious-eval '() '(app (lambda (var ())) (quote foo)))
'foo
> (tedious-eval '()
                '(app (lambda (var ())) (quote foo)))
'foo
> ((lambda (x) x) 'foo)
'foo
> (tedious-eval '()
                '(var ()))
'unbound-variable
> (tedious-eval '((() . foo))
                '(var ()))
'(() . foo)
> (tedious-eval '(foo)
                '(var ()))
'foo
> (tedious-eval '(foo bar baz)
                '(var ()))
'foo
> (tedious-eval '(foo bar baz)
                '(var (())))
'bar
> (tedious-eval '(foo bar baz)
                '(var ((()))))
'baz
> (tedious-eval '(bar baz)
                '(app (lambda (var (()))) (quote foo)))
'bar
> (tedious-eval '(bar baz quux quuux)
                '(app (lambda (var (((()))))) (quote foo)))
'quux
> (tedious-eval '(bar baz quux quuux)
                '(app (lambda (var (((()))))) (quote foo)))
'quux
> (tedious-eval '()
                '(lambda (lambda (lambda (var ((())))))))
'#(closure (lambda (lambda (var ((()))))) ())
> (tedious-eval '()
                '(app (lambda (lambda (lambda (var ((()))))))
                      (quote foo)))
'#(closure (lambda (var ((())))) (foo))
> (tedious-eval '()
                '(app (app (lambda (lambda (lambda (var ((()))))))
                           (quote foo))
                      (quote bar)))
'#(closure (var ((()))) (bar foo))
> (tedious-eval '()
                '(app (app (app (lambda (lambda (lambda (var ((()))))))
                                (quote foo))
                           (quote bar))
                      (quote baz)))
'foo
> (tedious-eval '()
                '(app
                  (app
                   (app
                    (lambda
                        (lambda
                            (lambda
                                (var ((()))))))
                    (quote foo))
                   (quote bar))
                  (quote baz)))
'foo
> ((((lambda (x)
       (lambda (y)
         (lambda (z)
           x)))
     'foo)
    'bar)
   'baz)
'foo
> ((((lambda (x)
       (lambda (y)
         (lambda (z)
           z)))
     'foo)
    'bar)
   'baz)
'baz
> (tedious-eval '()
                '(app
                  (app
                   (app
                    (lambda
                        (lambda
                            (lambda
                                (var ()))))
                    (quote foo))
                   (quote bar))
                  (quote baz)))
'baz
> ((((lambda (x)
       (lambda (y)
         (lambda (z)
           z)))
     'foo)
    'bar)
   'baz)
'baz
> (define f (lambda (x)
              (lambda (y)
                (lambda (z)
                  z))))
> f
#<procedure:f>
> (((f
     'foo)
    'bar)
   'baz)
'baz
> (define f (lambda (x)
              (lambda (y)
                (lambda (z)
                  x))))
> (((f
     'foo)
    'bar)
   'baz)
'foo
> (((f 'foo) 'bar) 'baz)
'foo
> (closure (x)
           (lambda (y)
             (lambda (z)
               x))
           '())
; closure: undefined;
;  cannot reference an identifier before its definition
;   in module: top-level
; [,bt for context]
> (closure (y)
           (lambda (z)
             x)           
           '((x . foo)))
