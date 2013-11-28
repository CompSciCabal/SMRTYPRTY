#lang racket

(require racket/stxparam)

(define part%
  (class object%
    (super-new)
    (field (part-name #f))
    (field (parent #f))
    (field (thread #f))))

(define reactor%
  (class part%
    (super-new)
    (field (body #f))))

(define container%
  (class part%
    (super-new)
    (field (parts '()))
    (field (connections (hash)))

    (define/public (decide-targets sender pin)
      (printf "Options : ~a" connections)
      (printf "Decided on ::~a\n" (hash-ref connections (list sender pin) (list)))
      (hash-ref connections (list sender pin) (list)))))

(define (send! target pin message)
  (printf "~a -> ~a::~a\n" message target pin)
  (thread-send (get-field thread target) (cons pin message)))

(define-syntax-rule (forever body)
  (begin
    (define (rec) body (rec))
    (rec)))

(define-syntax-parameter out!
  (lambda (stx)
    (raise-syntax-error (syntax-e stx)
                        "for use inside of the `define-reactor` macro")))

(define-syntax-rule (define-reactor name body)
  (define name
    (letrec ((self (new reactor%))
             (out!-fn (lambda (pin . message)
                        (let ((targets (send (get-field parent self) decide-targets self pin)))
                          (for-each
                           (lambda (target)
                             (send! target pin message))
                           targets)))))
      (set-field! 
       thread self
       (thread
        (lambda ()
          (forever
           (syntax-parameterize ((out! (make-rename-transformer #'out!)))
                                body)))))
      (set-field! part-name self 'name)
      self)))

(define-reactor printer
  (displayln (format "PRINTER :: ~a" (thread-receive))))

(define-reactor adder
  (out! 'sum (+ (cdr (thread-receive))
                (cdr (thread-receive)))))

(define box
  (let ((self-internal (new container%)))
    (set-field! parts self-internal (hash 'adder adder 'printer printer))
    (set-field! connections self-internal
                (hash (list self-internal 'in) (list adder)
                      (list adder 'sum) (list printer)))
    (for-each (lambda (part) (set-field! parent part self-internal))
              (hash-values (get-field parts self-internal)))
    (set-field!
     thread self-internal
     (thread
      (lambda ()
        (forever
         (let ((incoming (thread-receive)))
           (for-each (lambda (target)
                       (send! target 'in (cdr incoming)))
                     (hash-ref (get-field connections self-internal)
                               (list self-internal (car incoming))
                               (list))))))))
    self-internal))