# Simulator

To run register machine code,

- load and run `simulator.rkt` in DrRacket;
- evaluate `make-machine` code in DrRacket's REPL;
- `start` the machine in DrRacket's REPL.

Example

    (define a-machine
      (make-machine
        (list <of operations>)
        '(<instructions>)))

    (start a-machine)


# Compiler

To compile Scheme source code to register machine object code,

- load and run `compiler.rkt` in DrRacket;
- `compile` the code in DrRacket's REPL.

Example

    (display (compile
     '(define (factorial n)
        (if (= n 1)
            1
            (* (factorial (- n 1)) n)))
     'val
     'next))


# Evaluator

To start evaluator, load and run `evaluator.rkt` in DrRacket's REPL.

Then in the evaluator's REPL, you can either evaluate Scheme expressions

    (define (factorial n)
        (if (= n 1)
            1
            (* (factorial (- n 1)) n)))

    (factorial 5)

or compile Scheme expressions first and then run them

    (compile
     '(define (factorial n)
        (if (= n 1)
            1
            (* (factorial (- n 1)) n))))

    (factorial 5)
