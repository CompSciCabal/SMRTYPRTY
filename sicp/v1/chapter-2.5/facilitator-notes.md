# 2.5.3 -- Symbolic Algebra
###### Why do I always get the mathy ones?

## General questions

- Have you ever used "data directed recursion" before?
- Why don't we apply the same tactic to `terms` as well as polynomials?

## Exercises

#### 2.87
Install `=zero?` for polynomials.

#### 2.88
Extend the poly system to include subtraction.
- Did you find generic negation to be useful?

#### 2.89
Define procedures that implement the term-list representation described above for dense polynomials.

#### 2.90
Make polys generic over sparse and dense representations.
- Did anyone do it?
- What problems did you run into?
- What did you end up changing?

#### 2.91
Too long to summarize; pg 312 in my PDF

#### 2.92
Impose ordering on variables and make mul/add work for polynomials in different variables.
- Did anyone do it?
- How `not easy` was it?
- Where in particular did you hit problems?

#### 2.93
Modify rational-arithmetic to use generic operations, but change `make-rat` so that it does not attempt to reduce to lowest terms.

#### 2.94
Using `div-terms`, implement `remainder-terms`.
