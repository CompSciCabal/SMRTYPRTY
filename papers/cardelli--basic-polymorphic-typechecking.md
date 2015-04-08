# Basic Polymorphic Typechecking
###### Luca Cardelli

- Mostly focused on implicit parametric polymorphism (Functions that can accept multiple types in some or all of their arguments, and do so with the use of type variables, rather than concrete type annotations at definition)
	- Implicit polymorphism can be thought of as abbreviated explicit polymorphism
	- Implicit is less verbose (because the system discovers type information, rather than having it specified by the programmer)
	- Explicit is more flexible (because there are otherwise valid types that cannot be discovered automatically)
	- Implicit vs. Explicit is a continuum (it's possible to be entirely implicit, entirely explicit, or some in-between state where some information is explicitly specified while some is discovered by the compilaion mechanisms)

- History section mentions shout-outs to Strachey, Curry, Hindley, Robinson, Gordon, Milner, Damas, Coppo, MacQueen, Bruce, McCracken and an earlier Cardelli
	- In that order. It's not chronological, but it looks like the foundational paper was Strachey.

- "Polymorphism comes from the interaction of two contrasting programming language design goals: static typing and reusability" pg 4
	- Static typing and related systems are concerned with preventing certain classes of runtime faults through static analysis
	- Reusability is the ability to write procedures for "[an] open ended collection of applications"
	- Static typechecking prevents some reusability and reusable programs are harder to check statically
	- Polymorphic systems try to get the best of both worlds (getting back some reusability while maintaining static analysis capabilities)

- The Modula-2 program at the end of the paper is included "In the hope of making the algorithm accessible to a larger group of people".

- The language we're demonstrating inference on is not an ML-style language (though Cardelli has high praise for its inferencer). Concrete syntax given on pg 5.

- A type can be
	- a variable (a, b, numeric, y)
	- a nullary operator (int, bool)
	- a parametric operator (→, ×) *those should probably be -> and * for ease of typing*
