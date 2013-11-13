NB. Exercise 1.33, p.61
NB. http://www.rogerstokes.free-online.co.uk/14.htm

ACC =: 1 : 0
com =: u @. 0
map =: u @. 1
fil =: u @. 2
((com /) @: map @: (#~ fil)) f.
)

square_primes =: (+ ` *: ` (1&p:)) ACC

NB. square_primes 1 2 3 4
