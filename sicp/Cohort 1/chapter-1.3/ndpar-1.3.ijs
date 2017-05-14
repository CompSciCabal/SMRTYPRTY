NB. Exercise 1.29, p.60
NB. http://www.jsoftware.com/svn/addons/trunk/math/misc/integrat.ijs

simpson =: 2 : 0
'a b n' =. 3{. y,128
h =. (b-a) % n
val =. x a + h * i. >: n
h * +/ val * 3%~ 1,((n-1)$4 2),1
)

NB. Exercise 1.33, p.61
NB. http://www.rogerstokes.free-online.co.uk/14.htm

ACC =: 1 : 0
com =: u @. 0
map =: u @. 1
fil =: u @. 2
((com /) @: map @: (#~ fil)) f.
)

square_primes =: (+ ` *: ` (1&p:)) ACC

echo square_primes 1 2 3 4


NB. Exercise 1.35, p.70
NB. http://www.rogerstokes.free-online.co.uk/13.htm

FPF =: 1 : '(u ^: _ ) 0.5'     NB. fixed-point-finder adverb with initial guess 0.5
phi =: 1: + %                  NB. iterative function to calculate phi

echo phi FPF
