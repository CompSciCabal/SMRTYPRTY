NB. SICP 2.1.4 - Interval Arithmetic in J
NB. Implemented at http://www.meetup.com/Toronto-Code-Retreat/events/148531162/

NB. All verbs have explicit rank 1 so that they can be
NB. easily applied to N-dimentional case.

center =: (+/ % 2:) " 1
width  =: (-/ @ |. % 2:) " 1

NB. Couple helper verbs
cross  =: (* |.) " 1 1
minmax =: (<./ , >./) " 1

add  =: + " 1 1
sub  =: (- |.) " 1 1
prod =: (minmax @ (* , cross)) " 1 1
div  =: (([ % 0:) ` (prod %) @. (0: < */ @ ])) " 1 1


NB. TESTS
NB. N-dimentional cube is represented by its diagonal.
NB. The diagonal is implemented as a pair of points in R^N space (two N-tuples).
NB. Here is an example of two 3D cubes

A =: i.3 2
NB. 0 1
NB. 2 3
NB. 4 5

B =: 2 + i.3 2
NB. 2 3
NB. 4 5
NB. 6 7

A add B
NB.  2  4
NB.  6  8
NB. 10 12

A sub B
NB. _3 _1
NB. _3 _1
NB. _3 _1

A prod B
NB.  0  3
NB.  8 15
NB. 24 35

A div B
NB.        0      0.5
NB.      0.4     0.75
NB. 0.571429 0.833333


NB. The original 1D example from SICP is just a special case

1 2 add  3 4        NB. = 4 6
1 2 sub  3 4        NB. = _3 _1
1 2 prod 3 4        NB. = 3 8
1 2 div  3 4        NB. = 0.25 0.666667


NB. One thing to note is a division by zero in J

 1 2 div _3 4       NB. = _ _
_1 2 div _3 4       NB. = __ _
_1 0 div _3 4       NB. = __ 0
 0 1 div _3 4       NB. = 0 _
