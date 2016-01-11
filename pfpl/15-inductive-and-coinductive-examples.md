# Examples for Inductive and Coinductive Types: PFPL 15
## Enumerating the Nats
 Expanding the first few iterations of `fold` (omitting the subscripted type specifier,  `nat`, I.e `fold` is actually `fold_nat`).  By doing this we see where the sum types `unit + nat` are used and where just `nat` is used.  Enumerating means alternately folding and then right injecting into the sum type.
 
 ```
 e0 = l.()
 nat0 = fold(e0)
 e1 = r.nat0
 nat1 = fold(e1)
 e2 = r.nat1
 ```
 The expressions `ei` are valid inputs to fold of type `unit + nat` and the `nat`s are the output of `fold`.
## Stepping with nats example
We can look at a simple example application of the dynamics of the recursor (rule 15.2c).  
Let's say we have an abstractor, 

```
x.e1 = x.map{t.unit + t}(y.fold(r.fold(y)))(x)
     = x.case x {l._ -> nat0 | r.y -> r.fold(r.fold(y))}
```
which doubles any `nat`.  The case analysis expansion is the same as what is done when expanding the rule 15.2c in the text.
And we also have an input expression, 

```
e2 = r.nat2
```

Then we can evaluate the expression using `rec` and we will expect the result to be double our input, 

```
rec(x.e1; fold(e2)) = 
```


## Enumerating the conats
- Writing down the definition of the conats in the same reduced form as for the nats.
- Repeating the stepping example but with the infinite streams

After that I want to try some things combining product types, nested sum types
