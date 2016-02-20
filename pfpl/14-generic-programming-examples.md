#Generic Programming Examples for PFPL 14
##Understanding Map

An example could be a product type of strings and nats, 

`P4 := nat x str x nat X str`, 

and an instance of that type, 

`r = (6, "six", 8, "eight")`

To map `+ 7` across the `nat`s but leave the `str`s unchanged, we can identify the types of interested a type operator with `t.T` with, 

`T := t x str x t x str`.

We can then apply it to our instance `r` of type `P4`,

`map{t.T}(x.x+7)(r) -> map{t.t x str x t x str}(x.x+7)(r)`

Following the dynamics of chapter 14 we first expand the map across the product type `T` using rule (14.3c) repeatedly,

```
map{t.T}(x.x+7)(r) -> (map{t.t}(x.x+7)(6), 
                       map{t.string}(x.x+7)("six"),
                       map{t.t}(x.x+7)(8),
                       map{t.string}(x.x+7)("eight"))
```

Now this becomes a bit confusing to me as the rules in 14.3 don't specify what happens for type `t.str`, the only thing that makes sense is that we apply a version of 14.3b with `str` instead of `unit`.  14.3b says don't substitute anything in for x, instead just return the argument (Edit Exercise 14.2 asks to prove this property for any constant type operator, so this appears to be correct).  Then we get

```
map{t.T}(x.x+7)(r) -> (map{t.t}(x.x+7)(6), 
                       "six",
                       map{t.t}(x.x+7)(8),
                       "eight")
```
This leaves the two interesting entries where instead we apply rule 14.3a and substitute the argument for x and evaluate, 

```
map{t.T}(x.x+7)(r) -> (13, 
                       "six",
                       15,
                       "eight")
```

Ok so it seems like we can apply single functions, but what if we want to add 7 to both representations?  That is we want to get the result, `(13, "thirteen", 15, "fifteen")`?
What if I want to average the first and third values and save the result to a fifth component?

## Positive Type Operator
To explore the consequences of allowing non positive type operators, we can try an example, if I have the two functions,

```
f = lambda(x) x + 7
type(f) := nat -> nat
```
and

```
iseven = lambda(x) (x % 2) == 0
type(iseven) := nat -> bool
```
and I want to map `iseven` over the output of `f`,

```
map{t.nat -> t}(x.iseven x)(f)
 = lambda(x1 : nat)map{t.t}(x.iseven x)(f x1)
 = lambda(x1 : nat)(iseven f x1)
```
This new construct has the type, `nat -> bool` as expected, but let's say we instead tried to map over the argument.  Note this doesn't really make sense to do as the types won't match but the rules allow it if we don't restrict ourselves to positive type operators.  In this case the substitution occurs in the opposite order and we get a type error when we try to apply `f`, 

```
map{t.t -> nat}(x.iseven x)(f)
 = lambda(x1 : nat)(f map{t.t}(x.iseven x)(x1))
 = lambda(x1 : nat)(f iseven x1)
```
and now something is wrong since `f` is getting the wrong input type, `bool` instead of `nat`.  The restriction to positive type operators prevents this problem from happening though is likely not the only way.  This example suggests that if we had instead identified functions with identical input and output types that maybe that could solve the problem as well.  I guess we will have to wait and see why Harper chose to elaborate on this particular solution.
