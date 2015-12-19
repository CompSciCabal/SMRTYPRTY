#Infinite Data Types, PFPL 14 and 15


##Understanding Map

An example could be a product type of strings and nats, 

`T := nat x str x nat X str`, 

and an instance of that type, 

`r = (6, "six", 8, "eight")`

To map `+ 7` across the `nat`s but leave the `str`s unchanged, we can identify the types of interest with `t` and write, 

`map{t.T}(x.x+7)(r)`

Following the dynamics of chapter 14 we first expand the map across the product type `T` using rule (14.3c) repeatedly,

```
map{t.T}(x.x+7)(r) -> (map{t.t}(x.x+7)(6), 
                       map{t.string}(x.x+7)("six"),
                       map{t.t}(x.x+7)(8),
                       map{t.string}(x.x+7)("eight"))
```

Now this becomes a bit confusing to me as the rules in 14.3 don't specify what happens for type `t.string`, the only thing that makes sense is that we apply a version of 14.3b with string instead of unit.  14.3b says don't substitute anything in for x, instead just return the argument.  So we get

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
To explore the consequences of allowing non positive map, we can try an example, if I have the function,

```
f := lambda(x) x + 7
type(f) := nat -> nat
```
and we have the double apply function for functions of type `nat -> nat`, 

```
double := lambda(f) f f
type(double) := (nat -> nat) -> (nat -> nat)
```
Now if we want to map double on f we do, 

```
map{t.t -> t}(x.double x)(f)
```
where `t = nat -> nat`, so we can apply a variant of the rule given in 14.5 to get, 

```
map{t.t -> t}(x.double x)(f) 
 = lambda(x1 : nat -> nat)map{t.t}(x.double x)(f x1)
 = lambda(x1 : nat -> nat)(double f x1)
```
Something is weird here since x1 has the wrong type to be an argument for f, probably a mistake.
