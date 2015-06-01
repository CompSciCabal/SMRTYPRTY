## Multiple representations for Abstract Data

##### General
- How does this compare to the approach we took with rational number/point/vector arithmetic?
- What are the pros and `cons` of each approach?
- Most of this chapter is dedicated to problems related to programming in the large. Can you think of problems (other than type conflicts, obviously) emergent in teams of multiple programmers? In teams composed of multiple teams?
- Can you think of other solutions to the problem of representation collision than the one presented?

##### 2.4.2
- What problems does the type tagging process solve (both computational and social)? Are there places where any of these problems could still arise? How would you close the loop?

##### 2.4.3
- The name used for "programming with a type-dispatch table in mind" is "Data-directed programming". Is this an appropriate name? Have you heard others?
- Have you seen something that takes the columnar ("intelligent data object") approach described under the *Message passing* heading?

##### Homework

- 2.73:
    a) What was done? Why can't we do the same for `number?` and `variable?`
	b) Show the corresponding methods for sums and products.
	c) Show the additional methods for something else.
	d) How does your implementation change with changes to the tag order?

- 2.74:
    a) Implement `get-record` for ~~IBM~~ ~~Microsoft~~ ~~Google~~ Insatiable Inc. How should records be structured for this?
	b) Implement `get-salary`. How should the record be structured for this?
	c) Implement `find-employee-record`.
	d) When ~~IBM~~ ~~Microsoft~~ ~~Google~~ Insatiable takes over a new company, what do they need to change to incorporate into the central system?

- 2.75: Implement `make-from-mag-ang` in message-passing style
- 2.76: Describe describe which of `explicit-dispatch`, `data-directed` and `message-passing` styles is most appropriate for:
    a) a system in which new types are often added?
	b) a system in which new operations must often be added?
