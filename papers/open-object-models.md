# Open, Extensible Object ModelsNotes
[PDF link](piumarta.com/software/cola/objmodel2.pdf)

### The Paper

- The little **Abstract** sub-section is why I found this paper interesting enough to recommend. If you remove that parenthetical comment comparing object systems, it strikes me as a pretty deep truth about language design that you want the user to be able to make radical transformations to the underlying language in the course of their work (the left out part is that, as much as possible, those radical transformations should not prevent particular programs written in the language from easily interoperating with other such programs).

- Minimal object implementation. We seem to need three types (`object`, `symbol` and `vtable`) along with five methods (`intern`, `addMethod`, `lookup`, `allocate` and `delegated`) to bootstrap the entire thing. The minimalist approach is somewhat reminiscent of our scheme adventures, though this paper does not present a full interpreter.

- Why aren't `send` and `bind` methods themselves? I'm assuming performance reasons (they go so far as to present a macro implementation of `send`). Relatedly, why implement `send` as a macro even in the absence of an inline cache?

- Unimpressed by the typesetting here. The pg7/pg8 split is a particularly egregious example.

- The benchmarks seem to claim that the implementation of `send` using inline caching outperforms a built-in `switch` statement. Thoughts?

- The conclusion cites Smalltalk-80's object system and the McCarthy paper.
