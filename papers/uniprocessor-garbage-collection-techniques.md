# Uniprocessor Garbage Collection Techniques
[PDF link](www.cs.rice.edu/%7Ejavaplt/311/Readings/wilson92uniprocessor.pdf)

## 1 Automatic Storage Reclamation
### 1.1 Motivation

The setup. They basically lay out the arguments in favor of automatic memory management.
- Allows modularity *(since no individual module needs to be responsible for explicitly freeing memory, it's not necessary for any one module to know when a particular block of memory is no longer of interest to other modules)*
- Better maintainability and program structure *(since memory management is taken care of by the system, the programmer doesn't need to worry about making deallocation calls, or updating them when the code changes. Programmers are also, theoretically, freed from the need for stupid static allocation tricks to avoid managing particular parts of memory.)*
- In a large enough program, you will eventually need to do this yourself in any case.

They also seem to take the same approach as the Open Object Model guys, and pre-emptively start arguing that garbage collection is more efficient than manual memory allocation.

### 1.2 Two-Phase Abstraction

Garbage collectors break down into two phases

1. Distinguishing live objects from dead ones *("garbage detection")*
2. Reclaiming dead objects *("garbage reclamation")*

### 1.3 & 1.4

Quick notes about some details of real life implementations, and a note that the ideas being presented in this paper are slightly idealized. 1.4 is some meta-information about the rest of the paper.

### 2 Basic Garbage Collection Techniques

Here, we'll be discussing the basic garbage detection techniques of "reference counting" and "tracing".

### 2.1 Reference Counting

- Each object has a header that counts the number of pointers to it
- Each pointer creating/destroying primitive of the language updates said header information as appropriate
- When an object has 0 inbound pointers, it is collected immediately *(and recursively; any object which this one pointed to will be subject to the same decrement-and-potential-reclamation routine)*

Advantages:
- Consistent performance and incremental garbage detection *(no collection spikes, which makes it well suited for meeting real-time guarantees)*

Disadvantages:
- Ineffective in the face of certain kinds of garbage
- Inefficient proportionally to the pointer creation/reclamation rate

We'll get into both in a moment

#### 2.1.1 The Problem with Cycles

Because a reference counter keeps a count of the number of inbound references to a particular object, and decides when to reclaim on the basis of that count, an otherwise unreachable cycle of pointers will live forever.

The bumper-sticker takeaway is:

> If an object is not pointed to by any variable or other object, it is clearly garbage, but the converse is often not true.

#### 2.1.2 The Efficiency Problem

Whenever a pointer is created or destroyed, a count needs to be updated. If a variable's value is changed from one pointer two another, it's two counts. So the more pointers/objects a program establishes, the more overhead it will incur in the form of these count modifications. This bites especially hard with procedure arguments *(which are extremely short-lived, and therefore need lots of book-keeping given their lifetime)*

#### 2.1.3 Deferred Reference Counting

Basically:
- Don't update reference counts to stack objects immediately
- "Every now and then", do the necessary book-keeping for stack objects and collect any that need collecting

This avoids the disproportionate overhead of short-lived objects that we noted above, but still doesn't avoid the overhead incurred when heap objects need to point to each other.

#### 2.1.4 Variations on Reference Counting

- Use a small reference count *(as small as one bit)*, collect the obvious garbage the usual way, but use a backup tracing system to deal with the rest

Notes on usage in general:
- Useful in non-memory resource reclamation *(such as file handles)*
- Useful in languages that don't allow cyclic structures
- Useful in situations where most of the space will be occupied by long-lived objects
- Useful in places where you need guarantees on maximum collection time taken

### 2.2 Mark-Sweep Collection

- Periodically *(usually when there is no free space on a free-list)* start a collection cycle
- Mark all reachable memory starting from some root nodes
- Traverse all of memory, moving any unmarked chunks onto appropriate free-lists

Disadvantages:
- Difficult to handle different size objects without fragmenting memory
- Cost of collection is proportional to heap size *(since you need to mark all available memory, and traverse all memory during collection)*
- Poor locality of reference *(when you free objects, the live ones are left in-place, so what you get is large areas of free memory with some used chunks interspersed. This plays poorly with virtual memory systems)*

### 2.3 Mark-Compact Collection

- Periodically start a collection cycle as in 2.2
- Mark all reachable memory starting from some root nodes
- Traverse memory to find new places to put reachable nodes
- Traverse reachable memory again to move pointers to new locations
- Traverse reachable memory again to actually copy reachable nodes into new locations

Note that there's no free-lists involved here. Not entirely sure why the last two can't be interleaved. These collection strategies end up performing worse than Mark-Sweep if there's lots of memory to move around.

### 2.4 Copying Garbage Collection

Copying collectors pull a similar trick to Mark-Compact collectors; they don't use free-lists, and instead arrange reachable memory so that it's contiguous. Unlike the other two tracers, they do their collection inline with the marking phase.

#### 2.4.1 A Simple Copying Collector: "Stop-and-Copy" Using Semispaces

- The heap is divided into two semi-spaces, and one of them is designated `current`
- When the `current` space is full, trigger a collection cycle
- Traverse all reachable memory from some root nodes, copying each chunk into the other semi-space and leaving a forwarding pointer, then designate the other semi-space `current` *(does this imply that we shouldn't allocate things from the free list immediately? If we need forwarding pointers, that implies that some stuff still points into old memory, which means we might potentially cause some damage by repurposing an object before all pointers targeting it have been updated)*

This is what we implemented in SICP *(footnote 12 on pg 10 is an interesting historical note)*. The downside here is that you give up half of memory. The upside is that this collector style has much shorter pauses than 2.2 and 2.3. You can't be *too* naive, otherwise you'll get multiple copies of some used objects in the new semi-space.

#### 2.4.2 Efficiency of Copying Collection

- Classic space/time tradeoff
- Larger heap-spaces force collections less often *(hence incurring a lower cost for collection)*, but use more memory *(duh)*

### 2.5 Non-Copying Implicit Collection

- Works similarly to 2.4; two sets of memory *(instead of spaces)*, one of which is "current" and the other acts as a free-list
- At some threshold, a collection cycle is triggered, and each live object is copied into the currently empty free-list
- The free-list and current set of working memory then switch roles

The pointers used to organize memory in this scheme are behind the scenes *(not accessible from the programmers' perspective)*, which allows some compiler optimizations that aren't possible with other tracers *(details are promised later, when we get to parallel and real-time collectors)*.

### 2.6 Choosing Among Basic Tracing Techniques

Costs depend on:

1. Initial work required at each collection *(initial root set scanning)*
2. Work done at allocation
3. Work done at collection time *(the actual tracing)*

The "initial work" is fixed per program *(depending on the size of the root set)*, allocation work depends on number of objects allocated, and collection work depends on number of live objects.

- Copying collectors are less effective in systems where most memory is expected to be occupied

### 2.7 Problems With Simple Tracing Collectors

- Copying and Treadmill *(can't find a reference to this elsewhere; I'm reading it as the collector described in 2.4)* collectors have excellent asymptotic performance. Approaching zero with larger memory sizes. HOWEVER. Large amounts of memory are expensive, and we lose locality of reference *(usually, every location is used before any location is freed)*. So we can only grow heap so far until any advantage is negated by memory paging costs.
- This is a problem that affetcs, to some degree, all tracers and deferred reference counters *(generational collectors fight this be reusing a smaller area of memory more often)*
- Interesting footnote 13 about collectors that mitigate this cost. Also, a note about a paper that demonstrates that cyclic reuse is a poor match for hierarchical memory *(and a suggestion that LIFO reuse would be better than FIFO. That's... tempting enough for me to try to implement)*
- Pause times are disruptive when a collection cycle comes up *(OPINION: anecdotally, not as disruptive as you think)*, and Generational collectors fight it by lowering typical collection time *(ANOTHER OPINION: they do so at the cost of occasionally large collection time, which is worse from the users' perspective)*

### 2.8 Conservatism in Garbage Collection

- The ideal is to collect an object immediately after its last use. We can't because you can't tell in general when that happens

The general conservative assumptions:

1. any variable in the stack, globals or registers is live *(compilers may otimize away dead values, but anything that hasn't been so discarded is assumed to be live)*
2. a) Tracers only  -- nothing is garbage until we run out of space
   b) Counters only -- any pointer is treated as relevant *(even if it comes from a dead object)*
