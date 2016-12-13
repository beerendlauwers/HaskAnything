---
title: "LVars: lattice-based data structures for deterministic parallel and distributed programming"
url-video: https://www.youtube.com/watch?v=8dFO5Ir0xqY
url-slides: about:blank
authors: Lindsey Kuper
source: http://www.composeconference.org/2015/program/index.html
conference: Compose2015
tags:
- Parallel Programming
- LVars
- Distributed Programming
libraries:
 - lvish
---

**NOTE:** The presentation given at Compose appears to not have been recorded. A link to another instance of her talk at RICON West 2013 was used as a replacement.

Parallel programming is notoriously difficult. A fundamental reason for this difficulty is that programs can yield inconsistent answers, or even crash, due to unpredictable interactions between parallel tasks. But it doesn't have to be this way: guaranteed-deterministic parallel programming models promise freedom from subtle, hard-to-reproduce nondeterministic bugs. In a deterministic program, the same input always produces the same output; in a guaranteed-deterministic parallel programming model, then, that property is true of every program written using the model, even when those programs are run in parallel.

So, what are these parallel programming models that guarantee determinism, and how do they work? In this talk, we'll survey the landscape of guaranteed-deterministic approaches to parallel programming. Then, we'll dig into lattice-based data structures, or LVars, which are the basis for a new guaranteed-deterministic parallel programming model that my colleagues and I developed. Throughout the talk, we'll look at examples written using [LVish](http://hackage.haskell.org/package/lvish), our Haskell library for programming with LVars. Finally, we'll consider the relationship between LVars and conflict-free replicated data types for eventually consistent distributed systems, like the system that stores the contents of your Amazon shopping cart.
