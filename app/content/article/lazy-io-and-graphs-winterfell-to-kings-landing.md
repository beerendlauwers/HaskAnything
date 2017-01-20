---
title: "Lazy I/O and graphs: Winterfell to King's Landing"
date: 2017-01-17
permission-file: 
url: "https://jaspervdj.be/posts/2017-01-17-lazy-io-graphs.html"
authors:
 - Jasper Van der Jeugt
type: "article"
tags:
 - "Graphs"
libraries:
---

Introduction of the article:

> This post is about Haskell, and lazy I/O in particular. It is a bit longer than usual, so I will start with a high-level overview of what you can expect:
> 
> * We talk about how we can represent graphs in a “shallow embedding”. This means we will not use a dedicated Graph type and rather represent edges by directly referencing other Haskell values.
> 
> * This is a fairly good match when we want to encode infinite 1 graphs. When dealing with infinite graphs, there is no need to “reify” the graph and enumerate all the nodes and egdes – this would be futile anyway.
> 
> * We discuss a Haskell implementation of shortest path search in a weighted graph that works on these infinite graphs and that has good performance characteristics.
> 
> * We show how we can implement lazy I/O to model infinite graphs as pure values in Haskell, in a way that only the “necessary” parts of the graph are loaded from a database. This is done using the unsafeInterleaveIO primitive.
> 
> * Finally, we discuss the disadvantages of this approach as well, and we review some of common problems associated with lazy I/O.
