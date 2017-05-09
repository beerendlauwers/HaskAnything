---
title: "Free Delivery"
url-video: "https://www.youtube.com/watch?v=Z8HfOuFqhU4"
url-slides: "about:blank"
authors:
 - Jeremy Gibbons
source: http://icfp16.sigplan.org/event/haskellsymp-2016-papers-free-delivery-functional-pearl-
tags:
 - Distributed Computing
 - Applicative Functors
libraries:
---

Remote procedure calls are computationally expensive, because network round-trips take several orders of magnitude longer than local interactions. One common technique for amortizing this cost is to batch together multiple independent requests into one compound request. Batching requests amounts to serializing the abstract syntax tree of a small program, in order to transmit it and run it remotely. The standard representation for abstract syntax is to use free monads; we show that free applicative functors are actually a better choice of representation for this scenario.
