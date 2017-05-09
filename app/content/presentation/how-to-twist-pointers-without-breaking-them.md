---
title: "How to Twist Pointers without Breaking Them"
url-video: "https://www.youtube.com/watch?v=f607zpe8-V8"
url-slides: "about:blank"
authors:
 - Satvik Chauhan
 - Piyush Kurur
 - Brent Yorgey
source: http://icfp16.sigplan.org/event/haskellsymp-2016-papers-how-to-twist-pointers-without-breaking-them
tags:
 - Category Theory
 - Cryptography
libraries:
 - raaz
---

Using the theory of monoids and monoid actions, we give a unified framework that handles three common pointer manipulation tasks, namely, data serialisation, deserialisation, and memory allocation. Our main theoretical contribution is the formulation of the notion of a *twisted functor*, a generalisation of the semi-direct product construction for monoids. We show that semi-direct products and twisted functors are particularly well suited as an abstraction for many pointer manipulation tasks.

We describe the implementation of these abstractions in the context of a cryptographic library for Haskell. Twisted functors allow us to abstract all pointer arithmetic and size calculations into a few lines of code, significantly reducing the opportunities for buffer overflows.
