---
title: Liquid Types for Haskell
url-video: https://www.youtube.com/watch?v=LEsEME7JwEE
url-slides: http://goto.ucsd.edu/~nvazou/compose16/_site/01-index.html
authors: Niki Vazou
source: http://www.composeconference.org/2016/speakers/
conference: Compose2016
tags: 
- Liquid Types
- Program Analysis
libraries: 
---

Haskell has many delightful features, perhaps the most beloved of which is its type system which allows developers to specify and verify a variety of program properties at compile time. However, many properties, typically those that depend on relationships between program values are impossible, or at the very least, cumbersome to encode within Haskell's type system. 

Liquid types enable the specification and verification of value-dependent properties by extending Haskell’s type system with logical predicates drawn from efficiently decidable logics. 

In this talk, we will start with a high level description of Liquid Types. Next, we will present an overview of LiquidHaskell, a liquid type checker for Haskell. In particular, we will describe the kinds of properties that can be checked, ranging from generic requirements like totality (will `head` crash?) and termination (will `mergeSort` loop forever?), to application specific concerns like memory safety (will my code SEGFAULT?) and data structure correctness invariants (is this tree BALANCED?).

Joint work with: Ranjit Jhala, Eric Seidel, Patrick Rondon, Dimitris Vytiniotis and Simon Peyton-Jones.
