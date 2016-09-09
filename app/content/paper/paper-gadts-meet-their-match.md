---
title: GADTs Meet Their Match
paper-url: http://research.microsoft.com/en-us/um/people/simonpj/papers/pattern-matching/gadtpm.pdf
authors-paper: Georgios Karachalias, Tom Schrijvers, Dimitrios Vytiniotis and Simon Peyton Jones
tags: gadts
---

For ML and Haskell, accurate warnings when a function definition
has redundant or missing patterns are mission critical. But today's
compilers generate bogus warnings when the programmer
uses guards (even simple ones), GADTs, pattern guards, or view
patterns. We give the first algorithm that handles all these cases in
a single, uniform framework, together with an implementation in
GHC, and evidence of its utility in practice.