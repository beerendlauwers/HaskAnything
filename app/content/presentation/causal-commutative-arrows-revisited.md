---
title: "Causal Commutative Arrows Revisited"
url-video: "https://www.youtube.com/watch?v=bnFHYsL4QNc"
url-slides: "http://www.thev.net/PaulLiu/download/cca-revisit-slides.pdf"
authors:
 - Jeremy Yallop
 - Hai Liu
source: http://icfp16.sigplan.org/event/haskellsymp-2016-papers-causal-commutative-arrows-revisited
tags:
 - Arrows
libraries:
---

Causal commutative arrows (CCA) extend arrows with additional constructs and laws that make them suitable for modelling domains such as functional reactive programming, differential equations and synchronous dataflow.

Earlier work has revealed that a syntactic transformation of CCA computations into normal form can result in significant performance improvements, sometimes increasing the speed of programs by orders of magnitude.

In this work we reformulate the normalization as a type class instance and derive optimized observation functions via a specialization to stream transformers to demonstrate that the same dramatic improvements can be achieved without leaving the language.
