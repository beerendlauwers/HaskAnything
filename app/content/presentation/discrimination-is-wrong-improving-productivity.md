---
title: "Discrimination is Wrong: Improving Productivity"
url-video: "https://www.youtube.com/watch?v=cB8DapKQz-I"
url-slides: "https://github.com/meiersi/HaskellerZ/blob/master/meetups/20150530-ZuriHac2015_Edward_Kmett-Discrimination_is_Wrong_Improving_Productivity/Discrimination%20-%20Zurihac.pdf"
authors:
 - Edward Kmett
source: https://www.youtube.com/watch?v=cB8DapKQz-I
tags:
 - Category Theory
 - Contravariance
 - Final Encoding
libraries:
 - discrimination
---

This talk is a case study in library design in Haskell.

Fritz Henglein has shown through a number of excellent papers how to use "discrimination" to do lots of things in O(n): Sorting many more data types than you'd expect, table joins, etc.

In the process of optimizing this approach and wrapping it up in a form that can be easily consumed, we'll take a lot of detours through the different ways you can think about code when optimizing Haskell.

We'll need some category theory, from a deeper understanding of monoids to Day convolution.

We'll need to consider final and initial encodings.

We'll need to drift down to low level system concerns from building a custom foreign prim to nesting unsafePerformIO within unsafePerformIO.

We'll need properties of laziness from productivity to IVars.

Along the way we'll find and fix a small problem with the initial discrimination paper, which opens the door to streaming results, rather than having to wait until all the input is ready.
