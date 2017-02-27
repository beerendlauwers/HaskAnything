---
title: Verdict - Reified refinement
url-video: https://www.youtube.com/watch?v=w9X1I1ySCCM
url-slides: https://raw.githubusercontent.com/jkarni/bobkonf/master/presentation.md
authors:
 - Julian Arni
source: http://bobkonf.de/2016/arni.html
conference: BOB2016
tags:
 - DSL
libraries:
 - verdict
---

[Verdict](https://hackage.haskell.org/package/verdict) is a Haskell library that provides a way of reifying constraints on a type. It exposes a type-level DSL for constraints, and provides functions to verify that those constraints hold of a value, to represent those constraints as JSON schema, to deserialize values with validation, to deal with implications between constraints, and others. We will see how this provides us with a lightweight and practical (at the cost of some safety and runtime checks) contract system.
