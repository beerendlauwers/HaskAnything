---
title: Dynamic Typing in GHC
url-video: https://www.youtube.com/watch?v=asdABzBUoGM
url-slides: about:blank
authors:
 - Stephanie Weirich
source: http://www.composeconference.org/2016/speakers/
conference: Compose2016
tags:
libraries:
---

The ability to perform type tests at runtime blurs the line between statically-typed and dynamically-typed languages. Recent developments in Haskell’s type system allow even programs that use reflection to themselves be statically typed, using a type-indexed runtime representation of types called TypeRep.

The Glasgow Haskell Compiler currently supports the definition of a Dynamic type through the use of the Typeable type class. This definition works well with Haskell’s open universe of types. All new types are automatically convertible to type Dynamic. However, this implementation of Dynamic is also unsafe: the implementation of operations such as dynApply—which applies one Dynamic as a function to another—requires unsafeCoerce.

In this talk, I will present an improved interface in the Typeable type class, leading to a safer implementation of type Dynamic. Our revised Typeable class requires a new feature, kind equalities, that will soon be available in GHC.

(This is joint work with Simon Peyton Jones, Richard Eisenberg and Dimitrios Vytiniotis.)
