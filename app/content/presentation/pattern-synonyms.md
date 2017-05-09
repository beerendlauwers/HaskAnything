---
title: "Pattern Synonyms"
url-video: "https://www.youtube.com/watch?v=Zv4u3OZzsZo"
url-slides: "about:blank"
authors:
 - Matthew Pickering
 - Gergo Erdi
 - Simon Peyton Jones
 - Richard Eisenberg
source: http://icfp16.sigplan.org/event/haskellsymp-2016-papers-pattern-synonyms
tags:
 - GHC
libraries:
---

Pattern matching has proven to be a convenient, expressive way of inspecting data. Yet this language feature, in its traditional form, is limited: patterns must be data constructors of concrete data types. No computation or abstraction is allowed. The data type in question must be concrete, with no ability to enforce any invariants. Any change in this data type requires all clients to update their code.

This paper introduces pattern synonyms, which allow programmers to abstract over patterns, painting over all the shortcomings listed above. Pattern synonyms are assigned types, enabling a compiler to check the validity of a synonym independent of its definition. These types are intricate; detailing how to assign a type to a pattern synonym is a key contribution of this work. We have implemented pattern synonyms in the Glasgow Haskell Compiler, where they have enjoyed immediate popularity, but we believe this feature could easily be exported to other languages that support pattern matching.
