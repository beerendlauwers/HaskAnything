---
title: Applications of Datatype Generic Programming in Haskell
url-video: https://www.youtube.com/watch?v=ZtWzh4mBzaI
url-slides: http://bobkonf.de/2016/slides/hahn.pdf
authors: Sönke Hahn
source: http://bobkonf.de/2016/hahn.html
conference: BOB2016
tags:
 - Generic Programming
 - Algebraic Datatypes (ADTs)
libraries:
---

Datatype Generic Programming (DGP) allows to write code that operates on arbitrary Algebraic Datatypes (ADTs). The classic example is serialization and deserialization, but DGP opens up a wide field of applications. And while the support for generic serialization (and some other use-cases) through existing libraries is very stable and mature, other areas of applications of DGP still deserve further exploration. (E.g. database schema generation, database access, generic UIs to modify values, test data generation, etc.)

Haskell's static type system, its ADTs and its type classes pose unique challenges not found in other mainstream languages, but also offer unique benefits. On top of that type-level programming (e.g. with type families) opens up even more possibilities.

This talk gives an introduction to the principles of DGP in Haskell and discusses possible applications – both by looking at existing libraries and by providing an outlook on what could be done with DGP in the future.
