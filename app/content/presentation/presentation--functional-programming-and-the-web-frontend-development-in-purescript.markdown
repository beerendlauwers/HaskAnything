---
title: "Functional Programming and the Web: Frontend Development in PureScript"
url-video: https://www.youtube.com/watch?v=EArV7Uy-TD0
url-slides: http://bobkonf.de/2016/slides/karg.pdf
authors: Michael Karg, Jürgen Nicklisch-Franken
source: http://bobkonf.de/2016/karg.html
conference: BOB2016
tags:
 - Purescript
libraries:
---

PureScript is a strongly-typed functional programming language that compiles to JavaScript. Its roots lie in the Haskell world, and the (open source) PureScript compiler itself is implemented in Haskell.

It aims to be a general-purpose language for the Web; the code produced by the compiler is standalone, and as such does not require any additional runtime system library, and can be seamlessly integrated with existing JavaScript libraries or frameworks.

While PureScript’s syntax may be very close to Haskell’s, there are interesting conceptual differences between the two languages, such as PureScript’s strict evaluation, or the use of extensible effects in PureScrips’s type system.

At Symbolian GmbH, we use PureScript as a main tool in the development of a WebGL-based 3D visualization library for big (and small) data. Whilst this being a commercial closed-source product, we have contributed to the PureScript ecosystem over the course of its development: PureScript’s bindings to WebGL as well as various utility packages for matrix and vector datatypes have been released by Symbolian.

In our talk, we want to give a short overview over PureScript and the state of its ecosystem. Then, we want to share our experiences with using this relatively young language for a production-grade software with respect to some real-world challenges like e.g. performance profiling, compiler optimization of the generated JavaScript code, or interface stability.
