---
title: "Improving Type Error Localization for Languages with Type Inference"
url-video: https://www.youtube.com/watch?v=BMT6MZ5zuvw
url-slides: /files/presentation/presentation--improving-type-error-localization-for-languages-with-type-inference.pdf
authors: Zvonimir Pavlinovic, Tim King and Thomas Wies (speaker)
source: http://www.composeconference.org/2016/speakers/
conference: Compose2016
tags:
 - Program Analysis
 - Type Inference
libraries:
---

Functional programming languages have spearheaded groundbreaking innovations in programming language design and implementation. Garbage collection, type inference, higher-order functions, and polymorphic type abstractions are just some examples of innovative features that have emerged from research in these languages. Expert programmers cherish these features because they can significantly increase a programmer’s productivity. However, these advanced language features are often difficult to master because they require a deep understanding of the inner workings of the compiler.

Type inference is a prime example of a language feature that can boost productivity but can also be a usability nightmare. A type inference algorithm infers the types of expressions in a program based on how these expressions are used. The compiler can thus provide strong static correctness guarantees about a program without requiring the programmer to annotate the program with type information. On the other hand, automated type inference can also be counterproductive. If a conflicting use of an expression (i.e. a type mismatch) is detected, the compiler reports the corresponding program location as the source of the type error. However, the actual error source is often elsewhere in the program. This results in confusing error messages. The programmer needs to understand the basic workings of the inference algorithm to interpret the error message correctly and debug the program. This can often be a frustrating experience.

In this talk, we will explain how type inference algorithms work and why type error localization is so difficult in the presence of type inference. We will then present a new algorithm for localizing type errors that provides formal quality guarantees about the identified type error sources. The algorithm works efficiently in practice and has the potential to significantly improve the quality of type error reports produced by state-of-the-art compilers.
