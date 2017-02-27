---
title: Cryptography and verification with Cryptol
url-video: https://www.youtube.com/watch?v=sC2_5WaavFc
url-slides: about:blank
authors:
 - Austin Seipp
source: http://www.composeconference.org/2016/speakers/
conference: Compose2016
tags:
 - Cryptography
 - DSL
libraries:
---

Cryptographic primitives exist all through-out the modern software stack, yet their construction and composition is often delicate and error prone. Furthermore, specifications are often far removed from real implementations, and written in high level prose or pseudo-code - while we tend to implement such software in low-level, bug-prone programming languages.

Cryptol is a domain-specific language, inspired by Haskell, designed for the construction and verification of cryptographic software. Cryptol programs often serve as ‘executable specifications’ of some design, yielding easy to understand programs that serve as excellent references. Furthermore, through a novel use of SAT-based verification tools, Cryptol can allow you to verify real world software conforms to the specification in an easy, automated fashion.

This talk will primarily focus on the relevant aspects of writing and using the Cryptol toolkit, including verification on real world cryptographic functions written in C and Java, along with some notes on its implementation.
