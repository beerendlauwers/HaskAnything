---
title: Theorem Proving For All
paper-url: https://arxiv.org/pdf/1806.03541.pdf
authors: 
 - Niki Vazou
 - Joachim Breitner
 - Will Kunkel
 - David Van Horn
 - Graham Hutton
tags:
 - Refinement Types
libraries:
 - liquidhaskell
---

Equational reasoning is one of the key features of pure functional languages such as Haskell. To date, however, such reasoning always took place externally to Haskell, either manually on paper, or mechanised in a theorem prover. This article shows how equational reasoning can be performed directly and seamlessly within Haskell itself, and be checked using Liquid Haskell. In particular, language learners --- to whom external theorem provers are out of reach --- can benefit from having their proofs mechanically checked. Concretely, we show how the equational proofs and derivations from Graham's textbook can be recast as proofs in Haskell (spoiler: they look essentially the same).
