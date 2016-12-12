---
title: Analyzing Programs with Z3
url-video: https://www.youtube.com/watch?v=ruNFcH-KibY
url-slides: jelv.is/talks/compose-2016/slides.html
authors: Tikhon Jelvis
source: http://www.composeconference.org/2016/speakers/
conference: Compose2016
tags:
- Z3
- Liquid Types
- Refinement Types
- Program Analysis
libraries: 
---

SMT solvers are widely used in research to analyze and verify programs. This lets us check invariants and compare programs against a spec exhaustively, with bounds on the number of loop iterations and the size of the heap. SMT solvers are also useful for other sorts of analysis including sophisticated type checking (like refinement types in Liquid Haskell) and fields other than program analysis (like security research where they can be used to analyze cryptographic algorithms and protocols). 

I’ll demonstrate how to compile a simple language to an SMT formula and analyze programs using the Haskell Z3 bindings. Z3 has bindings in other languages including OCaml and .NET, so these ideas will be immediately useful to everyone even if the details are slightly different from the Haskell code. The underlying ideas will also help people approach other problems with Z3.
