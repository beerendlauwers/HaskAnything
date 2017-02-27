---
title: "'There and Back Again' and What Happened After"
url-video: https://www.youtube.com/watch?v=u_OsUlwkmBQ
url-slides: https://github.com/kwf/TABA-AWHA
authors:
 - Kenneth Foner
source: http://www.composeconference.org/2016/speakers/
conference: Compose2016
tags:
libraries:
---

Danvy and Goldberg’s 'There and Back Again' (1) puts the 'fun' in 'functional pearl'. This classic, underappreciated paper describes a clever new pattern for implementing a large family of functions over lists while using only one traversal, rather than the multiple traversals that many other approaches require. The technique naturally gives rise to elegant algorithms for computing symbolic convolutions, generalized palindrome tests, and Catalan numbers.

In the introduction to the paper, the authors remark that in a dependently typed language it would be possible to give precise types to analogous functions over length-indexed lists–lists which carry their lengths in their types. We take this as a challenge, translating the 'There and Back Again' (TABA) pattern into modern idiomatic Haskell, making inherent use of cutting-edge features of GHC's type system.

Reconstructing 'There and Back Again' in this richer setting requires us to elucidate some subtle arithmetic invariants of the pattern, both to ourselves and to the type system. To automatically verify the tricky arithmentic latent in the pearl, we use GHC's brand new type-checker plugin interface to augment our type system with the power of the Z3 theorem prover 2. Writing down these precise types also gives us new insight into the structure of this programming pattern. Along this journey of translation, we may simultaneously satisfy the type-checker, the theorem-prover, and our own curiosity.

1: Danvy, Olivier and Goldberg, Mayer. 'There and Back Again.' Basic Research in Computer Science 66.4 (2005): 397-413.
2: Diatchki, Iavor S. 'Improving Haskell Types with SMT.' Proceedings of the 8th ACM SIGPLAN Symposium on Haskell. ACM, 2015.
