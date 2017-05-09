---
title: "Desugaring Haskell's do-Notation into Applicative Operations"
url-video: "https://www.youtube.com/watch?v=6WM4gFP7rs4"
url-slides: "about:blank"
authors:
 - Simon Marlow
 - Simon Peyton Jones
 - Edward Kmett
 - Andrey Mokhov
source: http://icfp16.sigplan.org/event/haskellsymp-2016-papers-desugaring-haskell-s-do-notation-into-applicative-operations
tags:
 - GHC
 - Haskell In Production
libraries:
---

Monads have taken the world by storm, and are supported by
do-notation (at least in Haskell). Programmers are increasingly
waking up to the usefulness and ubiquity of Applicatives, but they
have so far been hampered by the absence of supporting notation. 

In
this paper we show how to re-use the very same do-notation to work
for Applicatives as well, providing efficiency benefits for some types
that are both Monad and Applicative, and syntactic convenience for
those that are merely Applicative. The result is fully implemented
as an optional extension in GHC, and is in use at Facebook to make
it easy to write highly-parallel queries in a distributed system.
