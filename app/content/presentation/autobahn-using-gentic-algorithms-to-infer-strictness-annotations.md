---
title: "Autobahn: Using Genetic Algorithms to Infer Strictness Annotations"
url-video: "https://www.youtube.com/watch?v=wUmKI6IVEpM"
url-slides: "about:blank"
authors:
 - Yisu Remy Wang
 - Diogenes Nunez
 - Kathleen Fisher
source: http://icfp16.sigplan.org/event/haskellsymp-2016-papers-autobahn-using-genetic-algorithms-to-infer-strictness-annotations
tags:
 - Testing
 - Inference
 - Performance
libraries:
---

Although laziness enables beautiful code, it comes with non-trivial performance costs. The ghc compiler for Haskell has optimizations to reduce those costs, but the optimizations are not sufficient. As a result, Haskell also provides a variety of strictness annotations so that users can indicate program points where an expression should be evaluated eagerly. Skillful use of those annotations is a black art, known only to expert Haskell programmers. In this paper, we introduce AUTOBAHN, a tool that uses genetic algorithms to automatically infer strictness annotations that improve program performance on representative inputs. Users examine the suggested annotations for soundness and can instruct AUTOBAHN to automatically produce modified sources. Experiments on 60 programs from the NoFib benchmark suite show that AUTOBAHN can infer annotation sets that improve runtime performance by a geometric mean of 8.5%. Case studies show AUTOBAHN can reduce the live size of a GC simulator by 99% and infer application-specific annotations for Aeson library code. A 10-fold cross-validation study shows the AUTOBAHN -optimized GC simulator generally outperforms a version optimized by an expert.
