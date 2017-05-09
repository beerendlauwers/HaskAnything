---
title: "Revisiting Software Transactional Memory in Haskell"
url-video: "https://www.youtube.com/watch?v=c0E9iSYGNtU"
url-slides: "about:blank"
authors:
 - Matthew Le
 - Ryan Yates
 - Matthew Fluet
source: http://icfp16.sigplan.org/event/haskellsymp-2016-papers-revisiting-software-transactional-memory-in-haskell
tags:
 - Software Transactional Memory (STM)
libraries:
---

Software Transactional Memory (STM) has become very popular in Haskell. Currently, there are nearly 500 packages on Haskell's package archive that directly use STM\@. Despite the widespread use in real world applications, Haskell's STM implementation has seen very few updates since its introduction in 2005.

In this work, we describe our efforts to redesign the STM implementation in the Glasgow Haskell Compiler (GHC), based on a TL2-like implementation that is able to support both `orElse` and `retry` without the use of traditional nested transactions. We argue that our implementation is simpler than the current GHC implementation while supporting opacity. We also demonstrate that our implementation performs better than the current GHC implementation on a number of benchmarks by multiple orders of magnitude for long-running transactions.

In an effort to measure the performance of `orElse` and `retry`, we present an STM-based work stealing scheduler. With `orElse` and `retry`, we are able to elegantly implement the scheduler in just a few lines of code. We have modified the Par Monad, a real-world Haskell package that provides deterministic parallelism, to use our STM-based work stealing scheduler and show that it is not only simpler but is able to perform as well as the current scheduler.
