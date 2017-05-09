---
title: "High-Performance Client-Side Web Applications through Haskell EDSLs"
url-video: "https://www.youtube.com/watch?v=gQmDeq9eyF8"
url-slides: "about:blank"
authors:
 - Anton Ekblad
source: http://icfp16.sigplan.org/event/haskellsymp-2016-papers-high-performance-client-side-web-applications-through-haskell-edsls
tags:
 - DSL
 - Embedded DSL
 - Web Application
 - Benchmark
libraries:
---

We present Aplite, a domain-specific language embedded in Haskell for implementing performance-critical functions in client-side web applications. In Aplite, we apply partial evaluation, multi-stage programming and techniques adapted from machine code-targeting, high-performance EDSLs to the domain of web applications. We use Aplite to implement, among other benchmarks, procedural animation using Perlin noise, symmetrical encryption and K-means clustering, showing Aplite to be consistently faster than equivalent hand-written JavaScript – up to an order of magnitude for some benchmarks. We also demonstrate how Aplite's multi-staged nature can be used to automatically tune programs to the environment in which they are running, as well as to inputs representative of the programs' intended workload.

High-performance computation in the web browser is an attractive goal for many reasons: interactive simulations and games, cryptographic applications and reducing web companies' electricity bills by outsourcing expensive computations to users' web browsers. Similarly, functional programming in the browser is attractive due to its promises of simpler, shorter, safer programs. In this paper, we propose a way to combine the two.
