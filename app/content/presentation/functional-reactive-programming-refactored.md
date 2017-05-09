---
title: "Functional Reactive Programming, Refactored"
url-video: "https://www.youtube.com/watch?v=FmwOd4z9LdM"
url-slides: "about:blank"
authors:
 - Ivan Perez
 - Manuel Barenz
 - Henrik Nilsson
source: http://icfp16.sigplan.org/event/haskellsymp-2016-papers-functional-reactive-programming-refactored
tags:
 - Functional Reactive Programming (FRP)
 - Benchmark
libraries:
 - Yampa
---

Functional Reactive Programming (FRP) has come to mean many things. Yet, scratch the surface of the multitude of realisations, and there is great commonality between them. This paper investigates this commonality, turning it into a mathematically coherent and practical FRP realisation that allows us to express the functionality of many existing FRP systems and beyond by providing a minimal FRP core parameterised on a monad. We give proofs for our theoretical claims and we have verified the practical side by benchmarking a set of existing, non-trivial Yampa applications running on top of our new system with very good results.
