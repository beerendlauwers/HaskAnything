---
title: "FLTKHS - Easy native GUIs in Haskell, Today!"
url-video: https://www.youtube.com/watch?v=5hoQLovZBxQ
url-slides: https://github.com/deech/fltkhs-compose-conference-2016-talk/blob/master/Talk.org
authors:
 - Aditya Siram
source: http://www.composeconference.org/2016/speakers/
conference: Compose2016
tags:
libraries:
---

[FLTKHS](https://hackage.haskell.org/package/fltkhs) is a low-cost, hassle-free way to write cross-platform native GUI applications in Haskell. It is a quite complete binding to the [FLTK](http://fltk.org/) toolkit, a mature cross-platform C++ GUI library.

This talk will show how, compared to other offerings in the Haskell ecosystem, FLTKHS makes it easy to get up and running by:

- having minimal dependencies. You probably have most of them installed already.
- making it easy for non-Haskellers to learn the API by emulating the object oriented callback style most programmers are already familiar with.
- providing complete integration with the FLTK GUI builder. Dragging-and-dropping widgets, and wiring up callbacks in the builder generates Haskell code.
- compiling to zero-dependency statically linked binaries on all platforms.
- allowing creating custom widgets in pure Haskell

However it isn’t all roses. This talk will also cover:

- the trade-offs and pain points of writing a Haskell binding to a OO-heavy C++ library that relies heavily on polymorphic dispatch
- the downsides of trying to emulate that style in Haskell and things the author would have done differently
- the pro and cons vs a other object-oriented models in the Haskell ecosystem

In summary, the talk aims to show that FLTKHS is ready for use today and has a lot of advantages but will also be honest about mistakes made and why one might not want to use it.
